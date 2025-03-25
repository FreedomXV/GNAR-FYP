# R Script Testing for GNAR Models and observing prediction error (time shifted)
# Using the GNARX model for enhanced forecasting
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")
library("forecast")

frequency = 'daily'
alphaOrder = 12
betaRep = 2
betaOrder = c(rep(betaRep, alphaOrder))

forecast_steps = 1
time_lag = 7
diff_order = 4
if (frequency == 'monthly'){
  forecast_steps = 12
  time_lag = 1
  diff_order = 1
} else if (frequency == 'daily'){
  forecast_steps = 365
  time_lag = 28
  diff_order = 1
}
nyc_line_focus = 1

nyc_adjacency = read.csv('Data Processing/Data/Master Sets/NYC Master/NYC_Subway_Adjacency_Matrix_by_Line.csv', header = TRUE, row.names = 1)
colnames(nyc_adjacency) = gsub("X", "", colnames(nyc_adjacency))
# colnames(nyc_adjacency) = gsub("\\.\\.\\.", " - ", colnames(nyc_adjacency))
# colnames(nyc_adjacency) = gsub("\\.", " ", colnames(nyc_adjacency))
rownames(nyc_adjacency) = colnames(nyc_adjacency)
nyc_adjacency
station_network_list = c(names(nyc_adjacency))
station_network_list

filepath = paste('Data Processing/Data/Master Sets/NYC Master/NYC_Time_Series_', frequency, '_ridership_by_Line.csv', sep="")
nyc_ts = read.csv(filepath, header = TRUE, row.names = 1)
colnames(nyc_ts) = gsub("X", "", colnames(nyc_ts))
# colnames(nyc_ts) = gsub("\\.\\.\\.", " - ", colnames(nyc_ts))
# colnames(nyc_ts) = gsub("\\.", " ", colnames(nyc_ts))
nyc_ts = nyc_ts %>% select(any_of(station_network_list))
time_intervals = length(rownames(nyc_ts))
nyc_ts
nyc_ts = ts(nyc_ts)
class(nyc_ts)

for (i in 1:length(colnames(nyc_ts))){
  print(i)
  print(tsoutliers(nyc_ts[,i]))
  nyc_ts[,i] = tsclean(nyc_ts[,i])
}

ts.plot(rowMeans(nyc_ts))
nyc_ts_diff = diff(nyc_ts, lag = time_lag, differences = diff_order)
ts.plot(rowMeans(nyc_ts_diff))
nyc_ts_diff_norm = (nyc_ts_diff - mean(nyc_ts_diff)) / sd(nyc_ts_diff)

library(tseries)
# Perform the Augmented Dickey-Fuller Test
adf_result <- adf.test(rowMeans(nyc_ts_diff_norm), k = time_lag)
adf.test(rowMeans(nyc_ts_diff_norm), k = time_lag)

# Stationary under the assumption
# ** All stations share the same time series model, with different coefficients, i.e applying the same differencing operator will stationarise the whole thing

# Adding supplementary vector time series containing POI data
station_network_labels = colnames(nyc_ts)
nyc_adjacency = nyc_adjacency[station_network_labels, station_network_labels]
nyc_net = matrixtoGNAR(nyc_adjacency)
nyc_net

date_index = 'created'
filepath_poi = paste('Data Processing/Data/Master Sets/NYC Master/NYC_Subway_POI_', frequency, '_vts_', date_index, '_by_Line.csv', sep="")
poi_vts = read.csv(filepath_poi, header = TRUE, row.names = 1)
colnames(poi_vts) = gsub("X", "", colnames(poi_vts))
# colnames(poi_vts) = gsub("\\.\\.\\.", " - ", colnames(poi_vts))
# colnames(poi_vts) = gsub("\\.", " ", colnames(poi_vts))
poi_vts = poi_vts %>% select(any_of(station_network_labels))
poi_vts = ts(poi_vts)
class(poi_vts)

for (i in 1:length(colnames(poi_vts))){
  print(i)
  print(tsoutliers(poi_vts[,i]))
  poi_vts[,i] = tsclean(poi_vts[,i])
}

ts.plot(rowMeans(poi_vts))
poi_vts_diff = diff(poi_vts, differences = 3)
ts.plot(rowMeans(poi_vts_diff))
poi_vts_diff_norm = (poi_vts_diff - mean(poi_vts_diff)) / sd(poi_vts_diff)

zero_matrix = matrix(0, nrow = (time_lag - 3), ncol = ncol(poi_vts_diff_norm))
nyc_ts_diff_norm = rbind(zero_matrix, nyc_ts_diff_norm)

# Perform the Augmented Dickey-Fuller Test
adf_result_poi <- adf.test(rowMeans(poi_vts_diff_norm))
print(adf_result_poi)

answer = GNARXfit(vts = nyc_ts_diff_norm, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder, xvts = list(poi_vts_diff_norm), lambdaOrder = c(3))
summary(answer)

calculate_rss = function(model){
  residuals = residuals(model)
  rss = sum(residuals^2)
  df = length(residuals) - length(coef(model))
  return (sqrt(rss/df))
}

# Comparing model BICs based on repeating betaOrder
maxLag = 4
betaArr = c()
for(b1 in 0:maxLag){
  print(b1)
  print('calculating BIC')
  betaArr[b1 + 1] <- BIC(GNARXfit(vts = nyc_ts_diff_norm, net = nyc_net, alphaOrder = alphaOrder, betaOrder = c(rep(b1, alphaOrder)), xvts = list(poi_vts_diff_norm), lambdaOrder = c(3)))
}

plot(betaArr)

# scaling alphaOrder from 1 to 14 and betaRep from 0 to 4

maxAlpha = as.numeric(adf_result$parameter)
maxBeta = maxLag

BICmat <- matrix(0, ncol = maxBeta+1, nrow = maxAlpha)

for (beta in 0:(maxBeta)){
  for (alpha in 1:(maxAlpha)){
    print(alpha)
    print(beta)
    print('calculating BIC')
    BICmat[alpha, beta + 1] <- BIC(GNARXfit(vts = nyc_ts_diff_norm, net = nyc_net, alphaOrder = alpha, betaOrder = c(rep(beta, alpha)), xvts = list(poi_vts_diff_norm), lambdaOrder = c(3)))
  }
}

save_dir = paste('R Scripts/GNAR Predictions/', frequency, '_GNARX', '_timeLag', time_lag, '_alpha', alphaOrder, '_beta', betaRep, '_forecast', forecast_steps, '_480 by 320', '/', sep="")
dir.create(file.path(save_dir))

contour(1:maxAlpha, 0:maxBeta, log(abs(floor(min(BICmat))) + BICmat), xlab = "alphaOrder", ylab = "betaOrder")

plotout = paste(save_dir, 'countour_plot', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

# best reasonable model determined to be alpha = 11, betaRep = 0 (lower betaRep is better)

alphaOrder = as.numeric(adf_result$parameter)
betaRep = 1
betaOrder = c(rep(betaRep, alphaOrder))

# now we compare error of each model
# sample random betamodels for alphaOrder = 11
# test 50 random models, without weights

sample_models = 50

if (frequency == 'monthly'){
  sample_models = (maxLag+1)**alphaOrder
} else if (frequency == 'daily'){
  sample_models = 100
}

betaSamples = matrix(0, ncol = alphaOrder, nrow = sample_models)
BICSamples = c()

if (frequency == 'monthly'){
  
  beta_combs = expand.grid(rep(list(0:maxLag), alphaOrder))
  
  for (model in 1:sample_models){
    betaOrder = c(unlist(beta_combs[model,]))
    print(betaOrder)
    BICSamples[model] = BIC(GNARXfit(vts = nyc_ts_diff_norm, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder, xvts = list(poi_vts_diff_norm), lambdaOrder = c(3)))
    betaSamples[model,] = betaOrder
  }
  
} else if (frequency == 'daily'){
  
  for (model in 1:sample_models){
    betaOrder = sample(0:maxLag, alphaOrder, replace = TRUE)
    print(betaOrder)
    BICSamples[model] = BIC(GNARXfit(vts = nyc_ts_diff_norm, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder, xvts = list(poi_vts_diff_norm), lambdaOrder = c(3)))
    betaSamples[model,] = betaOrder
  }
  
}

plot(BICSamples)

coefout = paste(save_dir, 'beta_samples', '.csv', sep="")
write.csv(betaSamples, coefout)

coefout = paste(save_dir, 'BIC_samples', '.csv', sep="")
write.csv(BICSamples, coefout)

plotout = paste(save_dir, 'BIC_samples', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

dim(nyc_ts)

# Checking pred vs actual
# Regress on [1:1280,] error checking on [1281,]

error_plot = data.frame(
  model_no = c(1:sample_models),
  pred_error = c(1:sample_models),
  BIC = c(1:sample_models)
)

calculate_mspe = function(model, actual_ts,forecast_steps){
  predictions = predict(model, n.ahead = forecast_steps)
  actual = actual_ts[(length(actual_ts[, 1]) - forecast_steps + 1):(length(actual_ts[, 1])),]
  
  pred_errors = actual - predictions
  ME = mean(pred_errors)
  VAR = sd(pred_errors)^2
  return (ME^2 + VAR)
}

for (model_no in 1:sample_models){
  error_plot['model_no'][model_no,] = model_no
  modelFit = GNARfit(vts = nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaSamples[model_no,])
  error_plot['BIC'][model_no,] = BIC(modelFit)
  print('BIC')
  print(BIC(modelFit))
  
  pred_error = calculate_mspe(modelFit, nyc_ts_diff_norm, forecast_steps)
  
  print('error')
  print(pred_error)
  error_plot['pred_error'][model_no,] = pred_error
  print('')
}

boxplot(pred_error ~ model_no, data = error_plot)

coefout = paste(save_dir, 'error_plots', '.csv', sep="")
write.csv(error_plot, coefout)

plotout = paste(save_dir, 'pred_error_plot', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

# data_frame[which.min(data_frame$C4),]
min_error = which.min(error_plot$pred_error)
min_BIC = which.min(error_plot$BIC)

norminv = function(ts){
  ts = (sd(nyc_ts_diff)*ts) + mean(nyc_ts_diff)
  return(ts)
}

# plot based on the minimum BIC

inv_nyc_ts = diffinv(nyc_ts_diff, lag = time_lag, differences = diff_order, xi = matrix(nyc_ts[1:(time_lag*diff_order),], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)))
ts.plot(rowMeans(inv_nyc_ts))
plot(nyc_ts[, nyc_line_focus], ylab = "Ridership Volume", xlab = "Timestamp")
ts_corrected = diffinv(norminv(nyc_ts_diff_norm), xi = matrix(nyc_ts[1:(time_lag*diff_order),], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
plot(ts_corrected[, nyc_line_focus], ylab = "Ridership Volume", xlab = "Timestamp")

min_BIC_model = GNARXfit(vts = nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaSamples[min_BIC, ], xvts = list(poi_vts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),]), lambdaOrder = c(3))

# ts_shifted = c(nyc_ts_diff_norm[, nyc_line_focus], rep(NA, forecast_steps))

plot(c(nyc_ts_diff_norm[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")
fitted_ans = fitted(min_BIC_model)
fitted_shifted = c(rep(NA, alphaOrder), fitted_ans[, nyc_line_focus])
lines(fitted_shifted, col = 2)

pred = predict(min_BIC_model, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),])), pred[, nyc_line_focus])
lines(pred_shifted, col = 3)

plotout = paste(save_dir, 'timeplot_', 'min_BIC_model_diff', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

plot(c(nyc_ts[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")

inv_fitted = diffinv(norminv(fitted_ans), xi = matrix(nyc_ts[alphaOrder:(alphaOrder - 1 + (time_lag*diff_order)),], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_fitted_shifted = c(rep(NA, alphaOrder), inv_fitted[, nyc_line_focus])
lines(inv_fitted_shifted, col = 2)

inv_pred = diffinv(norminv(pred), xi = matrix(nyc_ts[(length(nyc_ts[, 1]) - forecast_steps):(length(nyc_ts[, 1]) - forecast_steps - 1 + (time_lag*diff_order)), ], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts[, 1]) - forecast_steps),])), inv_pred[, nyc_line_focus])
lines(inv_pred_shifted, col = 3)
calculate_mspe(min_BIC_model, nyc_ts_diff_norm, forecast_steps)

plotout = paste(save_dir, 'timeplot_', 'min_BIC_model_inv', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

coef(min_BIC_model)
coefout = paste(save_dir, 'coefficients_', 'min_BIC_model', '.csv', sep="")
write.csv(coef(min_BIC_model), coefout)

meta_beta_vector = c(betaSamples[min_BIC, ])
meta_BIC = BIC(min_BIC_model)
meta_mspe = calculate_mspe(min_BIC_model, nyc_ts_diff_norm, forecast_steps)
meta_rss = calculate_rss(min_BIC_model)
meta_RSquared = 'TODO'

meta_summary = summary(min_BIC_model)
sum_out = paste(save_dir, 'meta_summary_min_BIC_model.txt')
sink(sum_out)
print(summary(min_BIC_model))
sink()

max_length = max(c(length(betaOrder), length(meta_BIC), length(meta_mspe), length(meta_rss), length(meta_RSquared), length(meta_summary)))

metadata = data.frame(
  betaOrder = c(meta_beta_vector,rep(NA, max_length - length(meta_beta_vector))),
  BIC = c(meta_BIC,rep(NA, max_length - length(meta_BIC))),
  mspe = c(meta_mspe,rep(NA, max_length - length(meta_mspe))),
  rss = c(meta_rss,rep(NA, max_length - length(meta_rss))),
  RSquared = c(meta_RSquared,rep(NA, max_length - length(meta_RSquared))),
  summary = c(meta_summary,rep(NA, max_length - length(meta_summary)))
)

coefout = paste(save_dir, 'metadata_', 'min_BIC_model', '.csv', sep="")
write.csv(metadata, coefout)

# plot based on the minimum prediction error
plot(nyc_ts[, nyc_line_focus], ylab = "Ridership Volume", xlab = "Timestamp")

min_error_model = GNARXfit(vts = nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaSamples[min_error, ], xvts = list(poi_vts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),]), lambdaOrder = c(3))

# ts_shifted = c(nyc_ts_diff_norm[, nyc_line_focus], rep(NA, forecast_steps))

plot(c(nyc_ts_diff_norm[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")
fitted_ans = fitted(min_error_model)
lines(fitted_ans[, nyc_line_focus], col = 2)

pred = predict(min_error_model, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),])), pred[, nyc_line_focus])
lines(pred_shifted, col = 3)

plotout = paste(save_dir, 'timeplot_', 'min_error_model_diff', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

plot(c(nyc_ts[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")

inv_fitted = diffinv(norminv(fitted_ans), xi = matrix(nyc_ts[alphaOrder:(alphaOrder - 1 + (time_lag*diff_order)),], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_fitted_shifted = c(rep(NA, alphaOrder), inv_fitted[, nyc_line_focus])
lines(inv_fitted_shifted, col = 2)

inv_pred = diffinv(norminv(pred), xi = matrix(nyc_ts[(length(nyc_ts[, 1]) - forecast_steps):(length(nyc_ts[, 1]) - forecast_steps - 1 + (time_lag*diff_order)), ], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts[, 1]) - forecast_steps),])), inv_pred[, nyc_line_focus])
lines(inv_pred_shifted, col = 3)
calculate_mspe(min_error_model, nyc_ts_diff_norm, forecast_steps)

plotout = paste(save_dir, 'timeplot_', 'min_error_model_inv', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

coef(min_error_model)
coefout = paste(save_dir, 'coefficients_', 'min_error_model', '.csv', sep="")
write.csv(coef(min_error_model), coefout)

meta_beta_vector = c(betaSamples[min_error, ])
meta_BIC = BIC(min_error_model)
meta_mspe = calculate_mspe(min_error_model, nyc_ts_diff_norm, forecast_steps)
meta_rss = calculate_rss(min_error_model)
meta_RSquared = 'TODO'

meta_summary = summary(min_error_model)
sum_out = paste(save_dir, 'meta_summary_min_error_model.txt')
sink(sum_out)
print(summary(min_error_model))
sink()

max_length = max(c(length(betaOrder), length(meta_BIC), length(meta_mspe), length(meta_rss), length(meta_RSquared), length(meta_summary)))

metadata = data.frame(
  betaOrder = c(meta_beta_vector,rep(NA, max_length - length(meta_beta_vector))),
  BIC = c(meta_BIC,rep(NA, max_length - length(meta_BIC))),
  mspe = c(meta_mspe,rep(NA, max_length - length(meta_mspe))),
  rss = c(meta_rss,rep(NA, max_length - length(meta_rss))),
  RSquared = c(meta_RSquared,rep(NA, max_length - length(meta_RSquared))),
  summary = c(meta_summary,rep(NA, max_length - length(meta_summary)))
)

coefout = paste(save_dir, 'metadata_', 'min_error_model', '.csv', sep="")
write.csv(metadata, coefout)

# plot based on beta 0 model
betaRep = 0
betaOrder = c(rep(betaRep, alphaOrder))
plot(nyc_ts[, nyc_line_focus], ylab = "Ridership Volume", xlab = "Timestamp")

beta0_model = GNARXfit(vts = nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder, xvts = list(poi_vts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),]), lambdaOrder = c(3))

# ts_shifted = c(nyc_ts_diff_norm[, nyc_line_focus], rep(NA, forecast_steps))

plot(c(nyc_ts_diff_norm[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")
fitted_ans = fitted(beta0_model)
lines(fitted_ans[, nyc_line_focus], col = 2)

pred = predict(beta0_model, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),])), pred[, nyc_line_focus])
lines(pred_shifted, col = 3)

plotout = paste(save_dir, 'timeplot_', 'beta0_model_diff', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

plot(c(nyc_ts[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")

inv_fitted = diffinv(norminv(fitted_ans), xi = matrix(nyc_ts[alphaOrder:(alphaOrder - 1 + (time_lag*diff_order)),], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_fitted_shifted = c(rep(NA, alphaOrder), inv_fitted[, nyc_line_focus])
lines(inv_fitted_shifted, col = 2)

inv_pred = diffinv(norminv(pred), xi = matrix(nyc_ts[(length(nyc_ts[, 1]) - forecast_steps):(length(nyc_ts[, 1]) - forecast_steps - 1 + (time_lag*diff_order)), ], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts[, 1]) - forecast_steps),])), inv_pred[, nyc_line_focus])
lines(inv_pred_shifted, col = 3)
calculate_mspe(beta0_model, nyc_ts_diff_norm, forecast_steps)

plotout = paste(save_dir, 'timeplot_', 'beta0_model_inv', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

coef(beta0_model)
coefout = paste(save_dir, 'coefficients_', 'beta0_model', '.csv', sep="")
write.csv(coef(beta0_model), coefout)

meta_beta_vector = betaOrder
meta_BIC = BIC(beta0_model)
meta_mspe = calculate_mspe(beta0_model, nyc_ts_diff_norm, forecast_steps)
meta_rss = calculate_rss(beta0_model)
meta_RSquared = 'TODO'

meta_summary = summary(beta0_model)
sum_out = paste(save_dir, 'meta_summary_beta0_model.txt')
sink(sum_out)
print(summary(beta0_model))
sink()

max_length = max(c(length(betaOrder), length(meta_BIC), length(meta_mspe), length(meta_rss), length(meta_RSquared), length(meta_summary)))

metadata = data.frame(
  betaOrder = c(meta_beta_vector,rep(NA, max_length - length(meta_beta_vector))),
  BIC = c(meta_BIC,rep(NA, max_length - length(meta_BIC))),
  mspe = c(meta_mspe,rep(NA, max_length - length(meta_mspe))),
  rss = c(meta_rss,rep(NA, max_length - length(meta_rss))),
  RSquared = c(meta_RSquared,rep(NA, max_length - length(meta_RSquared))),
  summary = c(meta_summary,rep(NA, max_length - length(meta_summary)))
)

coefout = paste(save_dir, 'metadata_', 'beta0_model', '.csv', sep="")
write.csv(metadata, coefout)

# plot based on beta 1 model
betaRep = 1
betaOrder = c(rep(betaRep, alphaOrder))
plot(nyc_ts[, nyc_line_focus], ylab = "Ridership Volume", xlab = "Timestamp")

beta1_model = GNARXfit(vts = nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder, xvts = list(poi_vts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),]), lambdaOrder = c(3))

# ts_shifted = c(nyc_ts_diff_norm[, nyc_line_focus], rep(NA, forecast_steps))

plot(c(nyc_ts_diff_norm[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")
fitted_ans = fitted(beta1_model)
lines(fitted_ans[, nyc_line_focus], col = 2)

pred = predict(beta1_model, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts_diff_norm[, 1]) - forecast_steps),])), pred[, nyc_line_focus])
lines(pred_shifted, col = 3)

plotout = paste(save_dir, 'timeplot_', 'beta1_model_diff', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

plot(c(nyc_ts[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")

inv_fitted = diffinv(norminv(fitted_ans), xi = matrix(nyc_ts[alphaOrder:(alphaOrder - 1 + (time_lag*diff_order)),], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_fitted_shifted = c(rep(NA, alphaOrder), inv_fitted[, nyc_line_focus])
lines(inv_fitted_shifted, col = 2)

inv_pred = diffinv(norminv(pred), xi = matrix(nyc_ts[(length(nyc_ts[, 1]) - forecast_steps):(length(nyc_ts[, 1]) - forecast_steps - 1 + (time_lag*diff_order)), ], ncol = ncol(nyc_ts), nrow = (time_lag*diff_order)), lag = time_lag, differences = diff_order)
inv_pred_shifted = c(rep(NA, nrow(nyc_ts_diff_norm[1:(length(nyc_ts[, 1]) - forecast_steps),])), inv_pred[, nyc_line_focus])
lines(inv_pred_shifted, col = 3)
calculate_mspe(beta1_model, nyc_ts_diff_norm, forecast_steps)

plotout = paste(save_dir, 'timeplot_', 'beta1_model_inv', '.png', sep="")
dev.copy(png, filename=plotout, width = 480, height = 320)
dev.off()

coef(beta1_model)
coefout = paste(save_dir, 'coefficients_', 'beta1_model', '.csv', sep="")
write.csv(coef(beta1_model), coefout)

meta_beta_vector = betaOrder
meta_BIC = BIC(beta1_model)
meta_mspe = calculate_mspe(beta1_model, nyc_ts_diff_norm, forecast_steps)
meta_rss = calculate_rss(beta1_model)
meta_RSquared = 'TODO'

meta_summary = summary(beta1_model)
sum_out = paste(save_dir, 'meta_summary_beta1_model.txt')
sink(sum_out)
print(summary(beta1_model))
sink()

max_length = max(c(length(betaOrder), length(meta_BIC), length(meta_mspe), length(meta_rss), length(meta_RSquared), length(meta_summary)))

metadata = data.frame(
  betaOrder = c(meta_beta_vector,rep(NA, max_length - length(meta_beta_vector))),
  BIC = c(meta_BIC,rep(NA, max_length - length(meta_BIC))),
  mspe = c(meta_mspe,rep(NA, max_length - length(meta_mspe))),
  rss = c(meta_rss,rep(NA, max_length - length(meta_rss))),
  RSquared = c(meta_RSquared,rep(NA, max_length - length(meta_RSquared))),
  summary = c(meta_summary,rep(NA, max_length - length(meta_summary)))
)

coefout = paste(save_dir, 'metadata_', 'beta1_model', '.csv', sep="")
write.csv(metadata, coefout)

