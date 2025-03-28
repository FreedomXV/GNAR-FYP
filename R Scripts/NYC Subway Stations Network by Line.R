# R Script Testing for GNAR Models and observing prediction error
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")
library("forecast")

frequency = 'daily'
alphaOrder = 14
betaRep = 2
betaOrder = c(rep(betaRep, alphaOrder))

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

station_network_labels = colnames(nyc_ts)
nyc_adjacency = nyc_adjacency[station_network_labels, station_network_labels]
nyc_net = matrixtoGNAR(nyc_adjacency)
nyc_net

answer = GNARfit(vts = nyc_ts, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder)
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
  betaArr[b1 + 1] <- BIC(GNARfit(vts = nyc_ts, net = nyc_net, alphaOrder = alphaOrder, betaOrder = c(rep(b1, alphaOrder))))
}

plot(betaArr)

# scaling alphaOrder from 1 to 14 and betaRep from 0 to 4

BICmat <- matrix(0, ncol = 5, nrow = 14)

maxAlpha = 14
maxBeta = 4

for (beta in 0:(maxBeta)){
  for (alpha in 1:(maxAlpha)){
    print(alpha)
    print(beta)
    print('calculating BIC')
    BICmat[alpha, beta + 1] <- BIC(GNARfit(vts = nyc_ts, net = nyc_net, alphaOrder = alpha, betaOrder = c(rep(beta, alpha))))
  }
}

contour(1:14, 0:4, log(abs(floor(min(BICmat))) + BICmat), xlab = "alphaOrder", ylab = "betaOrder")

# best reasonable model determined to be alpha = 11, betaRep = 0 (lower betaRep is better)

alphaOrder = 11
betaRep = 0
betaOrder = c(rep(betaRep, alphaOrder))

# now we compare error of each model
# sample random betamodels for alphaOrder = 11
# test 50 random models, without weights

betaSamples = matrix(0, ncol = 11, nrow = 50)
BICSamples = c()

for (model in 1:50){
  betaOrder = sample(0:4, alphaOrder, replace = TRUE)
  print(betaOrder)
  BICSamples[model] = BIC(GNARfit(vts = nyc_ts, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder))
  betaSamples[model,] = betaOrder
}

plot(BICSamples)

dim(nyc_ts)

# Checking pred vs actual
# Regress on [1:1280,] error checking on [1281,]

error_plot = data.frame(
  model_no = c(1:50),
  pred_error = c(1:50),
  BIC = c(1:50)
)


for (model_no in 1:50){
  error_plot['model_no'][model_no,] = model_no
  modelFit = GNARfit(vts = nyc_ts[1:1280,], net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaSamples[model_no,])
  error_plot['BIC'][model_no,] = BIC(modelFit)
  print(BIC(modelFit))
  predictions = predict(modelFit, n.ahead = 364)
  pred_error = sum((predictions - nyc_ts[1281:1644,])^2)
  print(pred_error)
  error_plot['pred_error'][model_no,] = pred_error
}

boxplot(pred_error ~ model_no, data = error_plot)

# data_frame[which.min(data_frame$C4),]
min_error = which.min(error_plot$pred_error)
min_BIC = which.min(error_plot$BIC)

norminv = function(ts){
  ts = (sd(nyc_ts_diff)*ts) + mean(nyc_ts_diff)
  return(ts)
}

forecast_steps = 365
nyc_line_focus = 1

plot(nyc_ts[, nyc_line_focus], ylab = "Ridership Volume", xlab = "Timestamp")

min_error_model = GNARfit(vts = nyc_ts[1:(length(nyc_ts[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = 11, betaOrder = betaSamples[min_error, ])

# ts_shifted = c(nyc_ts[, nyc_line_focus], rep(NA, forecast_steps))

plot(c(nyc_ts[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")
fitted_ans = fitted(min_error_model)
lines(fitted_ans[, nyc_line_focus], col = 2)

pred = predict(min_error_model, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts[1:(length(nyc_ts[, 1]) - forecast_steps),])), pred[, nyc_stn_focus])
lines(pred_shifted, col = 3)

min_BIC_model = GNARfit(vts = nyc_ts[1:(length(nyc_ts[, 1]) - forecast_steps),], net = nyc_net, alphaOrder = 11, betaOrder = betaSamples[min_BIC, ])

# ts_shifted = c(nyc_ts[, nyc_line_focus], rep(NA, forecast_steps))

plot(c(nyc_ts[, nyc_line_focus]), ylab = "Ridership Volume", xlab = "Timestamp", type = "l")
fitted_ans = fitted(min_BIC_model)
lines(fitted_ans[, nyc_line_focus], col = 2)

pred = predict(min_BIC_model, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts[1:(length(nyc_ts[, 1]) - forecast_steps),])), pred[, nyc_stn_focus])
lines(pred_shifted, col = 3)