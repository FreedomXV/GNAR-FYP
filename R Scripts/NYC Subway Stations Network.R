# R Script Testing for NYC subway Stations
# Maybe Rewrite this in a cleaner way such that I remember whats going
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")
library("forecast")

frequency = 'daily'
alphaOrder = 365
betaRep = 3
betaOrder = c(rep(betaRep, alphaOrder))
# forecast_steps = 12
# nyc_stn_focus = 1

nyc_adjacency = read.csv('Data Processing/Data/Master Sets/NYC Master/NYC_Subway_Adjacency_Matrix.csv', header = TRUE, row.names = 1)
colnames(nyc_adjacency) = gsub("X", "", colnames(nyc_adjacency))
nyc_adjacency
station_network_list = c(names(nyc_adjacency))
station_network_list

filepath = paste('Data Processing/Data/Master Sets/NYC Master/NYC_Time_Series_', frequency, '_ridership.csv', sep="")
nyc_ts = read.csv(filepath, header = TRUE, row.names = 1)
colnames(nyc_ts) = gsub("X", "", colnames(nyc_ts))
nyc_ts = nyc_ts %>% select(any_of(station_network_list))
time_intervals = length(rownames(nyc_ts))
nyc_ts
nyc_ts = ts(nyc_ts)
class(nyc_ts)

station_network_labels = colnames(nyc_ts)
nyc_adjacency = nyc_adjacency[station_network_labels, station_network_labels]
nyc_net = matrixtoGNAR(nyc_adjacency)
nyc_net

# plot(nyc_net, vertex.label = station_network_labels, arrow.mode = '-', directed = FALSE)
ts.plot(rowMeans(nyc_ts))
nyc_ts_diff = diff(nyc_ts, lag = 1)
ts.plot(rowMeans(nyc_ts_diff))

## reduce number of stations / number of lines / alter to line usage and edge weights based on number of interchanges between them

answer = GNARfit(vts = nyc_ts_diff, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder)
summary(answer)

nyc_ts

save_dir = paste('R Scripts/GNAR Predictions/', frequency, '_alpha', alphaOrder, '_beta', betaRep, '_forecast', forecast_steps, '/', sep="")
dir.create(file.path(save_dir))
coef(answer)
coefout = paste(save_dir, 'coefficients_', frequency, '_alpha', alphaOrder, '_forecast', forecast_steps, '.csv', sep="")
write.csv(coef(answer), coefout)

ts_shifted = c(nyc_ts[, nyc_stn_focus], rep(NA, forecast_steps))
plot(nyc_ts[, nyc_stn_focus], ylab = "Ridership Volume", xlab = "Timestamp")
plotout1 = paste(save_dir, 'timeplot_', frequency, '_alpha', alphaOrder, '_forecast', forecast_steps, '.png', sep="")
dev.copy(png, filename=plotout1, width = 1920, height = 1080)
dev.off()

plot(ts_shifted, ylab = "Ridership Volume", xlab = "Timestamp", type = "l")

fitted_ans = fitted(answer)
fitted_shifted = c(rep(NA, alphaOrder), fitted_ans[, nyc_stn_focus])
lines(fitted_shifted, col = 2)

pred = predict(answer, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts)), pred[, nyc_stn_focus])
lines(pred_shifted, col = 3)
plotout2 = paste(save_dir, 'predictionplot_', frequency, '_alpha', alphaOrder, '_forecast', forecast_steps, '.png', sep="")
dev.copy(png, filename=plotout2, width = 1920, height = 1080)
dev.off()

fileout = paste(save_dir, 'predictions_', frequency, '_alpha', alphaOrder, '_forecast', forecast_steps, '.csv', sep="")
write.csv(pred, fileout)
