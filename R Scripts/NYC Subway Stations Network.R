# R Script Testing for nyc subway Stations
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")

nyc_adjacency = read.csv('Data Processing/Data/Master Sets/NYC Master/NYC_Subway_Adjacency_Matrix.csv', header = TRUE, row.names = 1)
colnames(nyc_adjacency) = gsub("X", "", colnames(nyc_adjacency))
nyc_adjacency
station_network_list = c(names(nyc_adjacency))
station_network_list

frequency = 'daily'
filepath = paste('Data Processing/Data/Master Sets/NYC Master/NYC_Time_Series_', frequency, '.csv', sep="")
nyc_ts = read.csv(filepath, header = TRUE, row.names = 1)
colnames(nyc_ts) = gsub("X", "", colnames(nyc_ts))
nyc_ts = nyc_ts %>% select(any_of(station_network_list))
nyc_ts
nyc_ts = ts(nyc_ts)
class(nyc_ts)

station_network_labels = colnames(nyc_ts)
nyc_adjacency = nyc_adjacency[station_network_labels, station_network_labels]
nyc_net = matrixtoGNAR(nyc_adjacency)
nyc_net

plot(nyc_net, vertex.label = station_network_labels, arrow.mode = '-', directed = FALSE)

alphaOrder = 2
betaOrder = c(1,1)

answer = GNARfit(vts = nyc_ts, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder)

forecast_steps = 30
nyc_stn_focus = 1

nyc_ts

coef(answer)

ts_shifted = c(nyc_ts[, nyc_stn_focus], rep(NA, forecast_steps))
plot(nyc_ts[, nyc_stn_focus], ylab = "Ridership Volume")
plot(ts_shifted, ylab = "Ridership Volume", type = "l")

fitted_ans = fitted(answer)
fitted_shifted = c(rep(NA, alphaOrder-1), fitted_ans[, nyc_stn_focus])
lines(fitted_shifted, col = 2)

pred = predict(answer, n.ahead = forecast_steps)
colnames(pred) = station_network_labels
pred

pred_shifted = c(rep(NA, nrow(nyc_ts)), pred[, nyc_stn_focus])
lines(pred_shifted, col = 3)

