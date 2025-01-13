# R Script Testing for MRT Stations
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")

fiveNet
as.matrix(fiveNet)
summary(fiveVTS)

mrt_adjacency = read.csv('Data Processing/Data/Master Sets/TE_Line_Adjacency_Matrix_Extended_Full.csv', header = TRUE, row.names = 1)
mrt_adjacency
station_network_list = c(names(mrt_adjacency))
# station_network_list = c('TE15', 'TE16', 'TE18', 'TE19')
station_network_list
mrt_net = matrixtoGNAR(mrt_adjacency)
mrt_net
plot(mrt_net, vertex.label = station_network_list, arrow.mode = '-', directed = FALSE)
# https://igraph.org/r/doc/plot.igraph.html
# https://www.rdocumentation.org/packages/igraph/versions/0.2.1/topics/plot.igraph

mrt_tapin_ts = read.csv('Data Processing/Data/Master Sets/StationTimeSeries_TapInVolume_Full.csv', header = TRUE, row.names = 1)
mrt_tapin_ts = mrt_tapin_ts[station_network_list]
mrt_tapin_ts
mrt_tapin_ts = ts(mrt_tapin_ts)
class(mrt_tapin_ts)

alphaOrder = 2
betaOrder = c(1,1)

answer <- GNARfit(vts = mrt_tapin_ts, net = mrt_net, alphaOrder = alphaOrder, betaOrder =  betaOrder)
answer
#alphaOrder and betaOrder ??

forecast_steps = 6
mrt_stn_focus = 17

coef(answer)
ts_shifted <- c(mrt_tapin_ts[, mrt_stn_focus], rep(NA, forecast_steps))
plot(mrt_tapin_ts[, mrt_stn_focus], ylab = "Node A Time Series")
plot(ts_shifted, ylab = "Node A Time Series", type = "l")

fitted_shifted <- c(rep(NA, alphaOrder-1), fitted(answer)[, mrt_stn_focus])
lines(fitted_shifted, col = 2)
# fitted lines only got to t=4
# returns t - alphaOrder values (6-2 =)

pred = predict(answer, n.ahead = forecast_steps)
colnames(pred) = station_network_list
pred
pred_shifted <- c(rep(NA, nrow(mrt_tapin_ts)), pred[, mrt_stn_focus])
lines(pred_shifted, col = 3)

write.csv(pred, 'predictions.csv')

# for (i in 1:length(station_network_list)){
#   print(station_network_list[i])
#   print(pred[,i])
# }
