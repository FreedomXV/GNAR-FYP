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
fiveNet
plot(mrt_net, vertex.label = station_network_list, arrow.mode = '-', directed = FALSE)
# https://igraph.org/r/doc/plot.igraph.html
# https://www.rdocumentation.org/packages/igraph/versions/0.2.1/topics/plot.igraph

mrt_tapin_ts = read.csv('Data Processing/Data/Master Sets/StationTimeSeries_TapInVolume_Full.csv', header = TRUE, row.names = 1)
mrt_tapin_ts = mrt_tapin_ts[station_network_list]
# for (i in 1:length(station_network_list)){
#   print(station_network_list[i])
#   temp = mrt_tapin_ts[station_network_list[i]]
# }
fiveVTS
mrt_tapin_ts
mrt_tapin_ts = ts(mrt_tapin_ts)
class(mrt_tapin_ts)

answer <- GNARfit(vts = mrt_tapin_ts, net = mrt_net, alphaOrder = 2, betaOrder =  c(1,1))
answer
#alphaOrder and betaOrder ??

coef(answer)

plot(mrt_tapin_ts[, 1], ylab = "Node A Time Series")
lines(ts(fitted(answer)[, 1]), col = 2)
# fitted lines only got to t=4
# returns t - alphaOrder values (6-2 =)

pred = predict(answer, n.ahead = 6)

for (i in 1:length(station_network_list)){
  print(station_network_list[i])
  print(pred[,i])
}
