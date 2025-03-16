# R Script Testing for GNAR Models and observing prediction error
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")

frequency = 'daily'
alphaOrder = 11
betaRep = 3
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

ts.plot(rowMeans(nyc_ts))
nyc_ts_diff = diff(nyc_ts)
ts.plot(rowMeans(nyc_ts_diff))
nyc_ts_diff_norm = (nyc_ts_diff - mean(nyc_ts_diff)) / sd(nyc_ts_diff)

library(tseries)
# Perform the Augmented Dickey-Fuller Test
adf_result <- adf.test(rowMeans(nyc_ts_diff_norm))
print(adf_result)

# Stationary under the assumption
# ** All stations share the same time series model, with different coefficients, i.e applying the same differencing operator will stationarise the whole thing

station_network_labels = colnames(nyc_ts)
nyc_adjacency = nyc_adjacency[station_network_labels, station_network_labels]
nyc_net = matrixtoGNAR(nyc_adjacency)
nyc_net

answer = GNARfit(vts = nyc_ts_diff_norm, net = nyc_net, alphaOrder = alphaOrder, betaOrder = betaOrder)
summary(answer)

# BIC.Alpha2.Beta <- matrix(0, ncol = 5, nrow = 5)
# for(b1 in 0:4){
#   for(b2 in 0:4){
#     print(b1)
#     print(b2)
#     print('calculating BIC')
#     BIC.Alpha2.Beta[b1 + 1, b2 + 1] <- BIC(GNARfit(vts = nyc_ts_diff, net = nyc_net, alphaOrder = 2, betaOrder = c(b1, b2)))
#   }
# }
# 
# contour(0:4, 0:4, log(abs(floor(min(BIC.Alpha2.Beta))) + BIC.Alpha2.Beta), xlab = "Lag 1 Neighbor Order", ylab = "Lag 2 Neighbor Order")
