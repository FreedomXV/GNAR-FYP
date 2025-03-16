# Section 2.3 GNAR Network Example
# install.packages("GNAR")
# install.packages("igraph")

library("GNAR")
library("igraph")
# set.seed(1)

plot(fiveNet, vertex.label = c("A", "B", "C", "D", "E"))
typeof(fiveNet)
class(fiveNet)
print(fiveNet)
summary(fiveNet)

fiveNet2 <- GNARtoigraph(net = fiveNet)
summary(fiveNet2)

fiveNet3 <- igraphtoGNAR(fiveNet2)
all.equal(fiveNet, fiveNet3)

g <- make_ring(10)
print(igraphtoGNAR(g))

as.matrix(fiveNet)

mat = matrix(c(1,2,3), c(4,5,6), ncol=3, nrow=2)
mat
set.seed(920)
adj <- matrix(runif(9), ncol = 3, nrow = 3)
adj[adj < 0.3] <- 0
print(matrixtoGNAR(adj))

# Section 2.4 GNAR Network Example
# The fiveNet network

data("fiveNode", package="GNAR")
answer <- GNARfit(vts = fiveVTS, net = fiveNet, alphaOrder = 2, betaOrder =  c(1,1))
answer

coef(answer)

plot(fiveVTS[, 1], ylab = "Node A Time Series")
lines(fitted(answer)[, 1], col = 2)

myresiduals = residuals(answer)[, 1]
layout(matrix(c(1,2), 2, 1))
par(mar = c(4.1, 4.1, 0, 2.1), cex.axis = 0.9)
plot(ts(residuals(answer)[, 1]), ylab = "answer model residuals")
par(mar = c(4.1, 4.1, 2.1, 2.1), cex.axis = 0.9)
hist(residuals(answer)[, 1], main= "", xlab = "answer model residuals")


# Section 2.5 GNAR Network Example
# GNAR data simulation

set.seed(10)
fiveVTS2 = GNARsim(n = 200, net = fiveNet, alphaParams = list(c(0.4, 0, -0.6, 0, 0)), betaParams = list(c(0.3)))

print(GNARfit(vts = fiveVTS2, net = fiveNet, alphaOrder = 1, betaOrder = 1, globalalpha = FALSE))

set.seed(10)
fiveVTS3 = GNARsim(n = 200, net = fiveNet, alphaParams = list(rep(0.2, 5), rep(0.3, 5)), betaParams = list(c(0.2, 0.3), c(0)))

print(GNARfit(vts = fiveVTS3, net = fiveNet, alphaOrder = 2, betaOrder =c(2,0)))

fiveVTS4 <- simulate(GNARfit(vts = fiveVTS2, net = fiveNet, alphaOrder = 1, betaOrder = 1, globalalpha = FALSE), n = 200)

# Section 2.6 GNAR Network Example
# Missing data and changing connection weights

fiveVTS0 <- fiveVTS
fiveVTS0[50:150, 3] <- NA
nafit <- GNARfit(vts = fiveVTS0, net = fiveNet, alphaOrder = 2, betaOrder = c(1, 1))
layout(matrix(c(1, 2), 2, 1))
par(mar = c(4.1, 4.1, 0.75, 2.1), cex.axis = 0.75)
plot(ts(fitted(nafit)[, 3]), ylab = "Node C fitted values")
par(mar = c(4.1, 4.1, 0.75, 2.1), cex.axis = 0.75)
plot(ts(fitted(nafit)[, 4]), ylab = "Node D fitted values")

# Section 2.7 Stationarity conditions

set.seed(10)
fiveVTS4 <- GNARsim(n = 200, net = fiveNet, alphaParams = list(rep(0.2, 5)), betaParams = list(c(0.85)))
c(mean(fiveVTS4[1:50, ]), mean(fiveVTS4[51:100, ]), mean(fiveVTS4[101:150, ]), mean(fiveVTS4[151:200, ]))

# Section 3.1 Order selection

BIC(GNARfit())
BIC(GNARfit(betaOrder = c(2, 1)))


# Section 3.2 Example model selection for wind ts

oldpar<-par(cex=0.75)
windnetplot()
par(oldpar)

BIC(GNARfit(vts=vswindts,net=vswindnet,alphaOrder=1, betaOrder=0))
vswind_answer = GNARfit(vts=vswindts,net=vswindnet,alphaOrder=1, betaOrder=0)

# Call:
#   lm(formula = yvec ~ dmat + 0)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.6936 -0.0962  0.0350  0.1863  3.7612 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# dmat 0.9820367  0.0007074    1388   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.417 on 73439 degrees of freedom
# Multiple R-squared:  0.9633,	Adjusted R-squared:  0.9633 
# F-statistic: 1.927e+06 on 1 and 73439 DF,  p-value: < 2.2e-16
# 
# GNAR BIC: -233.3848

# for loop to test different betaOrders

BIC.Alpha2.Beta <- matrix(0, ncol = 15, nrow = 15)
for(b1 in 0:14){
  for(b2 in 0:14){
    print(b1)
    print(b2)
    print('calculating BIC')
    BIC.Alpha2.Beta[b1 + 1, b2 + 1] <- BIC(GNARfit(vts = vswindts, net = vswindnet, alphaOrder = 2, betaOrder = c(b1, b2)))
  }
}
  
contour(0:14, 0:14, log(251 + BIC.Alpha2.Beta), xlab = "Lag 1 Neighbor Order", ylab = "Lag 2 Neighbor Order")

goodmodel <- GNARfit(vts = vswindts, net = vswindnet, alphaOrder = 2, betaOrder = c(5, 1))
goodmodel

# 3.3 Constructing a inferred network

prediction <- predict(GNARfit(vts = fiveVTS[1:199,], net = fiveNet, alphaOrder = 2, betaOrder = c(1, 1)))
prediction

# 4 Use OECD example for network predictions
# install.packages('fields')

library("fields")
layout(matrix(c(1, 2), nrow = 1, ncol = 2), widths = c(4.5, 1))
image(t(apply(gdpVTS, 1, rev)), xaxt = "n", yaxt = "n", col = gray.colors(14), xlab = "Year", ylab = "Country")
axis(side = 1, at = seq(from = 0, to = 1, length = 52), labels = FALSE, col.ticks = "grey")
axis(side = 1, at = seq(from = 0, to = 1, length = 52)[5*(1:11)], labels = (1:52)[5*(1:11)])
axis(side = 2, at = seq(from = 1, to = 0, length = 35), labels = colnames(gdpVTS), las = 1, cex = 0.8)
layout(matrix(1))
image.plot(zlim = range(gdpVTS, na.rm = TRUE), legend.only = TRUE, col = gray.colors(14))

net1 <- seedToNet(seed.no = seed.nos[1], nnodes = 35, graph.prob = 0.15)
net2 <- seedToNet(seed.no = seed.nos[2], nnodes = 35, graph.prob = 0.15)
layout(matrix(c(2, 1), 1, 2))
par(mar=c(0,1,0,1))
plot(net1, vertex.label = colnames(gdpVTS), vertex.size = 0)
plot(net2, vertex.label = colnames(gdpVTS), vertex.size = 0)

## -----------------------
## Scrap code from MRT

# MANUALLY Defining MRT Net
# TODO Find automatic way to do it
# mrt_list = matrix(0, nrow = length(station_network_list), ncol = length(station_network_list))
# rownames(mrt_list) = station_network_list
# colnames(mrt_list) = station_network_list
# mrt_list
# 
# mrt_list['TE1', 'NS9.TE2'] = 1
# mrt_list['NS9.TE2', 'TE1'] = 1
# mrt_list['NS9.TE2', 'TE3'] = 1
# mrt_list['NS9.TE2', 'NS8'] = 1
# mrt_list['NS9.TE2', 'NS10'] = 1
# mrt_list['TE3', 'NS9.TE2'] = 1
# mrt_list['NS8', 'NS9.TE2'] = 1
# mrt_list['NS10', 'NS9.TE2'] = 1
# 
# mrt_list
# mrt_net = matrixtoGNAR(mrt_list)
# mrt_net
# plot(mrt_net, vertex.label = station_network_list)




# Covering Woodlands Examples ONLY
# TE1, NS9/TE2, TE3, NS8, NS10

# mrt_list = read.csv('Data Processing/Data/Master Sets/StationTimeSeries_TapInVolume.csv')
# station_network_list = c('TE1', 'NS9.TE2', 'TE3', 'NS8', 'NS10')
# station_network = subset(mrt_data, PT_CODE %in% station_network_list)
# station_network
# 
# ts.plot(nyc_ts)
# nyc_ts_diff = diff(nyc_ts)
# ts.plot(rowMeans(nyc_ts_diff))
# 
# # install.packages("tseries")
# library(tseries)
# # Perform the Augmented Dickey-Fuller Test
# adf_result <- adf.test(rowMeans(nyc_ts_diff))
# print(adf_result)
# 
# # Z-score standardization
# nyc_ts_diff_norm <- (nyc_ts_diff - mean(nyc_ts_diff)) / sd(nyc_ts_diff)
# nyc_ts_norm <- (nyc_ts - mean(nyc_ts)) / sd(nyc_ts)