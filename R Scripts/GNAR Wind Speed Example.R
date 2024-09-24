# Section 3.2 GNAR Network Example (Wind Speed)
# install.packages("GNAR")
# install.packages("igraph")

library("GNAR")
library("igraph")
# set.seed(1)

oldpar = par(cex = 0.75)
windnetplot()
par(oldpar)

help(GNAR)
vswind
vswindnet
vswindts
vswindcoords
vswindnames
vswindcoords
write.csv(vswindts, "vswindts.csv")

BIC(GNARfit(vts = vswindts, net = vswindnet, alphaOrder = 1, betaOrder = 0))

BIC(GNARfit(vts = vswindts, net = vswindnet, alphaOrder = 1, betaOrder = 0, globalalpha = FALSE))

BIC.Alpha2.Beta <- matrix(0, ncol = 15, nrow = 15)

for(b1 in 0:14)
  for(b2 in 0:14)
    BIC.Alpha2.Beta[b1 + 1, b2 + 1] <- BIC(GNARfit(vts = vswindts, net = vswindnet, alphaOrder = 2, betaOrder = c(b1, b2)))


contour(0:14, 0:14, log(251 + BIC.Alpha2.Beta), xlab = "Lag 1 Neighbor Order", ylab = "Lag 2 Neighbor Order")
