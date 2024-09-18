# Section 2.3 GNAR Network Example
# install.packages("GNAR")
# install.packages("igraph")

library("GNAR")
library("igraph")
# set.seed(1)

plot(fiveNet, vertex.label = c("A", "B", "C", "D", "E"))
print(fiveNet)
summary(fiveNet)

fiveNet2 <- GNARtoigraph(net = fiveNet)
summary(fiveNet2)

fiveNet3 <- igraphtoGNAR(fiveNet2)
all.equal(fiveNet, fiveNet3)

g <- make_ring(10)
print(igraphtoGNAR(g))

as.matrix(fiveNet)

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
plot(ts(residuals(answer)[, 1]), ylab = "answer model residuals")st
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

