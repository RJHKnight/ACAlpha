library(MASS)


# TODO: Make this tidy! Group_by?
# Remove if more than 20 returns are missing over the entire duration
stocksToKeep <- (apply(returns, 2, function(x) sum(is.na(x))) < 20)
returns <- returns[,stocksToKeep]
returns <- returns[complete.cases(returns[,]),]
returns <- returns[apply(returns, 2, function(x) max(abs(x))) < 0.5,]

details.pca <- runPCA(returns)

pD <- NA
pD$factor.exposures = details.pca$loadings
pD$specific.variance = details.pca$varResid
pD$loadings = NULL

w = sqrt(pD$specific.variance)
w = 1 / w

#Factor Exposures
fExp = pD$factor.exposures		
#Factor returns
G = ginv(t(fExp) %*% diag(w) %*% fExp)
b0 = G %*% t(fExp) %*% diag(w) %*% t(returns)		#weighted regression 


alpha = t(returns) - fExp %*% b0

# Compare with DAE
pca.alpha <- alpha["RIO.AX",]
dae.alpha <- RIO.predict 
actual <- returns[, colNum]

startI = 100
endI = 260

plot(actual[startI : endI])
lines(pca.alpha[startI : endI], col="red")
lines(dae.alpha[startI : endI], col="blue")
lines(actual[startI : endI], col="green")

