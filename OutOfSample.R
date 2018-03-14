# Requires the appropriate model to be run with in sample data.

symList <- getASXUniverse()

closePrices.oos <- getClosePrices(symList,
                                  startDate = "2018.01.01", 
                                  endDate = "2018.02.18")

returns.oos <- createReturnsMatrix(closePrices.oos)

# Filter to ensure we have the same universe
returns.oos <- returns.oos[,colnames(returns.oos) %in% colnames(returns)]
# And drop the first row
returns.oos <- returns.oos[-1,]

returns.norm.oos <- normalise(returns.oos)
returns.mean <- attributes(returns.norm.oos)$mean
returns.sd <- attributes(returns.norm.oos)$sd

fullPrediction.oos <- (model %>% predict(returns.norm.oos))

sym <- "BHP.AX"
colNum <- which(colnames(returns.oos) == sym)

RIO.predict <- denormalise(fullPrediction.oos[, colNum], returns.mean, returns.sd)
RIO.actual <- returns.oos[, colNum]

plot(RIO.predict, ylim = c(min(RIO.actual), max(RIO.actual)))
lines(RIO.actual, col="blue")
