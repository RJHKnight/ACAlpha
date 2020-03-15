# Market Neutral - Long/Short strategy
# Rebalance at End Of Day
# No knock-outs in the simple strategy.
# For now assume no transaction costs, so we can just calculate wealth accumulation per day.


numberOfPositionsPerSide = 20

returns.long <- 
  returns %>%
  select(- contains("norm"), -input.mean, -input.sd) %>%
  select(- contains("pred")) %>%
  gather(symbol, return, -date) 

returns.long <-
  cbind(returns.long,
        returns %>%
          select(- contains("norm"), -input.mean, -input.sd) %>%
          select(contains("pred"), date) %>%
          gather(symbol, predictedReturn, -date) %>%
          select(predictedReturn)
  )


returns.long$alpha <- (returns.long$predictedReturn - returns.long$return)

returns.long %<>%
  arrange(date) %>%
  group_by(date) %>%
  filter(abs(alpha) < 0.15) %>%
  mutate(rank = rank(alpha, ties.method = "first")) %>%
  mutate(longPosition = rank > max(rank) - numberOfPositionsPerSide,
         shortPosition = rank < numberOfPositionsPerSide + 1)

allDates <- unique(returns.long$date)

strategyReturns <- data.frame(date = allDates, return = NA)

for (i in (1:length(allDates))) {
  
  thisDate <- allDates[i]
  thisReturns <- subset(returns.long, date == thisDate)
  
  thisLongSyms <- subset(thisReturns, longPosition)$symbol
  thisShortSyms <- subset(thisReturns, shortPosition)$symbol
  
  thisReturn <- calculateWealthAccumulation(thisLongSyms, thisShortSyms, closePrices, thisDate)
  strategyReturns[i,2] <- thisReturn
}


strategyReturns$type <- "Basic"
strategyReturns$cumReturn <- cumsum(strategyReturns$return)

ggplot(strategyReturns, 
       aes(date, cumsum(return), colour=type, group=type)) + 
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("3 month"), date_labels = "%Y.%m")
