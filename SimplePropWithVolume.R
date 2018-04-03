# Market Neutral - Long/Short strategy
# Rebalance at End Of Day
# No knock-outs in the simple strategy.
# For now assume no transaction costs, so we can just calculate wealth accumulation per day.
source("calculateWealthAccumulation.R")

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

normalisedVolume <- getNormalisedVolume(syms = unique(returns.long$symbol), 
                                        dates = unique(returns.long$date))

returns.long %<>%
  right_join(normalisedVolume, by = c("date" = "date", "symbol"="sym")) 

minVol <- 1/10
maxVol <- 10

returns.long %<>%
  group_by(date) %>%
  filter(abs(predictedReturn) < 0.15) %>%
  filter(volume.norm < maxVol && volume.norm > minVol) %>%
  mutate(rank = rank(predictedReturn, ties.method = "first")) %>%
  mutate(longPosition = rank > max(rank) - numberOfPositionsPerSide,
         shortPosition = rank < numberOfPositionsPerSide + 1)

allDates <- unique(returns.long$date)

strategyReturnsWithVol <- data.frame(date = allDates, return = NA)

for (i in (1:length(allDates))) {
  
  thisDate <- allDates[i]
  thisReturns <- subset(returns.long, date == thisDate)
  
  thisLongSyms <- subset(thisReturns, longPosition)$symbol
  thisShortSyms <- subset(thisReturns, shortPosition)$symbol
  
  thisReturn <- calculateWealthAccumulation(thisLongSyms, thisShortSyms, closePrices, thisDate)
  strategyReturnsWithVol[i,2] <- thisReturn
}

strategyReturns <- subset(strategyReturns, date >= strategyReturnsWithVol$date[1])
strategyReturns$type <- "Base"
strategyReturns$cumReturn <- cumsum(strategyReturns$return)
strategyReturnsWithVol$type <- paste("VolKnockout (", minVol, "->", maxVol, ")", sep = "")
strategyReturnsWithVol$cumReturn <- cumsum(strategyReturnsWithVol$return)

ggplot(rbind(strategyReturns, strategyReturnsWithVol), 
       aes(date, cumReturn, colour=type, group=type)) + 
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%Y.%m")
     