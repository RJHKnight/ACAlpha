library(lubridate)
library(TicR)
library(zoo)
library(tidyverse)
library(magrittr)
library(scales)

source("calculateWealthAccumulation.R")

numberOfPositionsPerSide = 5

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

corpActions <- getCorpActions(syms = unique(returns.long$symbol), 
                              dates = unique(returns.long$date))

returns.long %<>%
  left_join(corpActions, by = c("date" = "corpActionDate", "symbol" = "sym"))

minVol <- 1/5
maxVol <- 5

returns.long %<>%
  dplyr::group_by(date) %>%
  dplyr::filter(abs(predictedReturn) < 0.15) %>%
  dplyr::filter(volume.norm < maxVol && volume.norm > minVol) %>%
  dplyr::filter(is.na(date.y)) %>%
  dplyr::mutate(rank = rank(predictedReturn, ties.method = "first")) %>%
  dplyr::mutate(longPosition = rank > max(rank) - numberOfPositionsPerSide,
         shortPosition = rank < numberOfPositionsPerSide + 1)

allDates <- unique(returns.long$date)

strategyReturnsWithVolAndCorp <- data.frame(date = allDates, return = NA)

for (i in (1:length(allDates))) {
  
  thisDate <- allDates[i]
  thisReturns <- subset(returns.long, date == thisDate)
  
  thisLongSyms <- subset(thisReturns, longPosition)$symbol
  thisShortSyms <- subset(thisReturns, shortPosition)$symbol
  
  thisReturn <- calculateWealthAccumulation(thisLongSyms, thisShortSyms, closePrices, thisDate)
  strategyReturnsWithVolAndCorp[i,2] <- thisReturn
}

strategyReturns <- subset(strategyReturns, date >= strategyReturnsWithVol$date[1])
strategyReturns$type <- "Base"
strategyReturns$cumReturn <- cumsum(strategyReturns$return)
strategyReturnsWithVolAndCorp$type <- paste("VolCorpKnockout (", minVol, "->", maxVol, ")", sep = "")
strategyReturnsWithVolAndCorp$cumReturn <- cumsum(strategyReturnsWithVolAndCorp$return)

ggplot(rbind(strategyReturns, strategyReturnsWithVolAndCorp), 
       aes(date, cumReturn, colour=type, group=type)) + 
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%Y.%m")