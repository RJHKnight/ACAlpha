# Return the daily volume normalised
# by the previous 20 day average
library(lubridate)
library(TicR)
library(tidyverse)
library(zoo)

getNormalisedVolume <- function(syms, dates, normalisationPeriod = 20) {
  
  startDate <- offsetByWorkingDate(min(dates), normalisationPeriod)
  endDate <- max(dates)
  
  volume <- getDailyData(symList = syms, startDate = format(startDate, "%Y.%m.%d"), 
                         endDate = format(endDate, "%Y.%m.%d"), columns = "volume")
  
  return (
    volume %>%
      arrange(date) %>%
      group_by(sym) %>%
      mutate(volume.ma = rollapply(data = lag(volume, 1), 
                                   width = normalisationPeriod, 
                                   FUN = mean, 
                                   align = "right", 
                                   fill = NA, 
                                   na.rm = T)) %>%
      filter(!is.na(volume.ma)) %>%
      mutate(volume.norm = volume / volume.ma) %>%
      filter(volume.norm > 0) %>%
      select(date, sym, volume.norm) 
      
  )
  
}

offsetByWorkingDate <- function(date, numDays) {

  if (numDays==0) { return (date) }
  
  else {
    workingDate = getPreviousWorkingDay(date)
    return (offsetByWorkingDate(workingDate, numDays-1))
  }
    
} 

getPreviousWorkingDay <- function(date) {
  
  # 1 = Monday, 7 = Sunday
  dow <- wday(date, week_start = 1)
  
  offset <- 1
  if (dow > 5) {
    
    offset = rawOffset(dow, 5)
  }
  else if (dow == 1) {
    offset = 3
  }
  
  return (rawOffset(date,offset))
}

rawOffset <- function(date, numDays) {
  
  if (is.POSIXct(date))
    return ( date - days(numDays))
  
  return (date - numDays)
}