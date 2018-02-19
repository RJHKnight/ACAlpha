# Cross Sectional SNE
library(keras)
library(TicR)
library(tidyverse)
library(magrittr)

# Setup
setUseWebServices(TRUE)
setWebServicesURL("###########")
setWebServicesUsername("##########")
setWebServicesPassword("##########")

getAverageTurnover <- function(syms) {
  
  todayString <- format(today(), "%Y.%m.%d")
  turnover <- getSummaryData(syms, columns = "meanturnover", endDate = todayString, noDays = 20)
  
  return (turnover$meanturnover)
}


getASXUniverse <- function() {
  
  # Criteria is:
  # - Member of the ASX200
  # - Average Daily Turnover > $2m AUD
  myUniverse <-
    read_csv("https://www.asx200list.com/uploads/csv/20180101-asx200.csv",  skip = 1) %>%
    mutate(Code = paste(Code, ".AX", sep="")) %>%
    select(Code) %>%
    mutate(turnover = getAverageTurnover(Code)) %>%
    filter(turnover > 2000000) %>%
    select(Code)
  
  return (myUniverse$Code)
}



getClosePrices <- function(syms, startDate, endDate) {

  closePrices <- as.tibble(getDailyData(symList = syms,
                                        startDate = startDate, 
                                        endDate = endDate,
                                        columns = "close"))
  
  closePrices %<>%
    arrange(sym, date) %>%
    group_by(sym) %>%
    mutate(return = log(close/lag(close)))
  
  # Add volatility
  closevolatility <- getSummaryData(
    symList = unique(closePrices$sym), 
    endDate = "2018.02.05",
    noDays = 20,
    columns = "closevolatility"
  )
  
  closePrices %<>%
    left_join(
      select(closevolatility, -date),
      by = "sym")
  
  return (closePrices)
}

