getOverTheDayMove <- function(syms, dates) {
  
  startDate = min(dates)
  endDate = max(dates)
  
  dailyData <- getDailyData(symList = syms, 
                            startDate = startDate, 
                            endDate = endDate,
                            columns = c("open", "close"))
  
  dailyData$move <- 10000 * (dailyData$close - dailyData$open) / dailyData$open
  
  # Add volatility
  closevolatility <- getSummaryData(
    symList = syms, 
    noDays = 20,
    columns = "closevolatility"
  )
  
  dailyData %<>%
    left_join(
      closevolatility, by = "sym")

  dailyData %<>%
    mutate(normalisedMove = move / closevolatility) %>%
    select(sym, date, normalisedMove)
  
  return (dailyData)
  
}
