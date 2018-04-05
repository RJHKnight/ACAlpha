getOverTheDayMove <- function(syms, dates) {
  
  startDate = min(dates)
  endDate = max(dates)
  
  dailyData <- getDailyData(symList = syms, 
                            startDate = startDate, 
                            endDate = endDate,
                            columns = c("open", "close"))
  
  dailyData$move <- 10000 * (dailyData$close - dailyData$open) / dailyData$open
  
  # Add volatility
  volatility <- getSummaryData(
    symList = syms, 
    endDate = endDate,
    noDays = 20,
    columns = "meanhighlowdiff"
  )
  
  dailyData %<>%
    left_join(
      volatility, by = "sym")

  dailyData %<>%
    mutate(normalisedMove = move / (10000 * meanhighlowdiff)) %>%
    select(sym, date.x, normalisedMove) %>%
    dplyr::rename(date = date.x)
  
  return (dailyData)
  
}
