# Return a list of dates within n,m 
# dates of an Earnings announcement
source("getNormalisedVolume.R")

getCorpActions <- function(syms, dates, daysBefore = 3) {
  
  startDate = offsetByWorkingDate(min(dates), daysBefore)
  endDate = max(dates)
  
  corpActions <- TicR::ticRequest("getCorpActions", syms, 
                             startDate = format(startDate, "%Y.%m.%d"),
                             endDate = format(endDate, "%Y.%m.%d"),
                             columns ="earnings")
  
  paddedDates <- corpActions %>%
    group_by(sym, date) %>%
    do(data.frame(date = .$date, sym = .$sym, corpActionDate = seq(from=offsetByWorkingDate(.$date, daysBefore), to=.$date, by = "day")))
  
  return (paddedDates)
  
}