calculateWealthAccumulation <- function(longSyms, shortSyms, closePrices, thisDate, weights = NULL) {
  
  allSyms <- c(longSyms, shortSyms)
  
  realisedReturns <- closePrices %>%
    arrange(sym, date) %>%
    mutate(nextClose = lead(close)) %>%
    mutate(closeToCloseReturn = 100 * (nextClose - close)/close) %>%
    filter(sym %in% allSyms) %>%
    filter(date == thisDate) %>%
    mutate(realisedReturn = ifelse(sym %in% longSyms, closeToCloseReturn, -closeToCloseReturn)) %>%
    select(sym, realisedReturn)
  
  if (is.null(weights)) {
    return (mean(realisedReturns$realisedReturn, na.rm = TRUE))
  }
  
  colnames(realisedReturns) <- realisedReturns$sym
  realisedReturns$weight <- NA
  
  # Weights based on the order of long/short syms
  realisedReturns[longSyms]$weight <- weights
  realisedReturns[shortSyms]$weight <- weights
  
  return (weighted.mean(realisedReturns$realisedReturn, realisedReturns$weight, na.rm = TRUE))
}