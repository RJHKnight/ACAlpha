createReturnsMatrix <- function(closePrices) {
  
  returns <- as.matrix(
    closePrices %>%
      filter(!is.na(return)) %>%
      mutate(return = return/closevolatility) %>%
      arrange(date) %>%
      select(-close, -closevolatility) %>%
      spread(sym, return) %>%
      select(-date)
  )
  
  # TODO: Make this tidy! Group_by?
  # Remove if more than 20 returns are missing over the entire duration
  stocksToKeep <- (apply(returns, 2, function(x) sum(is.na(x))) < 20)
  returns <- returns[,stocksToKeep]
  returns <- returns[complete.cases(returns[,]),]
  
  return (returns)
}