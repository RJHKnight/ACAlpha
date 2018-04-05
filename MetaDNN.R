# Deep Neural Network to condition the alpha forecast based on
#
# 1) Daily Volume
# 2) Daily Move
# 3) Previous Alpha Forecast
# 4) Corporate Actions?


######################
# Alpha
######################

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

# Add volatility
closevolatility <- getSummaryData(
  symList = unique(returns.long$symbol), 
  endDate = "2018.02.05",
  noDays = 20,
  columns = "closevolatility"
)

returns.long %<>%
  left_join(
    select(closevolatility, -date),
    by = c("symbol"="sym")) %>%
  mutate(alpha.raw = 10000 * alpha * closevolatility)



######################
# Predictors
######################
normalisedVolume <- getNormalisedVolume(syms = unique(returns.long$symbol), 
                                        dates = unique(returns.long$date))

returns.long %<>%
  left_join(normalisedVolume, by = c("date" = "date", "symbol"="sym"))


overTheDayMove <- getOverTheDayMove(
  syms = unique(returns.long$symbol),
  dates = unique(returns.long$date)
)

returns.long %<>%
  left_join(overTheDayMove, 
            by = c("symbol" = "sym", "date"="date"))

######################
# Actual Return
######################
realisedReturns <- closePrices %>%
  arrange(sym, date) %>%
  mutate(nextClose = lead(close)) %>%
  mutate(closeToCloseReturn = 10000 * (nextClose - close)/close) %>%
  select(date, sym, closeToCloseReturn)

returns.long %<>%
  left_join(realisedReturns, 
            by = c("symbol" = "sym", "date"="date"))


returns.long %>%
  filter(!is.na(closeToCloseReturn) & !is.na(volume.norm) & !is.na(alpha.raw)) %>%
  group_by(predictedReturnBucket = cut2(alpha.raw, g=10),
           volBucket = cut2(volume.norm, g=6)) %>%
  dplyr::summarise(meanReturn = mean(closeToCloseReturn)) %>%
  ggplot(., aes(predictedReturnBucket, meanReturn, fill=volBucket)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~ volBucket, scales = "free_y")

returns.long %>%
  filter(!is.na(closeToCloseReturn) & !is.na(volume.norm) & !is.na(alpha.raw)) %>%
  mutate(predictedReturnBucket = cut2(alpha.raw, g=10),
           volBucket = cut2(volume.norm, g=6)) %>%
  ggplot(., aes(predictedReturnBucket, closeToCloseReturn, fill=volBucket)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits=c(-600,600)) + 
  facet_wrap(~ volBucket)



