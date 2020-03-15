# Deep Neural Network to condition the alpha forecast based on
#
# 1) Daily Volume
# 2) Daily Move
# 3) Previous Alpha Forecast
# 4) Corporate Actions?


######################
# Alpha
######################
source("getOverTheDayMove.R")
library(Hmisc)
library(tidyverse)

returns.long <- 
  returns %>%
  dplyr::select(- contains("norm"), -input.mean, -input.sd) %>%
  dplyr::select(- contains("pred")) %>%
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


returns.long %>%
  filter(!is.na(closeToCloseReturn) & !is.na(volume.norm) & !is.na(alpha.raw)) %>%
  group_by(predictedReturnBucket = cut2(alpha.raw, g=5),
           dayMove = cut2(normalisedMove, g=6)) %>%
  summarise(averageReturn = mean(closeToCloseReturn)) %>%
  ggplot(., aes(predictedReturnBucket, averageReturn, fill=dayMove)) + 
  geom_bar(stat="identity") +
  facet_wrap(~ dayMove)

predictors.matrix <- returns.long %>%
  filter(!is.na(closeToCloseReturn)) %>%
  filter(!is.na(volume.norm)) %>%
  select(alpha.raw, volume.norm, normalisedMove) %>%
  as.matrix(.)

results <- returns.long %>%
  filter(!is.na(closeToCloseReturn)) %>%
  filter(!is.na(volume.norm)) %>%
  select(closeToCloseReturn)


inputDimensions <- ncol(predictors.matrix)

# 80/20 split
sampleSize <- floor(0.8 * nrow(predictors.matrix))
trainingIndex <- sample(seq_len(nrow(predictors.matrix)), size = sampleSize)

predictors.train <- predictors.matrix[trainingIndex, ]
predictors.test <- predictors.matrix[-trainingIndex, ]

results.train <- results$closeToCloseReturn[trainingIndex]
results.test <- results$closeToCloseReturn[-trainingIndex]


model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = inputDimensions, activation = 'sigmoid')

model %>% compile(
  loss = loss_mean_absolute_error,
  optimizer = optimizer_adadelta(),
  metrics = c('mae')
)

# Train & Evaluate -------------------------------------------------------

history <- model %>% fit(
  predictors.train, results.train,
  batch_size = 1,
  epochs = epochs,
  verbose = 1,
  validation_data = list(predictors.test, results.test)
)

