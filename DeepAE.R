library(keras)
library(pracma)
library(tidyverse)

source("normaliser.R")
source("GenerateToyDataSet.R")

epochs <- 50

returns <- createReturnsFrame(closePrices)
#returns <- generateToyDataSet()

returns <- addNormalisedValues(returns)

returns.norm <- getNormalisedMatrix(returns)
inputDimensions <- ncol(returns.norm)

# 80/20 split
sampleSize <- floor(0.8 * nrow(returns.norm))
trainingIndex <- sample(seq_len(nrow(returns.norm)), size = sampleSize)
returns.train <- returns.norm[trainingIndex, ]
returns.test <- returns.norm[-trainingIndex, ]


model <- keras_model_sequential()
model %>%
  layer_dense(units = 60, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = 40, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = 20, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = 40, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = 60, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = inputDimensions, activation = 'sigmoid')

model %>% compile(
  loss = loss_mean_absolute_error,
  optimizer = optimizer_adadelta(),
  metrics = c('mae')
)

# Train & Evaluate -------------------------------------------------------

history <- model %>% fit(
  returns.train, returns.train,
  batch_size = 1,
  epochs = epochs,
  verbose = 1,
  validation_data = list(returns.test, returns.test)
)

plot(history)

prediction.norm <- (model %>% predict(returns.norm))

colnames(prediction.norm) <- colnames(returns)[1:inputDimensions]
returns <- addPredictions(returns, prediction.norm, denormalise = TRUE)

# Comparison plot
returns %>%
  dplyr::mutate(i = 1:nrow(returns)) %>%
  dplyr::select(i, contains("COH")) %>%
  dplyr::select(-contains("norm")) %>%
  gather(type, value, -i) %>%
  mutate(panel1 = i < 250) %>%
  ggplot(., aes(i, value, colour=type)) +
  geom_line() + 
  facet_wrap(~ panel1, scales = "free_x", ncol = 1)
  
