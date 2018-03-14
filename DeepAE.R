library(keras)
library(pracma)
library(tidyverse)

# At initial run, we had 181 names in the universe
# so we are looking at a compression factor of 11.3x
epochs <- 200

returns <- createReturnsFrame(closePrices)

returns <- addNormalisedValues(returns)

# 80/20 split
returns.matrix <- getNormalisedMatrix(returns)
numDates <- nrow(returns.matrix)
sampleSize <- floor(0.8 * numDates)
trainingIndex <- sample(seq_len(numDates), size = sampleSize)
returns.train <- returns.matrix[trainingIndex, ]
returns.test <- returns.matrix[-trainingIndex, ]


model <- keras_model_sequential()
model %>%
  layer_dense(units = i, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = i-20, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = i-40, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = i-60, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = i-40, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = i-20, activation = 'relu', input_shape = c(inputDimensions)) %>%
  layer_dense(units = inputDimensions, activation = 'sigmoid')

model %>% compile(
  loss = loss_mean_squared_error,
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

fullPrediction <- (model %>% predict(returns.matrix))

