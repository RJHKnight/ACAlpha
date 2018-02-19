library(keras)
library(pracma)
library(tidyverse)

# At initial run, we had 181 names in the universe
# so we are looking at a compression factor of 11.3x
epochs <- 200

returns <- createReturnsMatrix(closePrices)
inputDimensions <- ncol(returns)

returns.norm <- normalise(returns)

# 80/20 split
numDates <- nrow(returns.norm)
sampleSize <- floor(0.8 * numDates)
trainingIndex <- sample(seq_len(numDates), size = sampleSize)
returns.train <- returns.norm[trainingIndex, ]
returns.test <- returns.norm[-trainingIndex, ]


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

fullPrediction <- (model %>% predict(returns.norm))

sym <- "RIO.AX"
colNum <- which(colnames(returns) == "RIO.AX")

RIO.predict <- denormalise(fullPrediction[, colNum], returns.mean, returns.sd)
RIO.actual <- returns[, colNum]

plot(RIO.predict, ylim = c(min(RIO.actual), max(RIO.actual)))
lines(RIO.actual, col="blue")

