library(keras)
library(pracma)

# At initial run, we had 181 names in the universe
# so we are looking at a compression factor of 11.3x
epochs <- 100

# Convert the prepared tibble to a matrix
returns <- createReturnsMatrix(closePrices)
inputDimensions <- ncol(returns)

# And normalise
returns.norm <- normalise(returns)

# 80/20 split
numDates <- nrow(returns.norm)
sampleSize <- floor(0.8 * numDates)
trainingIndex <- sample(seq_len(numDates), size = sampleSize)
returns.train <- returns.norm[trainingIndex, ]
returns.test <- returns.norm[-trainingIndex, ]

startDimension <- 10
endDimension <- 150
numRuns = (endDimension - startDimension) + 1

results <- data.frame(aeDimension = startDimension : endDimension, inSampleLoss = rep(NA, numRuns), oosLoss = rep(NA, numRuns))

for (i in startDimension : endDimension) {
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = i, activation = 'relu', input_shape = c(inputDimensions)) %>%
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
  
  results[(i-startDimension)+1,2] <- history$metrics$loss[epochs]
  results[(i-startDimension)+1,3] <- history$metrics$val_loss[epochs]
}


results %>%
  gather(sampleType, loss, -aeDimension) %>%
  ggplot(., aes(aeDimension, loss, colour = sampleType)) +
  geom_line()
