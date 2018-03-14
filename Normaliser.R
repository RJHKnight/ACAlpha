addNormalisedValues <- function(returnsFrame) {
 
  input.matrix <- 
    returnsFrame %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  # And normalise
  input.mean <- mean(input.matrix)
  input.sd <- sd(input.matrix)
  
  input.scaled <- (input.matrix - input.mean) / input.sd
  input.uniform <- 0.5 + (0.5 * erf(-1 * (input.scaled/sqrt(2))))
  
  input.uniform <- as.data.frame(input.uniform)
  colnames(input.uniform) <- paste(colnames(input.uniform), "norm", sep = ".")
  
  returnsFrame %<>%
    bind_cols(input.uniform) %>%
    add_column(input.mean) %>%
    add_column(input.sd)
    
  return (returnsFrame)
   
}

getNormalisedMatrix <- function(returnsFrame) {
  
  returns.matrix <- returnsFrame %>%
    select(contains("norm")) %>%
    as.matrix()

}


denormaliseMatrix <- function(input.normalised, input.mean, input.sd) {
  
  input.scaled <- -1 * sqrt(2) * erfinv((input.normalised - 0.5) / 0.5)
  input.raw <- (input.sd * input.scaled) + input.mean
  
  return (input.raw)
}

