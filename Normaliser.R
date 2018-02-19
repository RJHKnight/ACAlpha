normalise <- function(input) {
 
  # And normalise
  input.mean <- mean(input)
  input.sd <- sd(input)
  
  input.scaled <- (input - input.mean) / input.sd
  input.uniform <- 0.5 + (0.5 * erf(-1 * (input.scaled/sqrt(2))))
  
  attr(input.uniform, 'mean') <- input.mean
  attr(input.uniform, 'sd') <- input.sd
  
  
  return (input.uniform)
   
}

denormalise <- function(input.normalised, input.mean, input.sd) {
  
  input.scaled <- -1 * sqrt(2) * erfinv((input.normalised - 0.5) / 0.5)
  input.raw <- (input.sd * input.scaled) + input.mean
  
  return (input.raw)
}

