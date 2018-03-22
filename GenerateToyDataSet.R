library(dplyr)

# n hidden core factors, all following a random path of t_max steps
# m stocks, each following a path determined by
# beta_n * n + epsilon
# Initially make beta_n time invariant, then we can look a time varying betas.

# For simplicity, we will not introduce any scale to the n_t
# beta_n will be randomly selected i.i.d [-2,2]
# Variance of the n random walks (sigma_n) is a parameter

# n = number of hidden factors
# m = number of stocks
# t_max = number of timesteps
generateToyDataSet <- function(n = 5, m=200, t_max=1000) {
  
  # Calibration
  n_init = 100
  sigma_n = c(1,2,3,4,5)
  
  factorLevels <- sapply(sigma_n, function(x) generateRandomWalk(num_steps = t_max + 1, 
                                                                 n_init = n_init,
                                                                 sigma = x))
  
  factorReturns <- apply(factorLevels, 2, getReturn)
  
  betas <- matrix(runif(m * n, -2, 2), ncol = n)
  
  allReturns <- matrix(rep(NA, t_max*m), ncol = m)
  
  for (i in 1:t_max) {
    
    thisFactorReturn <- factorReturns[i,]
    allReturns[i,] <- apply(betas, 1, function(x) getOneReturn(thisFactorReturn, x))
  }
  
  allReturns <- data.frame(allReturns)
  
  colnames(allReturns) <- 1:length(allReturns)
  
  return (allReturns)
  
}

# TODO: Ensure this doesn't go below 0?
generateRandomWalk <- function(num_steps, n_init = 100, sigma = 1, uniform = TRUE) {
  
  randomDraws <- NA
  
  if (uniform) {
    
    randomDraws <- runif(num_steps, -sigma, sigma)
  }
  else {
    
    randomDraws <- rnorm(num_steps, 0, sigma)
  }
  
  return (n_init + cumsum(randomDraws))
}

getReturn <- function(x) {
  returns <- (x - lag(x)) / lag(x)
  return (returns[-1])
}

getOneReturn <- function(betas, factorLevel, noise = 0.01) {
  
  noiseTerm <- rnorm(0, sd = noise)
  return (sum(betas * factorLevel)  + noise)
}
