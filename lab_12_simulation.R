generate_data <- function(n,p){
  # Write a function `generate_data(n, p)` which returns a list
  # with the following elements: `covariates` which is a n-by-p matrix
  # of draws from the standard normal distribution, and `responses`
  # which is a vector of length n of draws from the standard normal.
  return(list(covariates = matrix(rnorm(n*p,mean=0,sd=1), nrow = n, ncol=p)), responses = rnorm(n,mean=0,sd=1))
}