generate_data <- function(n,p){
  # Write a function `generate_data(n, p)` which returns a list
  # with the following elements: `covariates` which is a n-by-p matrix
  # of draws from the standard normal distribution, and `responses`
  # which is a vector of length n of draws from the standard normal.
  return(list(covariates = matrix(rnorm(n*p,mean=0,sd=1), nrow = n, ncol=p)), responses = rnorm(n,mean=0,sd=1))
}

Write a function `model_select(covariates, responses,
                               cutoff)` which fits the linear regression `responses ~ covariates`
and retains only those covariates whose coefficient p-values are
less than or equal to `cutoff`. Then fit another regression using
only the retained covariates and return the p-values from this
reduced model. If there are no retained covariates return an empty
vector. *HINT*: You can use indexing inside of formulas:
  `lm(responses ~ covariates[, c(1, 2)])` will fit a regression with
only the first two covariates.

model_select <- function(covariates, responses,
                         cutoff){
  within.cutoff <- coef(lm(response~covariates)) <= cutoff
  the.p.vals <- c()
  if(any(which(within.cutoff))){
    new.lm<- lm(response~covariates[, which(within.cutoff)])
    the.p.vals <- new.lm$coefficients[,"Pr(>|t|)"]
  } 
  return(the.p.vals)
}