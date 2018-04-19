generate_data <- function(n,p){
  # Write a function `generate_data(n, p)` which returns a list
  # with the following elements: `covariates` which is a n-by-p matrix
  # of draws from the standard normal distribution, and `responses`
  # which is a vector of length n of draws from the standard normal.
  return(list(covariates = matrix(rnorm(n*p,mean=0,sd=1), nrow = n, ncol=p)), responses = rnorm(n,mean=0,sd=1))
}

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

run_simulation<- function(n_trials, n, p, cutoff){
    x = generate_data(n,p)
    p.vals = model_select(x$covariates, x$responses,cutoff)
    save(p.vals, "p_values.Rdata")
}

make_plot <-function(datapath){
  plot(readLines(datapath))
}



