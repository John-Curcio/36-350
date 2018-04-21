# 2a
generate_data <- function(n, p){
  covariates <- rnorm(n = n*p)
  covariates <- matrix(covariates, nrow=n, ncol=p)
  responses <- rnorm(n=n)
  return(list(covariates=covariates,
              responses=responses))
}