# 2a
generate_data <- function(n, p){
  covariates <- rnorm(n = n*p)
  covariates <- matrix(covariates, nrow=n, ncol=p)
  responses <- rnorm(n=n)
  return(list(covariates=covariates,
              responses=responses))
}

# 2b
model_select <- function(covariates, responses, cutoff){
  mod <- lm(responses ~ covariates)
  p_vals <- summary(mod)$coefficients[, "Pr(>|t|)"]
  sig_inds <- which(p_vals <= cutoff)
  if(length(sig_inds) > 0) return(lm(responses ~ covariates[,sig_inds]))
  
  return(lm(responses ~ 1))
}