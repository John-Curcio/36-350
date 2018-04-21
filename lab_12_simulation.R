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
  p_val_colname <- "Pr(>|t|)"
  mod <- lm(responses ~ covariates)
  p_vals <- summary(mod)$coefficients[, p_val_colname]
  sig_inds <- which(p_vals <= cutoff)
  if(length(sig_inds) > 0){
    sig_mod <- lm(responses ~ covariates[,sig_inds])
    sig_p_vals <- summary(sig_mod)$coefficients[, p_val_colname]
    return(sig_p_vals)
  } else{
    return(c())
  }
}

# 2c
# run_simulation <- function(n_trials, n, p, cutoff){
#   datasets <- lapply(1:n_trials, 
#                      function(i){
#                        data <- generate_data(n, p)
#                        
#                        })
#   
# }

