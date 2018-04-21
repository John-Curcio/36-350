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
run_simulation <- function(n_trials, n, p, cutoff, datapath="p_vals.RData"){
  p_vals <- c()
  for(i in 1:n_trials){
    print(i)
    data <- generate_data(n, p);
    curr_p_vals <- model_select(data$covariates,
                                data$responses,
                                cutoff)
    p_vals <- c(p_vals, curr_p_vals)
  }
  # Edit described in 2d
  save(p_vals, file=datapath)
  #hist(p_vals)
}

# 2d
make_plot <- function(datapath="p_vals.RData"){
  load(datapath)
  hist(p_vals)
}