# extract_waic() function
# input: a list of inla results for multiple imputation datasets
# output: a vector of waic values from the inla results for all imputation



extract_waic <- function(f_condition_inla) {
  sapply(seq_along(f_condition_inla), function(i) f_condition_inla[[i]]$waic$waic)
}

