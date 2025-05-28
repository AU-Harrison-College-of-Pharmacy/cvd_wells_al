# sample_posterior_parameter() function
# input: a list of inla results for multiple imputation datasets
# output: stacked marginals for parameters as tibble format


sample_posterior_parameter <- function(f_condition_inla){
  
  param_names <- names(f_condition_inla[[1]]$marginals.fixed)
  
  stacked_marginals <- lapply(f_condition_inla, function(mod) {
    lapply(param_names, function(par) {
      inla.rmarginal(1000, mod$marginals.fixed[[par]])
    }) %>%
      setNames(param_names) %>%
      bind_cols()
  }) %>%
    bind_rows()
  
  return(stacked_marginals)
}
