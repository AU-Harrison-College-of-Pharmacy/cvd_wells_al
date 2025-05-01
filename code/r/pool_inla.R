# pool_inla() function
# input: a list of inla results for multiple imputation datasets
# output: term, estimate, std.error, conf.low, conf.high  
# Mimic mice::pool() function by combining parameter estimates across imputations

pool_inla <- function(f_condition_inla){
  
  param_names <- names(f_condition_inla[[1]]$marginals.fixed)
  
  stacked_marginals <- lapply(f_condition_inla, function(mod) {
    lapply(param_names, function(par) {
      inla.rmarginal(1000, mod$marginals.fixed[[par]])
    }) %>%
      setNames(param_names) %>%
      bind_cols()
  })
  
  output <- stacked_marginals %>%
    bind_rows() %>%
    summarise(
      across(everything(), 
             list(
               estimate = ~ exp(mean(.)),
               std.error = ~ sd(.),
               conf.low = ~ exp(quantile(., 0.025)),
               conf.high = ~ exp(quantile(., 0.975))
               ),
             .names = "{.col}_{.fn}"
             )
    ) %>%
    t() %>% 
    matrix(ncol=4, byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    `colnames<-`(c("estimate", "std.error", "conf.low", "conf.high")) %>%
    mutate(term = param_names) %>%
    select(term, estimate, std.error, conf.low, conf.high)
  
  
  return(list(output= output, estimated_parameters = stacked_marginals))
}
