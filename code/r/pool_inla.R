# pool_inla() function
# input: a list of inla output for a condition for multiple imputation datasets;
#       condition on waic
# output: a list of summary with term, estimate, std.error, conf.low, conf.high, AND stacked_marginals  
# Mimic mice::pool() function by combining parameter estimates across imputations

pool_inla <- function(f_condition_inla, waic = TRUE){
  
  source("r/sample_posterior_parameter.R")
  source("r/extract_waic.R")
  
  output <- list()
  
  stacked_marginals <- sample_posterior_parameter(f_condition_inla)
  
  output$stacked_marginals <- stacked_marginals
  
  output$summary <- stacked_marginals %>%
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
    mutate(term = names(stacked_marginals)) %>%
    relocate(term)
  
  if(waic == TRUE){
    output$waic <- extract_waic(f_condition_inla)
  }
  
  return(output)
}
