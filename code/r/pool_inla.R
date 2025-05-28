# pool_inla() function
# input: a stacked_marginal tibble from inla results for multiple imputation datasets
# output: term, estimate, std.error, conf.low, conf.high  
# Mimic mice::pool() function by combining parameter estimates across imputations

pool_inla <- function(stacked_marginals){
  
  output <- stacked_marginals %>%
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
 
  return(output)
}
