# pool_predictions_inla() function
# input: a list of inla results for multiple imputation datasets; terms; condition
# output: prediction summary to plot

pool_predictions_inla <- function(f_condition_inla, terms, condition = list()) {
  
  source("r/predict_response_inla.R")
  source("r/generate_newdata.R")
  y_hat_stacked <- lapply(f_condition_inla, function(f_condition_inla_each){
    
    predict_response_inla(f_condition_inla_each, terms = terms, condition = condition)$y_hat[, -c(1:2)]
  }) %>% bind_cols() %>%
    suppressMessages()
  
  # terms for prediction summary
  newdata_each <- generate_newdata(f_condition_inla[[1]], terms, condition)
  parsed_terms <- lapply(terms, function(term) {
    if (grepl("\\[", term)) {
      var <- sub(" .*", "", term)
    } else {
      var <- term
    }
  }
  ) %>% unlist()
  
  # output
  out <- newdata_each %>% 
    dplyr::select(all_of(parsed_terms)) %>%
    mutate(predicted = rowMeans(y_hat_stacked),
           conf.low = apply(y_hat_stacked, 1, quantile, probs = 0.025),
           conf.high = apply(y_hat_stacked, 1, quantile, probs = 0.975))

  return(out)
}
