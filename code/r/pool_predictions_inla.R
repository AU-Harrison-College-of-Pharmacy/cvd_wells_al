# pool_predictions_inla() function
# input: a list of inla results for multiple imputation datasets; terms; condition
# output: prediction summary to plot

pool_predictions_inla <- function(f_condition_inla, terms, condition = list()) {
  
  source("r/predict_response_inla.R")
  source("r/generate_newdata.R")
  
  parsed_terms <- lapply(terms, function(term) {
    if (grepl("\\[", term)) sub(" .*", "", term) else term
  }) %>% unlist()
  
  # Collect numeric draw matrices from each imputation
  mats <- lapply(f_condition_inla, function(f_condition_inla_each) {
    yh <- predict_response_inla(
      f_condition_inla_each,
      terms = terms,
      condition = condition
    )$y_hat
    
    k <- length(parsed_terms)                      
    if (ncol(yh) <= k) stop("No draw columns found in y_hat.")
    as.matrix(yh[, -(seq_len(k)), drop = FALSE]) 
  })
  
  y_hat_stacked <- do.call(cbind, mats)
  
  newdata_each <- generate_newdata(f_condition_inla[[1]], terms, condition)
  
  out <- newdata_each %>%
    dplyr::select(dplyr::all_of(parsed_terms)) %>%
    dplyr::mutate(
      predicted = rowMeans(y_hat_stacked),
      conf.low  = apply(y_hat_stacked, 1, quantile, probs = 0.025, names = FALSE),
      conf.high = apply(y_hat_stacked, 1, quantile, probs = 0.975, names = FALSE)
    )
  
  return(out)
}
