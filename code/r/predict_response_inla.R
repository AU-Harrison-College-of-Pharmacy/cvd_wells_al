# predict_response_inla() function
# input: a newdata and a set of estimated_parameters for an inla result for one imputation dataset; formula from inla output; terms
# output: a list with prediction summary and a matrix of predicted responses


predict_response_inla <- function(f_condition_inla_each, terms, condition = list()){
  
  source("r/generate_newdata.R")
  
  # estimated_parameters_each per each imputation data result
  estimated_parameters_each <- 
    lapply(param_names, function(par) {
      inla.rmarginal(1000, f_condition_inla_each$marginals.fixed[[par]])
    }) %>%
      setNames(names(f_condition_inla_each$marginals.fixed)) %>%
      bind_cols()
  # new data for prediction
  newdata_each <- generate_newdata(f_condition_inla_each, terms, condition)
  
  # set formula for prediction
  formula <- f_condition_inla_each$.args$formula
  formula <- as.character(formula)[3] %>%
    str_remove_all("f\\([^\\)]+\\)\\s*\\+?\\s*") %>%
    str_remove_all("offset\\([^\\)]+\\)\\s*\\+?\\s*") %>%
    str_remove("^\\s*\\+\\s*") %>%
    str_remove("\\+\\s*\\)?\\s*$") %>% 
    str_replace("\\)\\s*$", "") %>%     
    str_trim() %>%
    paste("~", .) %>%
    as.formula()
  
  # match contrasts for ordered factor variable 
  contrasts(newdata_each$cat_age_group) <- contr.poly(4)
  
  X <- model.matrix(formula, data = newdata_each)
  
  common_cols <- intersect(colnames(X), colnames(estimated_parameters_each))
  X <- X[, common_cols, drop = FALSE]
  estimated_parameters_each <- estimated_parameters_each[, common_cols, drop = FALSE]
  
  offset_value <- condition[[names(condition)]] 
  
  y_hat <- as.matrix(X) %*% t(as.matrix(estimated_parameters_each)) %>%
    exp(.)*offset_value
  
  parsed_terms <- lapply(terms, function(term) {
    if (grepl("\\[", term)) {
      var <- sub(" .*", "", term)
    } else {
      var <- term
    }
  }
  ) %>% unlist()
  
  out <- newdata_each %>% 
    dplyr::select(all_of(parsed_terms)) %>%
    mutate(predicted = rowMeans(y_hat),
           conf.low = apply(y_hat, 1, quantile, probs = 0.025),
           conf.high = apply(y_hat, 1, quantile, probs = 0.975))
  y_hat <- newdata_each %>% 
    dplyr::select(all_of(parsed_terms)) %>% 
    supressMessages(bind_cols(y_hat))
  
  return(list(pred_summary = out, y_hat = y_hat))
}


