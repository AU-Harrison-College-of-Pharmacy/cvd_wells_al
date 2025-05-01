# convert_estimates_from_polynomial_to_original_levels() function
# input: a result for one inla or glmer with an ordered (4 leveled) factor variable, cat_age_group; coefficient estimates with *L, *.Q, and *.C. 
# output: coefficient estimates with original levels


convert_estimates_from_polynomial_to_original_levels <- function(f_result, coef_estimates_poly_contrasts, inla = TRUE){
  if(inla == TRUE){
  #contrast matrix
  C <- f_result$.args$data$cat_age_group %>% 
    levels() %>%
    contr.poly()
  
  coef_matrix <- coef_estimates_poly_contrasts %>% select(contains(".L"), contains(".Q"), contains(".C")) %>% 
    as.matrix()
    
  original_level_estimates <- coef_matrix %*% t(C)
  colnames(original_level_estimates) <- paste0("cat_age_group", levels(f_hypertensive_inla[[1]]$.args$data$cat_age_group))
  
  }else{
  C  <- f_result@frame$cat_age_group %>% 
    levels() %>%
    contr.poly()
  
  coef_matrix <- coef_estimates_poly_contrasts %>% select(contains(".L"), contains(".Q"), contains(".C")) %>% 
    as.matrix()
  
  original_level_estimates <- coef_matrix %*% t(C)
  colnames(original_level_estimates) <- paste0("cat_age_group", levels(f_result@frame$cat_age_group))
  
  }
  
  return(original_level_estimates %>% as_tibble())
}
