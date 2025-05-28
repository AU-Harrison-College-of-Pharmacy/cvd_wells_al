# get_p_dir_rope_table() function
# input: lists of full and red inla results for 4 conditions
# output: a tibble with condition, p_dir, and ROPE columns

get_p_dir_rope_table <- function(f_hypertensive, 
                                 f_ischemic, 
                                 f_stroke_cerebrovascular, 
                                 f_diabetes, parameter_name){
  
  conditions <- list(Hypertension = f_hypertensive, 
                     Ischemic = f_ischemic, 
                     Stroke_cerebrovascular = f_stroke_cerebrovascular, 
                     Diabetes = f_diabetes)
  
  results <- lapply(names(conditions), function(cond) {
    model <- conditions[[cond]]
    posterior_samples <- sample_posterior_parameter(model)
    
    p_dir <- p_direction(posterior_samples[[parameter_name]])$pd
    rope_val <- rope(posterior_samples[[parameter_name]])$ROPE_Percentage
    
    tibble(Condition = cond, p_dir = p_dir, ROPE = rope_val)
  })
  
  output <- bind_rows(results)
  return(output)
}
