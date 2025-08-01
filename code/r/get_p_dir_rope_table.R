# get_p_dir_rope_table() function
# input: stacked_marginals of inla results for 4 conditions
# output: a tibble with condition, p_dir, and ROPE columns

get_p_dir_rope_table <- function(sample_hypertensive, 
                                 sample_ischemic, 
                                 sample_stroke_cerebrovascular, 
                                 sample_diabetes, 
                                 parameter_name) {
  
  # Use already-sampled posterior parameter tibbles
  conditions <- list(
    Hypertension = samp_hypertensive, 
    Ischemic = samp_ischemic, 
    Stroke_cerebrovascular = samp_stroke_cerebrovascular, 
    Diabetes = samp_diabetes
  )
  
  results <- lapply(names(conditions), function(cond) {
    posterior_samples <- conditions[[cond]]
    
    # Get posterior samples for the specific parameter
    param_samples <- posterior_samples[[parameter_name]]
    
    # Compute p_direction and ROPE
    p_dir <- p_direction(param_samples)$pd
    rope_val <- rope(param_samples)$ROPE_Percentage
    
    tibble(
      Condition = cond,
      p_dir = p_dir,
      ROPE = rope_val
    )
  })
  
  bind_rows(results)
}