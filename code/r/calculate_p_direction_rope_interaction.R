# calculate_p_direction_rope_interaction() function
# input: stacked_marginal (output of sample_posterior_parameter()) from a list of inla results for multiple imputation datasets
# output: a tibble row vector with ROPE values for a condition
# only for physiographic region


calculate_p_direction_rope_interaction <- function(stacked_marginals, kind = c("p_direction", "rope"), range=c(-0.1, 0.1)){
  
  
  posterior <- stacked_marginals %>%
    mutate(
    eff_AVR = amt_centered_scaled_mean_pct_wells_cbg,
    eff_CP  = eff_AVR + `amt_centered_scaled_mean_pct_wells_cbg:cat_physiographic_regionCumberland Plateau`,
    eff_EGCP = eff_AVR + `amt_centered_scaled_mean_pct_wells_cbg:cat_physiographic_regionEast Gulf Coastal Plain`,
    eff_HR = eff_AVR + `amt_centered_scaled_mean_pct_wells_cbg:cat_physiographic_regionHighland Rim`,
    eff_PU = eff_AVR + `amt_centered_scaled_mean_pct_wells_cbg:cat_physiographic_regionPiedmont Upland`
  ) %>%
    mutate(
      d_CP_AVR = eff_CP - eff_AVR,
      d_EGCP_AVR = eff_EGCP - eff_AVR,
      d_HR_AVR = eff_HR - eff_AVR,
      d_PU_AVR = eff_PU - eff_AVR,
      d_EGCP_CP = eff_EGCP - eff_CP,
      d_HR_CP = eff_HR - eff_CP,
      d_PU_CP = eff_PU - eff_CP,
      d_HR_EGCP = eff_HR - eff_EGCP,
      d_PU_EGCP = eff_PU - eff_EGCP,
      d_PU_HR = eff_PU - eff_HR
    ) 
  if(kind == "rope"){
  output <- posterior %>%
    summarise(across(starts_with("d_"), ~rope(.x, range = range)$ROPE_Percentage))
  }else{
    
    output <- posterior %>%
      summarise(across(starts_with("d_"), ~p_direction(.) %>% pull()))
  }
  
  return(output)
}
