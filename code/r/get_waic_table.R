# get_waic_table() function creates waic table data tibble
# input: waic vectors (output of extract_waic()) of full and red inla results for 4 conditions
# output: a tibble with condition, each (index), waic_full, waic_red, waic_diff columns

get_waic_table <- function(
    hypertensive_full, hypertensive_red, 
    ischemic_full, ischemic_red, 
    stroke_cerebrovascular_full, stroke_cerebrovascular_red, 
    diabetes_full, diabetes_red) {
  
  
  conditions <- c("hypertensive", "ischemic", "stroke_cerebrovascular", "diabetes")
  
  
  waic_full_list <- list(hypertensive_full, ischemic_full, stroke_cerebrovascular_full, diabetes_full)
  waic_red_list  <- list(hypertensive_red,  ischemic_red,  stroke_cerebrovascular_red,  diabetes_red)
  
  
  waic_full_df <- purrr::map2_dfr(waic_full_list, conditions, 
                                  ~ tibble(condition = .y, each = seq_along(.x), waic_full = .x)
  )
  
  waic_red_df <- purrr::map2_dfr(waic_red_list, conditions, 
                                 ~ tibble(condition = .y, each = seq_along(.x), waic_red = .x)
  )
  
  
  waic_table <- dplyr::left_join(waic_full_df, waic_red_df, by = c("condition", "each")) %>%
    mutate(waic_diff = waic_red - waic_full)
  
  return(waic_table)
}
