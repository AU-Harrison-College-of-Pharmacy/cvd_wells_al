# get_waic_table() function creates waic table data tibble
# input: lists of waic vectors of full and red inla results for 4 conditions
# output: a tibble with condition, each (index), waic_full, waic_red, waic_diff columns

get_waic_table <- function(
    waic_full_list, waic_red_list) {
  
  
  conditions <- c("Hypertensive", "Ischemic", "Stroke cerebrovascular", "Diabetes")
  
  
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