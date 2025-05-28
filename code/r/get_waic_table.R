# get_waic_table() function creates waic table data tibble
# input: lists of full and red inla results for 4 conditions
# output: a tibble with condition, each (index), waic_full, waic_red, waic_diff columns

get_waic_table <- function(
    f_hypertensive_full, f_hypertensive_red, 
    f_ischemic_full, f_ischemic_red, 
    f_stroke_cerebrovascular_full, f_stroke_cerebrovascular_red, 
    f_diabetes_full, f_diabetes_red){
  
  conditions <- c("hypertensive", "ischemic", "stroke_cerebrovascular", "diabetes")
  
  extract_waic <- function(object_name) {
    obj <- get(object_name)
    sapply(seq_along(obj), function(i) obj[[i]]$waic$waic)
  }
  
  
  
  waic_full <- lapply(conditions, function(condition) extract_waic(paste0("f_", condition, "_full")))
  waic_red <- lapply(conditions, function(condition) extract_waic(paste0("f_", condition, "_red")))
  
  
  waic_full <- do.call(rbind, lapply(seq_along(conditions), function(i) {
    tibble(
      condition = conditions[i],
      each = seq_along(waic_full[[i]]),
      waic_full = waic_full[[i]]
    )
  }))
  
  waic_red <- do.call(rbind, lapply(seq_along(conditions), function(i) {
    tibble(
      condition = conditions[i],
      each = seq_along(waic_red[[i]]),
      waic_red = waic_red[[i]]
    )
  }))
  
  waic_table <- left_join(waic_full, waic_red, by = c("condition", "each")) %>%
    mutate(waic_diff = waic_red - waic_full)
  
  return(waic_table)
}
