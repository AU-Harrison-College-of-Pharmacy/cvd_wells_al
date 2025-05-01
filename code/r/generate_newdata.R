# generate_newdata() function 
# generate new dataset to predict response with mean_reference as in ggeffects::predict_response()
# input: an inla output for one imputation dataset, terms, condition
# output: newdata as tibble format  

generate_newdata <- function(f_condition_inla_each, terms, condition = list()) {
  
  data <- f_condition_inla_each$.args$data %>%
    mutate(id_census_block_group = as.factor(id_census_block_group))
  
  vars_in_model <- all.vars(f_condition_inla_each$.args$formula)
  
  parsed_terms <- lapply(terms, function(term) {
    if (grepl("\\[", term)) {
      var <- sub(" .*", "", term)
      range_str <- sub(".*\\[|\\]", "", term) %>% sub("\\]", "", .) 
      range_vals <- as.numeric(strsplit(range_str, ":")[[1]])
      values = seq(range_vals[1], range_vals[2], by = 1)
      tibble(!!var := values)
    } else {
      var <- term
      values <- sort(unique(data[[var]]))
      tibble(!!var := values)
    }
  }
  )
  
  grid <- expand_grid(!!!parsed_terms)
  for (nm in names(condition)) {
    val <- condition[[nm]]
    grid[[nm]] <- val
  }

  for (v in vars_in_model) {
    if (!v %in% names(grid)) {
      # Use reference level for factors; mean for numeric
      if (is.factor(data[[v]]) || is.character(data[[v]])) {
        grid[[v]] <- factor(rep(levels(data[[v]])[1], nrow(grid)), levels = levels(data[[v]]))
      } else if (is.ordered(data[[v]])) {
        grid[[v]] <- ordered(rep(levels(data[[v]])[1], nrow(grid)), levels = levels(data[[v]]))
      } else if (is.numeric(data[[v]])) {
        grid[[v]] <- mean(data[[v]], na.rm = TRUE)
      }
    }
  }
  
  grid[["id_census_block_group"]] <- 0
  
  return(grid)
}


