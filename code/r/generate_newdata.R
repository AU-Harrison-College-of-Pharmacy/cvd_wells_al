# generate_newdata() function 
# generate new dataset to predict response with mean_reference as in ggeffects::predict_response()
# input: an inla output for one imputation dataset, terms, condition
# output: newdata as tibble format  
# Each imputation dataset has difference only at outcome (n_hypertensive_deaths, unused for prediction).
# Thus we don't have to repeat generate_newdata() to generate predictors for prediction for each imputation.
# So, We will run generate_newdata() once per each inla output for 24 imputations.

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
      tibble(!!sym(var) := values)
    } 
    else {
      var <- term
      if (is.factor(data[[var]]) || is.character(data[[var]])) {
        values <- sort(unique(data[[var]]))
        tibble(!!sym(var) := values)
      } else {
        values <- c(-1, 0, 1)
        tibble(!!sym(var) := values)
      }
    }
  })
  
  
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


