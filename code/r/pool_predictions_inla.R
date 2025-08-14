# pool_predictions_inla() function
# input: a list of inla results for multiple imputation datasets; terms; condition
# output: prediction summary to plot

pool_predictions_inla <- function(f_condition_inla, terms, condition = list()) {
  
  source("r/generate_newdata.R")
  
  parsed_terms <- lapply(terms, function(term) {
    if (grepl("\\[", term)) sub(" .*", "", term) else term
  }) %>% unlist()
  
  # Build newdata once from the first imputation since the predictors are common.
  newdata_each <- generate_newdata(f_condition_inla[[1]], terms, condition)
  
  # Insert the process of predict_response_inla() to create model.matrix() since this process is common across imputations and don't have to repeat multiple times.
  
  # Build fixed-effects-only formula
  formula <- f_condition_inla[[1]]$.args$formula
  formula <- as.character(formula)[3] %>%
    stringr::str_remove_all("f\\([^\\)]+\\)\\s*\\+?\\s*") %>%
    stringr::str_remove_all("offset\\([^\\)]+\\)\\s*\\+?\\s*") %>%
    stringr::str_remove("^\\s*\\+\\s*") %>%
    stringr::str_remove("\\+\\s*\\)?\\s*$") %>%
    stringr::str_replace("\\+\\s*\\)$", ")") %>%   # guard against trailing '+ )'
    stringr::str_trim() %>% 
    paste("~", .) %>%
    as.formula()
  
  # Only for factor variables, Sync factor levels to the training data, kind of factor variable (orderd or not), and contrasts as well
  fit_data <- f_condition_inla[[1]]$.args$data
  common_vars <- intersect(names(newdata_each), names(fit_data))
  for (nm in common_vars) {
    if (is.factor(fit_data[[nm]])) {
      newdata_each[[nm]] <- factor(
        as.character(newdata_each[[nm]]),
        levels  = levels(fit_data[[nm]]),
        ordered = is.ordered(fit_data[[nm]])
      )
      # If training data stored explicit contrasts, copy them verbatim.
      ctr0 <- tryCatch(contrasts(fit_data[[nm]]), error = function(e) NULL)
      if (!is.null(ctr0)) contrasts(newdata_each[[nm]]) <- ctr0
    }
  }
  
  # Detect how the inla fit encoded cat_age_group and match it in newdata
  if ("cat_age_group" %in% names(newdata_each)) {
    coef_names <- names(f_condition_inla[[1]]$marginals.fixed)
    
    # Check if the inla fit has polynomial coding. (coef_names like 'cat_age_group.L', '.Q', '.C')
    used_poly <- any(grepl("^cat_age_group\\.[LQC]$", coef_names))
    
    # Check if the inla fit use treatment coding with level labels.
    # (names like 'cat_age_group55 - 64 yrs' for MAIN effects; exclude interactions)
    used_treat_labels <- any(grepl("^cat_age_group", coef_names) &
                               !grepl("\\.",  coef_names) &
                               !grepl(":",   coef_names))
    
    lvl <- levels(newdata_each$cat_age_group)
    
    if (used_poly) {
      # Ensure ordered and set polynomial contrasts so names match (.L/.Q/.C)
      newdata_each$cat_age_group <- factor(newdata_each$cat_age_group,
                                           levels = lvl, ordered = TRUE)
      contrasts(newdata_each$cat_age_group) <- stats::contr.poly(length(lvl))
      
    } else if (used_treat_labels) {
      # the baseline as the level that has NO main-effect coefficient
      age_main <- coef_names[grepl("^cat_age_group", coef_names) &
                               !grepl("\\.", coef_names) &
                               !grepl(":",  coef_names)]
      age_coef_levels <- trimws(gsub("^cat_age_group", "", age_main))
      base_label <- setdiff(lvl, age_coef_levels)
      base_idx <- if (length(base_label) == 1) match(base_label, lvl) else 1
      
      # Treatment contrasts with *named* columns = level labels (match INLA names)
      # Create a contrast 4x3 matrix but this case it should have zeros at the first rows with lower 3x3 identity matrix  
      ctr <- stats::contr.treatment(length(lvl), base = base_idx)
      colnames(ctr) <- lvl[-base_idx]  # Assign column names for contrast matrix
      # Make unordered to avoid contr.poly defaults
      newdata_each$cat_age_group <- factor(as.character(newdata_each$cat_age_group),
                                           levels = lvl, ordered = FALSE)
      contrasts(newdata_each$cat_age_group) <- ctr
      
    } else {
      # Fallback: leave as-is (e.g., if cat_age_group not in fixed effects at all)
      # No action needed.
    }
  }
  
  offset_value <- if (length(condition)) condition[[names(condition)]] else NULL
  
  # Design matrix
  X <- model.matrix(formula, data = newdata_each)
  
  # Draw betas, align to X, and get mu = exp(X %*% beta)
  mats <- lapply(f_condition_inla, function(fit) {
    param_names <- names(fit$marginals.fixed)
    draws_df <- lapply(param_names, function(par) {
      inla.rmarginal(1000, fit$marginals.fixed[[par]])
    }) %>%
      stats::setNames(param_names) %>%
      dplyr::bind_cols()
    
    draws_mat <- as.matrix(draws_df)
    colnames(draws_mat) <- param_names
    
    common <- intersect(colnames(X), colnames(draws_mat))
    if (length(common) == 0L)
      stop("No overlap between model.matrix columns and fixed-effect names. Check factor levels/contrasts.")
    
    X_use     <- X[, common, drop = FALSE]
    betas_pxd <- t(draws_mat[, common, drop = FALSE])
    
    eta <- X_use %*% betas_pxd
    mu  <- exp(eta)
    if (!is.null(offset_value)) mu <- mu * as.numeric(offset_value)
    mu
  })
  
  y_hat_stacked <- do.call(cbind, mats)
  
  out <- newdata_each %>%
    dplyr::select(dplyr::all_of(parsed_terms)) %>%
    dplyr::mutate(
      predicted = rowMeans(y_hat_stacked),
      conf.low  = apply(y_hat_stacked, 1, quantile, probs = 0.025, names = FALSE),
      conf.high = apply(y_hat_stacked, 1, quantile, probs = 0.975, names = FALSE)
    )
  
  return(out)
}

