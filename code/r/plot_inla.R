# plot_inla() function
# input: output of pool_predictions_inla()
# output: ggplot object

plot_inla <- function(preds_condition_inla){
  
  factor_var <- names(preds_condition_inla)[sapply(preds_condition_inla, is.factor)]

    preds_condition_inla %>%
    ggplot(aes(x = amt_centered_scaled_mean_pct_wells_cbg, 
               y = predicted,
               color = .data[[factor_var]],
               fill = .data[[factor_var]])) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    theme_minimal() +
    theme(axis.line = element_line(color = "lightgray")) 

}
