library(tidyverse)
library(Amelia)
library(lme4)
library(sf)
library(INLA)

source("r/pool_inla.R")
source("r/sample_posterior_parameter.R")

# Run this file in the Window remote desktop. 

# dataset with imputation
df_hypertensive <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#########
# Interaction with age group
#########

# Hypertension

f_hypertensive_ixn_age_group <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(config = TRUE),
    control.fixed = list(expand.factor.strategy = "inla") 
    #Error in inla.core(formula = formula, family = family, contrasts = contrasts,  : With control.fixed = list(expand.factor.strategy='model.matrix'), then NA's in factor are not allowd. Please use strategy 'inla' instead.
  )
})

pool_inla(sample_posterior_parameter(f_hypertensive_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_03_pool_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive_ixn_age_group, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "K:/Projects/usgs_cvd_wells_al/output/04_03_preds_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

# Ischemic

f_ischemic_ixn_age_group <-  lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") +  amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

pool_inla(sample_posterior_parameter(f_ischemic_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_03_pool_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic_ixn_age_group, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "K:/Projects/usgs_cvd_wells_al/output/04_03_preds_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_ixn_age_group <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

pool_inla(sample_posterior_parameter(f_stroke_cerebrovascular_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_03_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular_ixn_age_group, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "K:/Projects/usgs_cvd_wells_al/output/04_03_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

# Diabetes

f_diabetes_ixn_age_group <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") +  amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

pool_inla(sample_posterior_parameter(f_diabetes_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_03_pool_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes_ixn_age_group, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "K:/Projects/usgs_cvd_wells_al/output/04_03_preds_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")





