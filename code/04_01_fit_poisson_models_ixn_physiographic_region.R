library(tidyverse)
library(Amelia)
library(lme4)
library(sf)
library(INLA)
# Run this file in the Window remote desktop. 

# dataset with imputation
df_hypertensive <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#########
# Interaction with physiographic region
#########

# Hypertension

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_hypertensive)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_01_pool_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "K:/Projects/usgs_cvd_wells_al/output/04_01_preds_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

# Ischemic

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_ischemic)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_01_pool_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "K:/Projects/usgs_cvd_wells_al/output/04_01_preds_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_stroke_cerebrovascular)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_01_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "K:/Projects/usgs_cvd_wells_al/output/04_01_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

# Diabetes

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_diabetes)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/04_01_pool_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "K:/Projects/usgs_cvd_wells_al/output/04_01_preds_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")