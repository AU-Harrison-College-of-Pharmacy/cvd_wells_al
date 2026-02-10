library(tidyverse)
library(Amelia)
library(lme4)
library(sf)
library(INLA)

source("r/pool_inla.R")
source("r/sample_posterior_parameter.R")
source("r/pool_predictions_inla.R")

# Run this file in the Window remote desktop. 

set.seed(1934)

# dataset with imputation
df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#########
# Interaction with physiographic region
#########

# Hypertension

f_hypertensive_ixn_physiographic_region <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + cat_rural + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(dic = TRUE, waic = TRUE)
  )
})

pool_inla(f_hypertensive_ixn_physiographic_region) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive_ixn_physiographic_region, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region", "cat_age_group"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_preds_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

# Ischemic

f_ischemic_physiographic_region <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + cat_rural + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE)
  )
})

pool_inla(f_ischemic_physiographic_region) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic_physiographic_region, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_preds_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_physiographic_region <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + cat_rural + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE)
  )
})

pool_inla(f_stroke_cerebrovascular_physiographic_region) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular_physiographic_region, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region", "cat_age_group"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

# Diabetes

f_diabetes_physiographic_region <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + cat_rural + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE)
  )
})

pool_inla(f_diabetes_physiographic_region) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes_physiographic_region, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_preds_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")