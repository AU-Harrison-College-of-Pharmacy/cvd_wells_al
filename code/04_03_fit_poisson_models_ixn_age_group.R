library(tidyverse)
library(Amelia)
library(lme4)
library(sf)
library(INLA)
#### 
# Check the covariate overlap of your models
####

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  dplyr::filter(is_included_in_analysis == 1)

df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(config = TRUE),
    control.fixed = list(expand.factor.strategy = "inla") 
    #Error in inla.core(formula = formula, family = family, contrasts = contrasts,  : With control.fixed = list(expand.factor.strategy='model.matrix'), then NA's in factor are not allowd. Please use strategy 'inla' instead.
  )
})

write_rds(f_hypertensive, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

# Ischemic heart disease

df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

write_rds(f_ischemic, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

# Stroke/cerebrovascular

df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

write_rds(f_stroke_cerebrovascular, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

# Diabetes

df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

write_rds(f_diabetes, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")