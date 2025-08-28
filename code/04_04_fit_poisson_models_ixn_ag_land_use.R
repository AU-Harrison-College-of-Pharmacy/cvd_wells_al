library(tidyverse)
library(Amelia)
library(lme4)
library(sf)

set.seed(2587)

# dataset with imputation
df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#########
# Interaction with agricultural land use
#########

# Hypertension

f_hypertensive_ixn_ag_land_use <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- glmer(
    n_hypertensive_deaths ~ (1 | id_census_block_group) + amt_centered_scaled_mean_pct_wells_cbg * amt_percent_agricultural_land_use_centered_scaled + cat_age_group + cat_rural + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control = glmerControl(optimizer = "bobyqa")) 
})

write_rds(f_hypertensive_ixn_ag_land_use, "/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_ixn_ag_land_use.rds")

# Ischemic

f_ischemic_ixn_ag_land_use <-  lapply(1:length(df_ischemic$imputations), function(i){
  f <- glmer(n_ischemic_deaths ~ (1 | id_census_block_group) +  amt_centered_scaled_mean_pct_wells_cbg * amt_percent_agricultural_land_use_centered_scaled + cat_age_group + cat_rural + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control = glmerControl(optimizer = "bobyqa")
  )
})

write_rds(f_ischemic_ixn_ag_land_use, "/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_ixn_ag_land_use.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_ixn_ag_land_use <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- glmer(n_stroke_cerebrovascular_deaths ~  (1 | id_census_block_group) + amt_centered_scaled_mean_pct_wells_cbg * amt_percent_agricultural_land_use_centered_scaled + cat_age_group + cat_rural + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control = glmerControl(optimizer = "bobyqa")
  )
})

write_rds(f_stroke_cerebrovascular_ixn_ag_land_use, "/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_ixn_ag_land_use.rds")

# Diabetes

f_diabetes_ixn_ag_land_use <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- glmer(n_diabetes_deaths ~  (1 | id_census_block_group) +  amt_centered_scaled_mean_pct_wells_cbg * amt_percent_agricultural_land_use_centered_scaled + cat_age_group + cat_rural + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control = glmerControl(optimizer = "bobyqa")
  )
})

write_rds(f_diabetes_ixn_ag_land_use, "/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_ixn_ag_land_use.rds")
