library(tidyverse)
library(lme4)
library(ggeffects)
library(patchwork)
library(mice)
#library(broom.mixed)
library(INLA)

source("r/sample_posterior_parameter.R")
source("r/pool_inla.R")
source("r/pool_predictions_inla.R")
source("r/plot_inla.R")

# Get the original well data so that we can show the results in terms of the original well percentages, not the centered and scaled well percentages
# Run this file in the Window remote desktop. 04 files have inla codes but it gives an error message for write_rds. Thus we run inla and use its output to get prediction dataset in 05 file.

# This 05 file includes 
## Primary analysis 
## Sensitivity analysis: restricting to second largest block groups
## Interaction with physiographic region
## Interaction with percent reporting AA race alone
## Interaction with age group




df <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

# dataset with imputation
df_hypertensive <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#########
# Primary analysis
#########

# Hypertension

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_hypertensive)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_hypertension_deaths_poisson_model_main_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "K:/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_inla.rds")

# Ischemic

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_ischemic)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_ischemic_deaths_poisson_model_main_inla.rds")


preds_ischemic <- pool_predictions_inla(f_ischemic, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "K:/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_stroke_cerebrovascular)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_stroke_cerebrovascular_deaths_poisson_model_main_inla.rds")


preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "K:/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_inla.rds")


# Diabetes

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_diabetes)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_diabetes_deaths_poisson_model_main_inla.rds")


preds_diabetes <- pool_predictions_inla(f_diabetes, 
                                             terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                             condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "K:/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_inla.rds")

#########
# Sensitivity analysis: restricting to second largest block groups
#########

# Hypertension

f_hypertensive_sensitivity_area <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_hypertensive_sensitivity_area)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_hypertension_deaths_poisson_model_sensitivity_areal_inla.rds")


preds_hypertensive_sensitivity_area <- pool_predictions_inla(f_hypertensive_sensitivity_area, 
                                                             terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                             condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive_sensitivity_area, "K:/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

# Ischemic

f_ischemic_sensitivity_area <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_ischemic_sensitivity_area)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_ischemic_deaths_poisson_model_sensitivity_areal_inla.rds")

preds_ischemic_sensitivity_area <- pool_predictions_inla(f_ischemic_sensitivity_area, 
                                                         terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                         condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic_sensitivity_area, "K:/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_sensitivity_area <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_stroke_cerebrovascular_sensitivity_area)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_stroke_cerebrovascular_deaths_poisson_model_sensitivity_areal_inla.rds")

preds_stroke_cerebrovascular_sensitivity_area <- pool_predictions_inla(f_stroke_cerebrovascular_sensitivity_area, 
                                                                       terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                                       condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular_sensitivity_area, "K:/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

# Diabetes

f_diabetes_sensitivity_area <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_diabetes_sensitivity_area)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_diabetes_deaths_poisson_model_sensitivity_areal_inla.rds")

preds_diabetes_sensitivity_area <- pool_predictions_inla(f_diabetes_sensitivity_area, 
                                                         terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                         condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes_sensitivity_area, "K:/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")


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
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "K:/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "K:/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "K:/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "K:/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")


#########
# Interaction with percent reporting AA race alone
#########

# Hypertension

f_hypertensive_ixn_pct_aa <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_hypertensive_ixn_pct_aa)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_hypertension_deaths_poisson_model_ixn_pct_aa_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive_ixn_pct_aa, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                             condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "K:/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_ixn_pct_aa_inla.rds")

# Ischemic

f_ischemic_ixn_pct_aa <-  lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_ischemic_ixn_pct_aa)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_ischemic_deaths_poisson_model_ixn_pct_aa_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic_ixn_pct_aa, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "K:/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_ixn_pct_aa_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_ixn_pct_aa <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_stroke_cerebrovascular_ixn_pct_aa)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular_ixn_pct_aa, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "K:/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa_inla.rds")

# Diabetes

f_diabetes_ixn_pct_aa <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

pool_inla(sample_posterior_parameter(f_diabetes_ixn_pct_aa)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_diabetes_deaths_poisson_model_ixn_pct_aa_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes_ixn_pct_aa, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "K:/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_ixn_pct_aa_inla.rds")

#########
# Interaction with age group
#########

# Hypertension

f_hypertensive_ixn_age_group <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
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
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive_ixn_age_group, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "K:/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

# Ischemic

f_ischemic_ixn_age_group <-  lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

pool_inla(sample_posterior_parameter(f_ischemic_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic_ixn_age_group, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "K:/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_ixn_age_group <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

pool_inla(sample_posterior_parameter(f_stroke_cerebrovascular_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular_ixn_age_group, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "K:/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

# Diabetes

f_diabetes_ixn_age_group <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_age_group + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE),
            control.fixed = list(expand.factor.strategy = "inla")
  )
})

pool_inla(sample_posterior_parameter(f_diabetes_ixn_age_group)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/05_pool_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes_ixn_age_group, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "K:/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")





