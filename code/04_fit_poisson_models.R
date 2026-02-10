library(tidyverse)
library(Amelia)
library(lme4)
library(sf)
library(INLA)

source("r/pool_inla.R")
source("r/sample_posterior_parameter.R")
source("r/pool_predictions_inla.R")
# Run this file in the Window remote desktop. 

# Primary analysis
# Sensitivity analysis: restricting to second largest block groups

#### 
# Check the covariate overlap of your models
####

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  dplyr::filter(is_included_in_analysis == 1)

p_overlap_wells_area <- df %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = Hmisc::cut2(amt_centered_scaled_mean_pct_wells_cbg, g = 4),
    cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4)
  ) %>%
  count(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells) %>%
  arrange(cat_centered_scaled_mean_pct_wells, cat_centered_scaled_area_land) %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = as.character(cat_centered_scaled_mean_pct_wells),
    cat_centered_scaled_area_land = as.character(cat_centered_scaled_area_land)
  ) %>%
  tidyr::complete(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells) %>%
  ggplot(aes(x = cat_centered_scaled_mean_pct_wells, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cat_centered_scaled_area_land, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = str_wrap("Percentage of households reliant on private wells (centered and scaled)", 40),
       title = str_wrap("Bar plot of quartiles of percentage of households reliant on private wells, by quartiles of land area", 55)
  )

ggsave("figs/04_overlap_wells_area.pdf", width = 5, height = 4, units = "in")

df %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = Hmisc::cut2(amt_centered_scaled_mean_pct_wells_cbg, g = 4),
    cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4),
    cat_pct_aa_only = Hmisc::cut2(amt_pct_aa_only, g = 4)
  ) %>%
  count(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells, cat_pct_aa_only) %>%
  arrange(cat_centered_scaled_mean_pct_wells, cat_centered_scaled_area_land, cat_pct_aa_only) %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = as.character(cat_centered_scaled_mean_pct_wells),
    cat_centered_scaled_area_land = as.character(cat_centered_scaled_area_land),
    cat_pct_aa_only = as.character(cat_pct_aa_only)
  ) %>%
  tidyr::complete(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells, cat_pct_aa_only) %>%
  ggplot(aes(x = cat_centered_scaled_mean_pct_wells, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(cat_pct_aa_only ~ cat_centered_scaled_area_land, scales = "free_y")

set.seed(4821)

# dataset with imputation
df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#########
# Primary analysis
#########

# Hypertension

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

pool_inla(f_hypertensive) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_hypertension_deaths_poisson_model_main_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                            condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_hypertension_deaths_poisson_model_inla.rds")

# Ischemic

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

pool_inla(f_ischemic) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_ischemic_deaths_poisson_model_main_inla.rds")


preds_ischemic <- pool_predictions_inla(f_ischemic, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_ischemic_deaths_poisson_model_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

pool_inla(f_stroke_cerebrovascular) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_stroke_cerebrovascular_deaths_poisson_model_main_inla.rds")


preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                      condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_stroke_cerebrovascular_deaths_poisson_model_inla.rds")


# Diabetes

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

pool_inla(f_diabetes) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_diabetes_deaths_poisson_model_main_inla.rds")


preds_diabetes <- pool_predictions_inla(f_diabetes, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_diabetes_deaths_poisson_model_inla.rds")

#########
# Sensitivity analysis: restricting to second largest block groups
#########

# Hypertension

f_hypertensive_sensitivity_area <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE, waic = TRUE)
  )
})

pool_inla(f_hypertensive_sensitivity_area) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_hypertension_deaths_poisson_model_sensitivity_areal_inla.rds")


preds_hypertensive_sensitivity_area <- pool_predictions_inla(f_hypertensive_sensitivity_area, 
                                                             terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                             condition = c(n_population_times_4 = 100000))

write_rds(preds_hypertensive_sensitivity_area, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

# Ischemic

f_ischemic_sensitivity_area <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE, waic = TRUE)
  )
})

pool_inla(f_ischemic_sensitivity_area) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_ischemic_deaths_poisson_model_sensitivity_areal_inla.rds")

preds_ischemic_sensitivity_area <- pool_predictions_inla(f_ischemic_sensitivity_area, 
                                                         terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                         condition = c(n_population_times_4 = 100000))

write_rds(preds_ischemic_sensitivity_area, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_sensitivity_area <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE, waic = TRUE)
  )
})

pool_inla(f_stroke_cerebrovascular_sensitivity_area) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_stroke_cerebrovascular_deaths_poisson_model_sensitivity_areal_inla.rds")

preds_stroke_cerebrovascular_sensitivity_area <- pool_predictions_inla(f_stroke_cerebrovascular_sensitivity_area, 
                                                                       terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                                       condition = c(n_population_times_4 = 100000))

write_rds(preds_stroke_cerebrovascular_sensitivity_area, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

# Diabetes

f_diabetes_sensitivity_area <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + cat_rural + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE, waic = TRUE)
  )
})

pool_inla(f_diabetes_sensitivity_area) %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_diabetes_deaths_poisson_model_sensitivity_areal_inla.rds")

preds_diabetes_sensitivity_area <- pool_predictions_inla(f_diabetes_sensitivity_area, 
                                                         terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                         condition = c(n_population_times_4 = 100000))

write_rds(preds_diabetes_sensitivity_area, "/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")