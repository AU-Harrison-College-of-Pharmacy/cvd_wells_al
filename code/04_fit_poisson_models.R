library(tidyverse)
library(Amelia)
library(lme4)
library(sf)

#### 
# Check the covariate overlap of your models
####

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  dplyr::filter(is_included_in_analysis == 1)

df %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = Hmisc::cut2(amt_centered_scaled_mean_pct_wells_cbg, g = 4),
    cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4)
  ) %>%
  count(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells) %>%
  arrange(cat_centered_scaled_mean_pct_wells, cat_centered_scaled_area_land) %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = as_character(cat_centered_scaled_mean_pct_wells),
    cat_centered_scaled_area_land = as_character(cat_centered_scaled_area_land)
  ) %>%
  tidyr::complete(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells) %>%
  ggplot(aes(x = cat_centered_scaled_mean_pct_wells, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cat_centered_scaled_area_land, scales = "free_y")

df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- glmer(n_hypertensive_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_hypertensive$imputations[[i]],
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_hypertensive, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")

# Ischemic heart disease

df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- glmer(n_ischemic_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_ischemic$imputations[[i]],
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_ischemic, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model.rds")

# Stroke/cerebrovascular

df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- glmer(n_stroke_cerebrovascular_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_stroke_cerebrovascular$imputations[[i]],
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_stroke_cerebrovascular, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model.rds")

# Diabetes

df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- glmer(n_diabetes_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_diabetes$imputations[[i]],
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_diabetes, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model.rds")


#######
# Sensitivity analyses restricting to second-largest quartile of block groups
#######

df %>%
  mutate(cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4)
) %>%
  distinct(cat_centered_scaled_area_land)

# Hypertensive
f_hypertensive_sensitivity_area <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- glmer(n_hypertensive_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_hypertensive$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_hypertensive_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_sensitivity_area.rds")

# Ischemic
f_ischemic_sensitivity_area <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- glmer(n_ischemic_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_ischemic$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_ischemic_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_sensitivity_area.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_sensitivity_area <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- glmer(n_stroke_cerebrovascular_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_stroke_cerebrovascular$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_stroke_cerebrovascular_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area.rds")

# Diabetes

f_diabetes_sensitivity_area <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- glmer(n_diabetes_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_diabetes$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_diabetes_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_sensitivity_area.rds")
