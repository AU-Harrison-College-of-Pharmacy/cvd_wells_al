library(tidyverse)
library(INLA)
library(bayestestR)
library(gt)

source("r/sample_posterior_parameter.R")
source("r/calculate_p_direction_rope_interaction.R")
source("r/get_p_dir_rope_table.R")
source("r/get_waic_table.R")

# Run this file in the window remote desktop.
#####
# This file has p_direction, rope, or waic for main, sensitivity, interaction with cat_physiographic_region, and interaction with amt_centered_scaled_pct_aa_only
# Each of main analysis is used as reduced model to calculate waic.
#####

# read dataset with imputation

df_hypertensive <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("K:/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

# Primary analysis

f_hypertensive_main <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

f_ischemic_main <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

f_stroke_cerebrovascular_main <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

f_diabetes_main <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

get_p_dir_rope_table(f_hypertensive_main, 
                     f_ischemic_main, 
                     f_stroke_cerebrovascular_main,  
                     f_diabetes_main, 
                     "amt_centered_scaled_mean_pct_wells_cbg") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_main_pct_wells_cbg_inla.rds")



# Sensitivity analyses restricting to second-largest quartile of block groups

f_hypertensive_sensitivity_area <- read_rds("K:/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

f_ischemic_sensitivity_area <- read_rds("K:/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

f_stroke_cerebrovascular_sensitivity_area <- read_rds("K:/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

f_diabetes_sensitivity_area <- read_rds("K:/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")

get_p_dir_rope_table(f_hypertensive_sensitivity_area, 
                     f_ischemic_sensitivity_area, 
                     f_stroke_cerebrovascular_sensitivity_area,  
                     f_diabetes_sensitivity_area, 
                     "amt_centered_scaled_mean_pct_wells_cbg") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_sensitivity_area_pct_wells_cbg_inla.rds")


#############
# Interaction with physiographic region
#############

f_hypertensive_full <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(dic = TRUE, waic = TRUE)
  )
})

f_ischemic_full <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE)
  )
})

f_stroke_cerebrovascular_full <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE)
  )
})

f_diabetes_full <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE)
  )
})


## p_direction for pairwise comparison between levels of regions  
bind_rows(
  calculate_p_direction_rope_interaction(f_hypertensive_full, kind = "p_direction"),
  calculate_p_direction_rope_interaction(f_ischemic_full, kind = "p_direction"),
  calculate_p_direction_rope_interaction(f_stroke_cerebrovascular_full, kind = "p_direction"),
  calculate_p_direction_rope_interaction(f_diabetes_full, kind = "p_direction")
) %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition) %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_pairwise_p_direction_ixn_physiographic_region_inla.rds")


## rope for pairwise comparison between levels of regions  
bind_rows(
  calculate_p_direction_rope_interaction(f_hypertensive_full, kind = "rope"),
  calculate_p_direction_rope_interaction(f_ischemic_full, kind = "rope"),
  calculate_p_direction_rope_interaction(f_stroke_cerebrovascular_full, kind = "rope"),
  calculate_p_direction_rope_interaction(f_diabetes_full, kind = "rope")
) %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition) %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_pairwise_rope_ixn_physiographic_region_inla.rds")



## rope summary for regions
rope_summary_region <- bind_rows(f_hypertensive_full %>% 
  sample_posterior_parameter() %>% select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)) ,
  
f_ischemic_full %>% 
  sample_posterior_parameter() %>% select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)),

f_stroke_cerebrovascular_full %>% 
  sample_posterior_parameter() %>% select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)),

f_diabetes_full %>% 
  sample_posterior_parameter() %>% select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)))  %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition)

write_rds(rope_summary_region, "K:/Projects/usgs_cvd_wells_al/output/09_rope_ixn_physiographic_region_inla.rds")

## Information criteria to compare Bayesian models


### waic for physiographic region
waic_table <- get_waic_table(
  f_hypertensive_full, f_hypertensive_red =f_hypertensive_main, 
  f_ischemic_full, f_ischemic_red = f_ischemic_main, 
  f_stroke_cerebrovascular_full, f_stroke_cerebrovascular_red = f_stroke_cerebrovascular_main, 
  f_diabetes_full, f_diabetes_red = f_diabetes_main
)  

waic_table %>%
  select(-each) %>%
  tbl_summary(
    by = condition,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ) 
  )
  
write_rds(waic_table, "K:/Projects/usgs_cvd_wells_al/output/09_waic_ixn_physiographic_region_inla.rds")  
  

# Interaction with amt_centered_scaled_pct_aa_only 
## p_direction and ROPE
f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + 
      amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + 
      amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(config = TRUE, waic=TRUE)
  )
})

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + 
              amt_centered_scaled_mean_pct_wells_cbg * amt_centered_scaled_pct_aa_only + 
              amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, waic=TRUE)
  )
})

## check for interaction term
get_p_dir_rope_table(f_hypertensive, 
                     f_ischemic, 
                     f_stroke_cerebrovascular,  
                     f_diabetes, 
                     "amt_centered_scaled_mean_pct_wells_cbg:amt_centered_scaled_pct_aa_only") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_ixn_pct_aa_only_inla.rds")

## check for amt_centered_scaled_pct_aa_only pct_aa_only
get_p_dir_rope_table(f_hypertensive, 
                     f_ischemic, 
                     f_stroke_cerebrovascular,  
                     f_diabetes, 
                     "amt_centered_scaled_pct_aa_only") %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_pct_aa_only_inla.rds")

## waic for amt_centered_scaled_pct_aa_only
waic_table_aa <- get_waic_table(
  f_hypertensive, f_hypertensive_main, 
  f_ischemic, f_ischemic_main, 
  f_stroke_cerebrovascular, f_stroke_cerebrovascular_main, 
  f_diabetes, f_diabetes_main
) 

waic_table_aa %>%
  select(-each) %>%
  tbl_summary(
    by = condition,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ) 
  ) 
waic_table_aa %>%
  write_rds("K:/Projects/usgs_cvd_wells_al/output/09_waic_ixn_pct_aa_only_inla.rds")  
