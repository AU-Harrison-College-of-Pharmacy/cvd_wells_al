library(tidyverse)
library(INLA)
library(bayestestR)
library(gt)

source("r/sample_posterior_parameter.R")
source("r/calculate_p_direction_rope_interaction.R")

# Primary analysis


df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")


f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})




f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})




f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})




f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})


bind_rows(
  sample_posterior_parameter(f_hypertensive)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
  mutate(Parameter = "Hypertension"),
  
  sample_posterior_parameter(f_ischemic)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
    mutate(Parameter = "Ischemic"),
  
  sample_posterior_parameter(f_stroke_cerebrovascular)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
    mutate(Parameter = "Stroke cerebrovascular"),
  
  sample_posterior_parameter(f_diabetes)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
  mutate(Parameter = "Diabetes")) %>%
  rename(Condition = Parameter)%>%
  left_join(
  bind_rows(
  sample_posterior_parameter(f_hypertensive)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage)  %>%
    mutate(Parameter = "Hypertension"),
  
  sample_posterior_parameter(f_ischemic)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage)  %>%
    mutate(Parameter = "Ischemic"),
  
  sample_posterior_parameter(f_stroke_cerebrovascular)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage)  %>%
    mutate(Parameter = "Stroke cerebrovascular"),
  
  sample_posterior_parameter(f_diabetes)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage)  %>%
    mutate(Parameter = "Diabetes")) %>%
  rename(Condition = Parameter), by = "Condition") %>%
  relocate(Condition)

# Sensitivity analyses restricting to second-largest quartile of block groups


f_hypertensive_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

f_ischemic_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

f_stroke_cerebrovascular_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

f_diabetes_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")

bind_rows(
  sample_posterior_parameter(f_hypertensive_sensitivity_area)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
    mutate(Parameter = "Hypertension"),
  
  sample_posterior_parameter(f_ischemic_sensitivity_area)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
    mutate(Parameter = "Ischemic"),
  
  sample_posterior_parameter(f_stroke_cerebrovascular_sensitivity_area)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
    mutate(Parameter = "Stroke cerebrovascular"),
  
  sample_posterior_parameter(f_diabetes_sensitivity_area)  %>%
    summarise(p_dir=p_direction(amt_centered_scaled_mean_pct_wells_cbg)$pd) %>%
    mutate(Parameter = "Diabetes")) %>%
  rename(Condition = Parameter) %>%
  relocate(Condition) %>%
  left_join(

  bind_rows(
  sample_posterior_parameter(f_hypertensive_sensitivity_area)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage) %>%
    mutate(Parameter = "Hypertension"),
  
  sample_posterior_parameter(f_ischemic_sensitivity_area)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage) %>%
    mutate(Parameter = "Ischemic"),
  
  sample_posterior_parameter(f_stroke_cerebrovascular_sensitivity_area)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage) %>%
    mutate(Parameter = "Stroke cerebrovascular"),
  
  sample_posterior_parameter(f_diabetes_sensitivity_area)  %>%
    summarise(ROPE=rope(amt_centered_scaled_mean_pct_wells_cbg)$ROPE_Percentage) %>%
    mutate(Parameter = "Diabetes")) %>%
  rename(Condition = Parameter) %>% 
  relocate(Condition), by = "Condition")



# Interaction with physiographic region

f_hypertensive_x <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

f_ischemic_x <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

f_stroke_cerebrovascular_x <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

f_diabetes_x <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")


## p_direction for pairwise comparison between levels of regions  
bind_rows(
  calculate_p_direction_rope_interaction(f_hypertensive_x, kind = "p_direction"),
  calculate_p_direction_rope_interaction(f_ischemic_x, kind = "p_direction"),
  calculate_p_direction_rope_interaction(f_stroke_cerebrovascular_x, kind = "p_direction"),
  calculate_p_direction_rope_interaction(f_diabetes_x, kind = "p_direction")
) %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition)

## rope for pairwise comparison between levels of regions  
bind_rows(
  calculate_p_direction_rope_interaction(f_hypertensive_x, kind = "rope"),
  calculate_p_direction_rope_interaction(f_ischemic_x, kind = "rope"),
  calculate_p_direction_rope_interaction(f_stroke_cerebrovascular_x, kind = "rope"),
  calculate_p_direction_rope_interaction(f_diabetes_x, kind = "rope")
) %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition)


## 
f_hypertensive_x %>% 
  sample_posterior_parameter() %>% select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(mean(rope_each))
  
f_ischemic_x %>% 
  sample_posterior_parameter() %>% select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(mean(rope_each))



## Information criteria to compare Bayesian models
#### https://www.paulamoraga.com/book-geospatial/sec-inla.html 

f_hypertensive_full <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(dic = TRUE, waic = TRUE)
  )
})

library(purrr)
imputations_long <- tibble(imputation = seq(1:24)) %>%
  group_by(imputation) %>%
  nest() %>%
  mutate(
    imputations = map(imputation, ~df_hypertensive$imputations[[.x]]),
    fit = map(imputations, ~inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
                                 data = .x,
                                 family = "poisson",
                                 control.compute = list(waic = TRUE),
                                 num.threads = 4))
  )

  bind_cols(., imputations = df_hypertensive$imputations)
df_hypertensive$imputations %>%
  map_df(~as_tibble(.x))

f_ischemic_full <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
})

f_stroke_cerebrovascular_full <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
})

f_diabetes_full <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg * cat_physiographic_region + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
})

f_hypertensive_red <- lapply(1:length(df_hypertensive$imputations), function(i){
  i <- i+1
  f <- inla(
    n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
    data = df_hypertensive$imputations[[i]],
    family = "poisson",
    control.compute = list(dic = TRUE, waic = TRUE)
  )
})

f_ischemic_red <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
})

f_stroke_cerebrovascular_red <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
})

f_diabetes_red <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~  f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
})

