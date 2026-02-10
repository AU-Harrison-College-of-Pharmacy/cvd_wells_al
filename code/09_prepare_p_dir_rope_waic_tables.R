library(tidyverse)
library(INLA)
library(bayestestR)
library(gt)
library(gtsummary)

source("r/sample_posterior_parameter.R")
source("r/calculate_p_direction_rope_interaction.R")
source("r/get_p_dir_rope_table.R")
source("r/get_waic_table.R")
source("r/calculate_pairwise_p_direction_rope_interaction.R")

# Run this file in the window remote desktop.
#####
# This file has p_direction, rope, or waic for main, sensitivity, interaction with cat_physiographic_region, and interaction with amt_centered_scaled_pct_aa_only
# Each of main analysis is used as reduced model to calculate waic.
#####

# read dataset with imputation

df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

#####
# Primary analysis and sensitivity anlaysis
##### 

  # Primary
files_main <- c(
  Hypertension          = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_hypertension_deaths_poisson_model_main_inla.rds",
  Ischemic              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_ischemic_deaths_poisson_model_main_inla.rds",
  `Stroke cerebrovascular` = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_stroke_cerebrovascular_deaths_poisson_model_main_inla.rds",
  Diabetes              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_diabetes_deaths_poisson_model_main_inla.rds"
)

samples <- files_main %>%
  map(read_rds) %>%
  map(function(x) {
    x$stacked_marginals
  })

waics_main <- files_main %>%
  map(read_rds) %>%
  map(function(x) {
    x$waic
  })

get_p_dir_rope_table(samples, "amt_centered_scaled_mean_pct_wells_cbg") %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_main_pct_wells_cbg_inla.rds")




  # Sensitivity analyses restricting to second-largest quartile of block groups

files <- c(
  Hypertension          = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_hypertension_deaths_poisson_model_sensitivity_areal_inla.rds",
  Ischemic              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_ischemic_deaths_poisson_model_sensitivity_areal_inla.rds",
  `Stroke cerebrovascular` = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_stroke_cerebrovascular_deaths_poisson_model_sensitivity_areal_inla.rds",
  Diabetes              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_pool_diabetes_deaths_poisson_model_sensitivity_areal_inla.rds"
)

samples <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$stacked_marginals
  })

get_p_dir_rope_table(samples, "amt_centered_scaled_mean_pct_wells_cbg") %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_sensitivity_areal_pct_wells_cbg_inla.rds")

#############
# Interaction with physiographic region
#############


files <- c(
  Hypertension          = "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds",
  Ischemic              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds",
  `Stroke cerebrovascular` = "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds",
  Diabetes              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_01_pool_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds"
)

samples <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$stacked_marginals
  })

waics <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$waic
  })

pdir_tbl <- calculate_pairwise_p_direction_rope_interaction(samples, "p_direction")
rope_tbl <- calculate_pairwise_p_direction_rope_interaction(samples, "rope")

## p_direction for pairwise comparison between levels of regions  
pdir_tbl %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_pairwise_p_direction_ixn_physiographic_region_inla.rds")

## rope for pairwise comparison between levels of regions  
rope_tbl %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_pairwise_rope_ixn_physiographic_region_inla.rds")

## rope summary for regions
rope_summary_region <- bind_rows(samples[[1]] %>% 
   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)) ,
  
samples[[2]] %>% 
 select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)),

samples[[3]] %>% 
   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)),

samples[[4]] %>% 
   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
  mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
  summarise(ROPE = mean(rope_each)))  %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition)

write_rds(rope_summary_region, "/Volumes/Projects/usgs_cvd_wells_al/output/09_rope_ixn_physiographic_region_inla.rds")

## Information criteria to compare Bayesian models


### waic for physiographic region
waic_table <- get_waic_table(waics, waics_main)  

waic_table %>%
  select(-each) %>%
  tbl_summary(
    by = condition,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ) 
  )
  
write_rds(waic_table, "/Volumes/Projects/usgs_cvd_wells_al/output/09_waic_ixn_physiographic_region_inla.rds")  
  
###########
# Interaction with amt_centered_scaled_pct_aa_only 
###########

files <- c(
  Hypertension          = "/Volumes/Projects/usgs_cvd_wells_al/output/04_02_pool_hypertension_deaths_poisson_model_ixn_pct_aa_inla.rds",
  Ischemic              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_02_pool_ischemic_deaths_poisson_model_ixn_pct_aa_inla.rds",
  `Stroke cerebrovascular` = "/Volumes/Projects/usgs_cvd_wells_al/output/04_02_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa_inla.rds",
  Diabetes              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_02_pool_diabetes_deaths_poisson_model_ixn_pct_aa_inla.rds"
)

samples <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$stacked_marginals
  })

waics <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$waic
  })

  # p_direction and ROPE
## check for interaction term
get_p_dir_rope_table(samples, 
                     "amt_centered_scaled_mean_pct_wells_cbg:amt_centered_scaled_pct_aa_only") %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_ixn_pct_aa_only_inla.rds")

## check for amt_centered_scaled_pct_aa_only pct_aa_only
get_p_dir_rope_table(samples, 
                     "amt_centered_scaled_pct_aa_only") %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_pct_aa_only_inla.rds")

## check for amt_centered_scaled_pct_aa_only pct_aa_only
get_p_dir_rope_table(samples, 
                     "amt_centered_scaled_mean_pct_wells_cbg") %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_p_dir_rope_cbg_pct_aa_only_inla.rds")

  # waic for amt_centered_scaled_pct_aa_only
waic_table <- get_waic_table(waics, waics_main)

waic_table %>%
  select(-each) %>%
  tbl_summary(
    by = condition,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ) 
  ) 
waic_table %>%
  write_rds("/Volumes/Projects/usgs_cvd_wells_al/output/09_waic_ixn_pct_aa_only_inla.rds")  


############
# Interaction with cat_age_group
############
files <- c(
  Hypertension          = "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_pool_hypertension_deaths_poisson_model_ixn_age_group_inla.rds",
  Ischemic              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_pool_ischemic_deaths_poisson_model_ixn_age_group_inla.rds",
  `Stroke cerebrovascular` = "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_pool_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds",
  Diabetes              = "/Volumes/Projects/usgs_cvd_wells_al/output/04_03_pool_diabetes_deaths_poisson_model_ixn_age_group_inla.rds"
)

samples <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$stacked_marginals
  })

waics <- files %>%
  map(read_rds) %>%
  map(function(x) {
    x$waic
  })


## rope summary for age
rope_summary_age <- bind_rows(samples[[1]] %>% 
                                   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
                                   mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
                                   summarise(ROPE = mean(rope_each)) ,
                                 
                                 samples[[2]] %>% 
                                   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
                                   mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
                                   summarise(ROPE = mean(rope_each)),
                                 
                                 samples[[3]] %>% 
                                   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
                                   mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
                                   summarise(ROPE = mean(rope_each)),
                                 
                                 samples[[4]] %>% 
                                   select(contains("amt_centered_scaled_mean_pct_wells_cbg:")) %>% 
                                   mutate(rope_each = apply(., 1, function(row) all(row >= -0.1 & row <= 0.1))) %>%
                                   summarise(ROPE = mean(rope_each)))  %>% 
  mutate(Condition = c("Hypertension", "Ischemic", "Stroke cerebrovascular", "Diabetes")) %>%
  relocate(Condition)

write_rds(rope_summary_age, "/Volumes/Projects/usgs_cvd_wells_al/output/09_rope_ixn_age_group_inla.rds")

## Information criteria to compare Bayesian models


### waic for age_group
waic_table <- get_waic_table(waics, waics_main)  

waic_table %>%
  select(-each) %>%
  tbl_summary(
    by = condition,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ) 
  )

write_rds(waic_table, "/Volumes/Projects/usgs_cvd_wells_al/output/09_waic_ixn_age_group_inla.rds") 
