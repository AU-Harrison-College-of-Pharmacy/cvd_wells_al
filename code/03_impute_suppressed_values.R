library(tidyverse)
library(Amelia)

set.seed(876234)

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

df_hypertensive <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_hypertensive_deaths, cat_physiographic_region, amt_pct_aa_only)

df_ischemic <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4,
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_ischemic_deaths, cat_physiographic_region, amt_pct_aa_only)

df_stroke_cerebrovascular <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4,
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_stroke_cerebrovascular_deaths, cat_physiographic_region, amt_pct_aa_only)

df_diabetes <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4,
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_diabetes_deaths, cat_physiographic_region, amt_pct_aa_only)

# Impute datasets for each outcome
imputation_boundaries <- matrix(c(7, 1, 5),
                                nrow = 2,
                                ncol = 3,
                                byrow = TRUE)
    ## Hypertensive
    p_missing <- round((nrow(df_hypertensive) - nrow(drop_na(df_hypertensive))) / nrow(df_hypertensive) * 100, 0)
    a_hypertensive <- amelia(
                        as.data.frame(df_hypertensive),
                        m = p_missing,
                        idvars = c("id_census_block_group"),
                        ords = c("cat_age_group", "n_hypertensive_deaths"),
                        noms = c("cat_physiographic_region"),
                        bounds = imputation_boundaries
    )
    
    ## Ischemic
    p_missing <- round((nrow(df_ischemic) - nrow(drop_na(df_ischemic))) / nrow(df_ischemic) * 100, 0)
    a_ischemic <- amelia(
      as.data.frame(df_ischemic),
      m = p_missing,
      idvars = c("id_census_block_group"),
      ords = c("cat_age_group", "n_ischemic_deaths"),noms = c("cat_physiographic_region"),
      bounds = imputation_boundaries
    )
    
    ## Stroke/cerebrovascular
    p_missing <- round((nrow(df_stroke_cerebrovascular) - nrow(drop_na(df_stroke_cerebrovascular))) / nrow(df_stroke_cerebrovascular) * 100, 0)
    a_stroke_cerebrovascular <- amelia(
      as.data.frame(df_stroke_cerebrovascular),
      m = p_missing,
      idvars = c("id_census_block_group"),
      ords = c("cat_age_group", "n_stroke_cerebrovascular_deaths"),
      noms = c("cat_physiographic_region"),
      bounds = imputation_boundaries
    )
    
    ## Diabetes
    p_missing <- round((nrow(df_diabetes) - nrow(drop_na(df_diabetes))) / nrow(df_diabetes) * 100, 0)
    a_diabetes <- amelia(
      as.data.frame(df_diabetes),
      m = p_missing,
      idvars = c("id_census_block_group"),
      ords = c("cat_age_group", "n_diabetes_deaths"),
      noms = c("cat_physiographic_region"),
      bounds = imputation_boundaries
    )
      
write_rds(a_hypertensive, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
write_rds(a_ischemic, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
write_rds(a_stroke_cerebrovascular, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
write_rds(a_diabetes, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")
