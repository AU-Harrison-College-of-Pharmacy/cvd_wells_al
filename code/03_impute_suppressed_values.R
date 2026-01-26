library(tidyverse)
library(Amelia)
library(sf)

set.seed(876234)

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

# This imputation is a little tough. There are variables missing at the CBG level, such as nitrate concetration. Then there are variables missinga the CBG-age group level, such as the count of deaths. Within each imputed dataset, the nitrate concentration should be the same for all 4 CBG-age group combinations for a given CBG. But there is no way in Amelia to set some variables to be imputed only at the group level. So, at the end, we will set the imputed CBG-level variables to be the mean of all the imputed values for the CBG-age group-level variable.

df_hypertensive <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_hypertensive_deaths, cat_physiographic_region, amt_centered_scaled_pct_aa_only, cat_rural, amt_percent_agricultural_land_use_centered_scaled, amt_median_nitrate_concentration_centered_scaled)

df_ischemic <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4,
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_ischemic_deaths, cat_physiographic_region, amt_centered_scaled_pct_aa_only, cat_rural, amt_percent_agricultural_land_use_centered_scaled, amt_median_nitrate_concentration_centered_scaled)

df_stroke_cerebrovascular <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4,
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_stroke_cerebrovascular_deaths, cat_physiographic_region, amt_centered_scaled_pct_aa_only, cat_rural, amt_percent_agricultural_land_use_centered_scaled, amt_median_nitrate_concentration_centered_scaled)

df_diabetes <- df %>%
  filter(is_included_in_analysis == 1) %>%
  mutate(
    n_population_times_4 = n_population * 4,
  ) %>%
  select(id_census_block_group, cat_age_group, amt_centered_scaled_area_land, amt_area_water, n_population_times_4, amt_centered_scaled_mean_pct_wells_cbg, n_diabetes_deaths, cat_physiographic_region, amt_centered_scaled_pct_aa_only, cat_rural, amt_percent_agricultural_land_use_centered_scaled, amt_median_nitrate_concentration_centered_scaled)

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
                        noms = c("cat_physiographic_region", "cat_rural"),
                        bounds = imputation_boundaries
    )
    
    ## Ischemic
    p_missing <- round((nrow(df_ischemic) - nrow(drop_na(df_ischemic))) / nrow(df_ischemic) * 100, 0)
    a_ischemic <- amelia(
      as.data.frame(df_ischemic),
      m = p_missing,
      idvars = c("id_census_block_group"),
      ords = c("cat_age_group", "n_ischemic_deaths"),noms = c("cat_physiographic_region", "cat_rural"),
      bounds = imputation_boundaries
    )
    
    ## Stroke/cerebrovascular
    p_missing <- round((nrow(df_stroke_cerebrovascular) - nrow(drop_na(df_stroke_cerebrovascular))) / nrow(df_stroke_cerebrovascular) * 100, 0)
    a_stroke_cerebrovascular <- amelia(
      as.data.frame(df_stroke_cerebrovascular),
      m = p_missing,
      idvars = c("id_census_block_group"),
      ords = c("cat_age_group", "n_stroke_cerebrovascular_deaths"),
      noms = c("cat_physiographic_region", "cat_rural"),
      bounds = imputation_boundaries
    )
    
    ## Diabetes
    p_missing <- round((nrow(df_diabetes) - nrow(drop_na(df_diabetes))) / nrow(df_diabetes) * 100, 0)
    a_diabetes <- amelia(
      as.data.frame(df_diabetes),
      m = p_missing,
      idvars = c("id_census_block_group"),
      ords = c("cat_age_group", "n_diabetes_deaths"),
      noms = c("cat_physiographic_region", "cat_rural"),
      bounds = imputation_boundaries
    )

# Now we will average the continuous imputed values for CBG-level variables. For categorical variables, we will use the mode. R doesn't have a mode function, so I created one via Gemini.
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
    
a_hypertensive$imputations <- lapply(1:length(a_hypertensive$imputations), function(i){
  group_by(a_hypertensive$imputations[[i]], id_census_block_group) %>%
    mutate(
      amt_median_nitrate_concentration_centered_scaled = mean(amt_median_nitrate_concentration_centered_scaled),
      amt_centered_scaled_mean_pct_wells_cbg = mean(amt_centered_scaled_mean_pct_wells_cbg),
      amt_centered_scaled_pct_aa_only = mean(amt_centered_scaled_pct_aa_only),
      cat_rural = get_mode(cat_rural),
      cat_physiographic_region = get_mode(cat_physiographic_region),
      amt_percent_agricultural_land_use_centered_scaled = mean(amt_percent_agricultural_land_use_centered_scaled)
    )
}
)

a_ischemic$imputations <- lapply(1:length(a_ischemic$imputations), function(i){
  group_by(a_ischemic$imputations[[i]], id_census_block_group) %>%
    mutate(
      amt_median_nitrate_concentration_centered_scaled = mean(amt_median_nitrate_concentration_centered_scaled),
      amt_centered_scaled_mean_pct_wells_cbg = mean(amt_centered_scaled_mean_pct_wells_cbg),
      amt_centered_scaled_pct_aa_only = mean(amt_centered_scaled_pct_aa_only),
      cat_rural = get_mode(cat_rural),
      cat_physiographic_region = get_mode(cat_physiographic_region),
      amt_percent_agricultural_land_use_centered_scaled = mean(amt_percent_agricultural_land_use_centered_scaled)
    )
}
)

a_stroke_cerebrovascular$imputations <- lapply(1:length(a_stroke_cerebrovascular$imputations), function(i){
  group_by(a_stroke_cerebrovascular$imputations[[i]], id_census_block_group) %>%
    mutate(
      amt_median_nitrate_concentration_centered_scaled = mean(amt_median_nitrate_concentration_centered_scaled),
      amt_centered_scaled_mean_pct_wells_cbg = mean(amt_centered_scaled_mean_pct_wells_cbg),
      amt_centered_scaled_pct_aa_only = mean(amt_centered_scaled_pct_aa_only),
      cat_rural = get_mode(cat_rural),
      cat_physiographic_region = get_mode(cat_physiographic_region),
      amt_percent_agricultural_land_use_centered_scaled = mean(amt_percent_agricultural_land_use_centered_scaled)
    )
}
)

a_diabetes$imputations <- lapply(1:length(a_diabetes$imputations), function(i){
  group_by(a_diabetes$imputations[[i]], id_census_block_group) %>%
    mutate(
      amt_median_nitrate_concentration_centered_scaled = mean(amt_median_nitrate_concentration_centered_scaled),
      amt_centered_scaled_mean_pct_wells_cbg = mean(amt_centered_scaled_mean_pct_wells_cbg),
      amt_centered_scaled_pct_aa_only = mean(amt_centered_scaled_pct_aa_only),
      cat_rural = get_mode(cat_rural),
      cat_physiographic_region = get_mode(cat_physiographic_region),
      amt_percent_agricultural_land_use_centered_scaled = mean(amt_percent_agricultural_land_use_centered_scaled)
    )
}
)
          
write_rds(a_hypertensive, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")
write_rds(a_ischemic, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")
write_rds(a_stroke_cerebrovascular, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")
write_rds(a_diabetes, "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")
