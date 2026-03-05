library(tidyverse)
library(gtsummary)
library(sf)
library(gt)

df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  filter(is_included_in_analysis == 1)

cbgs_table <- df %>%
  group_by(id_census_block_group) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(cat_quartiles_well_use = Hmisc::cut2(amt_mean_pct_wells_cbg, g = 4, digits = 2)) %>%
  select(amt_pct_aa_only, amt_population_density_per_km2_per_age_group, cat_quartiles_well_use, cat_physiographic_region) %>%
  tbl_summary(by = cat_quartiles_well_use,
              label = list(amt_population_density_per_km2_per_age_group = "Population density per 1 km^2 (among 45 - 54 year olds)")) %>%
  add_overall() %>%
  as_gt() %>%
  tab_caption(paste0("Table 1. Summary statistics of Census block groups included in study (n = ", nrow(distinct(df, id_census_block_group)), ")"))

gt::gtsave(cbgs_table, "code/07_cbgs_table.docx")

## Distribution of well use
df %>%
  group_by(id_census_block_group) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(
    median_well_use = median(amt_mean_pct_wells_cbg),
    first_quartile = quantile(amt_mean_pct_wells_cbg, 0.25),
    third_quartile = quantile(amt_mean_pct_wells_cbg, 0.75)
  )

events_table <- df %>%
  select(cat_age_group, n_population,  cat_hypertensive_deaths, cat_ischemic_deaths, cat_stroke_cerebrovascular_deaths, cat_diabetes_deaths) %>%
  tbl_summary(by = cat_age_group) %>%
  as_gt() %>%
  gtsave("code/07_events_table.docx")


## Event rate of each outcome by age group
df_imp_hyper <- df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")$imputations[[1]] %>%
  as_tibble()

df_imp_hyper %>%
  ungroup() %>%
  group_by(cat_age_group) %>%
  summarize(
    n_hyper = sum(n_hypertensive_deaths),
    n_pop = sum(n_population_times_4),
    hyper_rate = sum(n_hypertensive_deaths) / sum(n_population_times_4) * 100000
  )

df_imp_ischemic <- df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")$imputations[[1]] %>%
  as_tibble()

df_imp_ischemic %>%
  ungroup() %>%
  group_by(cat_age_group) %>%
  summarize(
    n_isch = sum(n_ischemic_deaths),
    n_pop = sum(n_population_times_4),
    isch_rate = sum(n_ischemic_deaths) / sum(n_population_times_4) * 100000
  )

df_imp_stroke <- df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")$imputations[[1]] %>%
  as_tibble()

df_imp_stroke %>%
  ungroup() %>%
  group_by(cat_age_group) %>%
  summarize(
    n_stroke = sum(n_stroke_cerebrovascular_deaths),
    n_pop = sum(n_population_times_4),
    stroke_rate = sum(n_stroke_cerebrovascular_deaths) / sum(n_population_times_4) * 100000
  )

