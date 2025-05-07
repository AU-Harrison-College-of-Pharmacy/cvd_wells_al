library(tidyverse)
library(gtsummary)
library(sf)
library(gt)

df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  filter(is_included_in_analysis == 1)

cbgs_table <- df %>%
  group_by(id_census_block_group) %>%
  filter(cat_age_group == "75 or over") %>%
  ungroup() %>%
  mutate(cat_quartiles_well_use = Hmisc::cut2(amt_mean_pct_wells_cbg, g = 4, digits = 2)) %>%
  select(amt_pct_aa_only, amt_population_density_per_km2_per_age_group, cat_quartiles_well_use, cat_physiographic_region) %>%
  tbl_summary(by = cat_quartiles_well_use,
              label = list(amt_population_density_per_km2_per_age_group = "Population density per 1 km^2 (among 75 years and older)")) %>%
  as_gt() %>%
  tab_caption(paste0("Table 1. Summary statistics of Census block groups included in study (n = ", nrow(distinct(df, id_census_block_group)), ")"))

gt::gtsave(cbgs_table, "code/07_cbgs_table.docx")

events_table <- df %>%
  select(cat_age_group, n_population,  cat_hypertensive_deaths, cat_ischemic_deaths, cat_stroke_cerebrovascular_deaths, cat_diabetes_deaths) %>%
  tbl_summary(by = cat_age_group) %>%
  as_gt() %>%
  gtsave("code/07_events_table.docx")
