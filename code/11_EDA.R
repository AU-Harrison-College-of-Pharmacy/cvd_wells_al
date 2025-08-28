library(tidyverse)
library(gtsummary)
library(sf)
library(tmap)

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

options(scipen = 999)

df %>%
  group_by(cat_age_group) %>%
  summarise(
    rate_hyper = sum(n_hypertensive_deaths, na.rm = TRUE) / sum(n_population * 4, na.rm = TRUE) * 100000
  )

df %>%
  count(cat_age_group, n_hypertensive_deaths)

df %>%
  filter(is_included_in_analysis == 1) %>%
  group_by(id_census_block_group) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    cat_wells_tertiles = Hmisc::cut2(amt_mean_pct_wells_cbg, g = 3, digits = 1)
  ) %>%
  select(amt_pct_aa_only, cat_rural, cat_wells_tertiles, cat_physiographic_region, amt_percent_agricultural_land_use) %>%
  tbl_summary(by = cat_wells_tertiles) %>%
  add_overall()

df %>%
  filter(is_included_in_analysis == 1) %>%
  select(contains("cat")) %>%
  select(-c(cat_ruca, cat_rural, cat_physiographic_region, cat_diabetes_deaths)) %>%
  tbl_summary(by = cat_age_group) %>%
  add_overall()

df %>%
  filter(is_included_in_analysis == 1) %>%
  group_by(id_census_block_group) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf() %>%
  tm_shape() +
  tm_polygons(fill = "amt_mean_pct_wells_cbg")
