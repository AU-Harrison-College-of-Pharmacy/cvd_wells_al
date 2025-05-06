library(tidyverse)
library(sf)
library(tigris)

df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

df

## Create a map of CBGs with high well use and high AA, as well as high well use and low AA. See if they are in the same place

high_wells_high_aa <- df %>%
  filter(amt_centered_scaled_mean_pct_wells_cbg > 1, 
         amt_pct_aa_only > 75) %>%
  select(id_census_block_group, geometry) %>%
  st_as_sf()

high_wells_low_aa <- df %>%
  filter(amt_centered_scaled_mean_pct_wells_cbg > 1, 
         amt_pct_aa_only < 25) %>%
  select(id_census_block_group, geometry) %>%
  st_as_sf()

counties_al <- counties(state = "AL", cb = TRUE, resolution = "20m", year = 2020)

p_high <- ggplot(data = counties_al) +
  geom_sf(fill = "white") +
  theme_void() +
  geom_sf(data = high_wells_high_aa, aes(fill = ">75% African American"), color = "grey", size = 0.1) +
  geom_sf(data = high_wells_low_aa, aes(fill = "<25% African American"), color = "grey", size = 0.1) +
  labs(
    title = "Well use percentage > 1 SD above the mean"
  ) +
  scale_fill_manual(values = c(">75% African American" = "blue", "<25% African American" = "yellow"),
                    name = "",
                    labels = c("<25% African American", ">75% African American")) +  # WARNING: This is so weird. The legend basically got the order wrong, so I had to manually switch the `labels` argument to get the colors labelled correctly.
  theme(legend.position = "bottom")


low_wells_high_aa <- df %>%
  filter(amt_centered_scaled_mean_pct_wells_cbg < 0, 
         amt_pct_aa_only > 75) %>%
  select(id_census_block_group, geometry) %>%
  st_as_sf()

low_wells_low_aa <- df %>%
  filter(amt_centered_scaled_mean_pct_wells_cbg < 0, 
         amt_pct_aa_only < 25) %>%
  select(id_census_block_group, geometry) %>%
  st_as_sf()

p_low <- ggplot(data = counties_al) +
  geom_sf(fill = "white") +
  theme_void() +
  geom_sf(data = low_wells_high_aa, fill = "blue") +
  geom_sf(data = low_wells_low_aa, fill = "yellow") +
  labs(
    title = "Well use percentage less than mean"
  )

library(patchwork)
p_overall <- p_low + p_high

p_overall

ggsave(filename = "figs/06_maps_high_low_wells_high_low_aa.pdf", p_overall, width = 9, height = 7, units = "in")

## zoom in on birmingham area

ggplot() +
  geom_sf(data = filter(counties_al, COUNTYFP %in% c("073", "117")), fill = "white") +
  theme_void() +
  geom_sf(data = filter(low_wells_high_aa, str_starts(id_census_block_group, "01073") | str_starts(id_census_block_group, "01117")), fill = "blue") +
  geom_sf(data = filter(low_wells_low_aa, str_starts(id_census_block_group, "01073") | str_starts(id_census_block_group, "01117")), fill = "yellow") +
  labs(
    title = "Low well use and high AA (blue) vs low well use and low (AA) yellow in JC or SC"
  )

## percent aa
p_aa <- df %>%
  select(id_census_block_group, amt_pct_aa_only, amt_mean_pct_wells_cbg, geometry) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = amt_pct_aa_only)) +
  theme_void()

p_wells <- df %>%
  select(id_census_block_group, amt_pct_aa_only, amt_mean_pct_wells_cbg, geometry) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = amt_mean_pct_wells_cbg)) +
  theme_void()
  
p_aa + p_wells

# SIMR: standardized incident mortality ratios

df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")

df_hypertensive$imputations[[1]] %>%
  as_tibble() %>%
  group_by(cat_age_group) %>%
  mutate(
    total_deaths = sum(n_hypertensive_deaths),
    total_population_4_years = sum(n_population_times_4),
    amt_sm = total_deaths / total_population_4_years,
    n_expected_deaths = amt_sm * n_population_times_4,
    smr = n_hypertensive_deaths / n_expected_deaths
  ) %>%
  select(id_census_block_group, total_deaths, total_population_4_years, n_expected_deaths, n_hypertensive_deaths, n_population_times_4,
         amt_sm, 
         smr) %>%
  left_join(., df) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = smr)) %>%
  facet_wrap(~ cat_age_gropu)
