library(tidyverse)
library(sf)
library(tigris)
library(tmap)

df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  filter(is_included_in_analysis == 1)

df

## Maps of important covariates
df_small <- df %>%
  filter(cat_age_group == "75 or over") %>%
  mutate(amt_hypertensive_per_100k_75_older = n_hypertensive_deaths / (4 * n_population) * 100000,
         amt_ischemic_per_100k_75_older = n_ischemic_deaths / (4 * n_population) * 100000,
         amt_stroke_cerebrovascular_per_100k_75_older = n_stroke_cerebrovascular_deaths / (4 * n_population) * 100000,
         amt_diabetes_per_100k_75_older = n_diabetes_deaths / (4 * n_population) * 100000) %>%
  select(amt_mean_pct_wells_cbg, amt_area_land, amt_pct_aa_only, amt_hypertensive_per_100k_75_older, amt_ischemic_per_100k_75_older, amt_stroke_cerebrovascular_per_100k_75_older, amt_diabetes_per_100k_75_older, geometry, cat_physiographic_region) %>%
  st_as_sf()

p_wells <- tm_shape(df_small) +
  tm_polygons(fill = "amt_mean_pct_wells_cbg", fill.legend = tm_legend(title = "Percent of households relying on private wells"))

p_area_land <- tm_shape(df_small) +
  tm_polygons(fill = "amt_area_land", fill.legend = tm_legend(title = "Land area (m^2)"))

p_pct_aa <- tm_shape(df_small) +
  tm_polygons(fill = "amt_pct_aa_only", fill.legend = tm_legend(title = "Percent self-reporting African American race only"))

p_hypertensive <- tm_shape(df_small) +
  tm_polygons(fill = "amt_hypertensive_per_100k_75_older", fill.legend = tm_legend(title = "Hypertensive deaths per 100,000 (75 years and older)"))

p_ischemic <- tm_shape(df_small) +
  tm_polygons(fill = "amt_ischemic_per_100k_75_older", fill.legend = tm_legend(title = "Ischemic deaths per 100,000 (75 years and older)"))

p_stroke_cerebrovascular <- tm_shape(df_small) +
  tm_polygons(fill = "amt_stroke_cerebrovascular_per_100k_75_older", fill.legend = tm_legend(title = "Stroke/cerebrovascular deaths per 100,000 (75 years and older)"))

p_diabetes <- tm_shape(df_small) +
  tm_polygons(fill = "amt_diabetes_per_100k_75_older", fill.legend = tm_legend(title = "Diabetes deaths per 100,000 (75 years and older)"))

p_physio <- tm_shape(df_small) +
  tm_polygons(fill  = "cat_physiographic_region", fill.legend = tm_legend(title = "Physiographic regions"))

p_overall <- tmap_arrange(p_physio, p_wells, p_area_land, p_pct_aa, p_hypertensive, p_ischemic, p_stroke_cerebrovascular, p_diabetes)

tmap_options(component.autoscale = TRUE)

tmap_save(p_overall, "figs/06_covariate_maps.png", width = 10, height = 10, units = "in")




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