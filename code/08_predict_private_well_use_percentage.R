library(tidyverse)
library(rpart)
library(rpart.plot)
library(sf)

df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  filter(is_included_in_analysis == 1) %>%
  st_as_sf() %>%
  mutate(
    centroid = st_centroid(geometry)
  )

df_small <- df %>%
  group_by(id_census_block_group) %>%
  slice(1) %>%
  ungroup() %>%
  select(amt_area_land, amt_pct_aa_only, amt_mean_pct_wells_cbg, amt_area_water, centroid) %>%
  as_tibble() %>%
  mutate(
    x = map_dbl(centroid, ~as.numeric(.[1])),
    y = map_dbl(centroid, ~as.numeric(.[2]))
  ) %>%
  select(-centroid, -geometry)

df_small

f <- rpart(amt_mean_pct_wells_cbg ~ ., data = df_small)

png("figs/08_cart_predicted_well_percentage.png", res = 300, width = 3, height = 3, units = "in")
rpart.plot::rpart.plot(f)
dev.off()
