library(tidyverse)
library(sf)
library(spdep)
library(modelr)
library(tigris)
library(tmap)


df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")
df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")

a = tibble(fits = f_hypertensive,
       original_data = df_hypertensive$imputations) %>%
  mutate(imputation = seq(1:n())) %>%
  mutate(
    preds = map2(original_data, fits, ~add_predictions(.x, model = .y, type = "response") %>%
                   mutate(res = pred - n_hypertensive_deaths))
  )

residuals <- a %>%
  unnest(c(imputation, preds)) %>%
  select(imputation, id_census_block_group, res, cat_age_group) %>%
  group_by(id_census_block_group, cat_age_group) %>%
  summarise(
    avg_residual = mean(res)
  ) %>%
  ungroup() %>%
  filter(cat_age_group == "75 or over")


residuals_st <- residuals %>%
  left_join(., df %>% select(id_census_block_group, geometry, cat_age_group), by = c("id_census_block_group", "cat_age_group")) %>%
  st_as_sf()

residuals_st %>%
  tm_shape() +
  tm_polygons(fill = "avg_residual")

residuals_st %>%
  filter(str_starts(id_census_block_group, "01073|01117")) %>%
  tm_shape() +
  tm_polygons(fill = "avg_residual")

nb <- poly2nb(residuals_st, queen = FALSE)
nbw <- nb2listw(nb, style = "W")

gmoran <- moran.test(residuals_st$avg_residual, 
                     nbw,
                     alternative = "greater")

gmoran

residuals_jefferson_shelby_st <- residuals_st %>%
  filter(str_starts(id_census_block_group, "01073|01117"))

gmoran_jefferson_shelby <- moran.test(residuals_jefferson_shelby_st$avg_residual,
                                      nb2listw(poly2nb(residuals_jefferson_shelby_st, queen = FALSE), style = "W")
                                      )
gmoran_jefferson_shelby
