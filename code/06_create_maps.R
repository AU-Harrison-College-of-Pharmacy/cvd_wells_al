library(tidyverse)
library(sf)

df <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

df

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")

preds_hypertensive <- lapply(f_hypertensive, predict_response, terms = c("id_census_block_group", "cat_age_group [75 or over]"),
                             condition = c(n_population_times_4 = 100000),
                             type = "random") %>%
  pool_predictions()

preds_hypertensive %>%
  as_tibble() %>%
  rename(id_census_block_group = x,
         cat_age_group = group) %>%
  mutate(cat_age_group = as.character(cat_age_group)) %>%
  left_join(., filter(df, cat_age_group == "75 or over") %>% mutate(cat_age_group = as.character(cat_age_group)), by = c("id_census_block_group", "cat_age_group")) %>%
  ggplot() +
  geom_sf(aes(fill = predicted, geometry = geometry), linewidth = 0.1) +
  theme_void()
