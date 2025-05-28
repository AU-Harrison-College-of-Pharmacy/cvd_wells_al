library(tidyverse)
library(Amelia)
library(lme4)
library(sf)
library(INLA)

#### 
# Check the covariate overlap of your models
####

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds") %>%
  dplyr::filter(is_included_in_analysis == 1)

p_overlap_wells_area <- df %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = Hmisc::cut2(amt_centered_scaled_mean_pct_wells_cbg, g = 4),
    cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4)
  ) %>%
  count(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells) %>%
  arrange(cat_centered_scaled_mean_pct_wells, cat_centered_scaled_area_land) %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = as.character(cat_centered_scaled_mean_pct_wells),
    cat_centered_scaled_area_land = as.character(cat_centered_scaled_area_land)
  ) %>%
  tidyr::complete(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells) %>%
  ggplot(aes(x = cat_centered_scaled_mean_pct_wells, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cat_centered_scaled_area_land, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = str_wrap("Percentage of households reliant on private wells (centered and scaled)", 40),
       title = str_wrap("Bar plot of quartiles of percentage of households reliant on private wells, by quartiles of land area", 55)
  )

ggsave("figs/04_overlap_wells_area.pdf", width = 5, height = 4, units = "in")

df %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = Hmisc::cut2(amt_centered_scaled_mean_pct_wells_cbg, g = 4),
    cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4),
    cat_pct_aa_only = Hmisc::cut2(amt_pct_aa_only, g = 4)
  ) %>%
  count(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells, cat_pct_aa_only) %>%
  arrange(cat_centered_scaled_mean_pct_wells, cat_centered_scaled_area_land, cat_pct_aa_only) %>%
  mutate(
    cat_centered_scaled_mean_pct_wells = as.character(cat_centered_scaled_mean_pct_wells),
    cat_centered_scaled_area_land = as.character(cat_centered_scaled_area_land),
    cat_pct_aa_only = as.character(cat_pct_aa_only)
  ) %>%
  tidyr::complete(cat_centered_scaled_area_land, cat_centered_scaled_mean_pct_wells, cat_pct_aa_only) %>%
  ggplot(aes(x = cat_centered_scaled_mean_pct_wells, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(cat_pct_aa_only ~ cat_centered_scaled_area_land, scales = "free_y")

df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_hypertensive$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_hypertensive, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_inla.rds")

# Ischemic heart disease

df_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_ischemic_deaths.rds")

f_ischemic <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_ischemic$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_ischemic, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_inla.rds")

# Stroke/cerebrovascular

df_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_stroke_cerebrovascular_deaths.rds")

f_stroke_cerebrovascular <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_stroke_cerebrovascular$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_stroke_cerebrovascular, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_inla.rds")

# Diabetes

df_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_diabetes_deaths.rds")

f_diabetes <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
            data = df_diabetes$imputations[[i]],
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_diabetes, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_inla.rds")


#######
# Sensitivity analyses restricting to second-largest quartile of block groups
#######

df %>%
  mutate(cat_centered_scaled_area_land = Hmisc::cut2(amt_centered_scaled_area_land, g = 4)
) %>%
  distinct(cat_centered_scaled_area_land)

# Hypertensive
f_hypertensive_sensitivity_area <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- inla(n_hypertensive_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             data = df_hypertensive$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_hypertensive_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

# Ischemic
f_ischemic_sensitivity_area <- lapply(1:length(df_ischemic$imputations), function(i){
  f <- inla(n_ischemic_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             data = df_ischemic$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_ischemic_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

# Stroke/cerebrovascular

f_stroke_cerebrovascular_sensitivity_area <- lapply(1:length(df_stroke_cerebrovascular$imputations), function(i){
  f <- inla(n_stroke_cerebrovascular_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             data = df_stroke_cerebrovascular$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_stroke_cerebrovascular_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

# Diabetes

f_diabetes_sensitivity_area <- lapply(1:length(df_diabetes$imputations), function(i){
  f <- inla(n_diabetes_deaths ~ f(id_census_block_group, model = "iid") + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + amt_centered_scaled_area_land + offset(log(n_population_times_4)),
             data = df_diabetes$imputations[[i]] %>% filter(amt_centered_scaled_area_land >= -0.4228, amt_centered_scaled_area_land < 0.0508),
            family = "poisson",
            control.compute = list(config = TRUE)
  )
})

write_rds(f_diabetes_sensitivity_area, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")
