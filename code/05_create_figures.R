library(tidyverse)
library(lme4)
library(ggeffects)
library(patchwork)
library(mice)
library(broom.mixed)
library(INLA)

source("r/pool_inla.R")
source("r/pool_predictions_inla.R")
source("r/plot_inla.R")

# Get the original well data so that we can show the results in terms of the original well percentages, not the centered and scaled well percentages

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_inla.rds")

pool_inla(f_hypertensive) %>%
  select(term, estimate, conf.low, conf.high)

preds_hypertensive <- pool_predictions_inla(f_hypertensive, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

p_hypertensive <- preds_hypertensive %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_hypertensive_deaths_inla.pdf",
       p_hypertensive)

# Ischemic

f_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_inla.rds")

pool_inla(f_ischemic) %>%
  select(term, estimate, conf.low, conf.high)

preds_ischemic <- pool_predictions_inla(f_ischemic, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                        condition = c(n_population_times_4 = 100000))

p_ischemic <- preds_ischemic %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_ischemic_deaths_inla.pdf",
       p_ischemic)

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_inla.rds")

pool_inla(f_stroke_cerebrovascular) %>%
  select(term, estimate, conf.low, conf.high)

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                      condition = c(n_population_times_4 = 100000))

p_stroke_cerebrovascular <- preds_stroke_cerebrovascular %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_stroke_cerebrovascular_deaths_inla.pdf",
       p_stroke_cerebrovascular)

# Diabetes

f_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_inla.rds")

pool_inla(f_diabetes) %>%
  select(term, estimate, conf.low, conf.high) 

preds_diabetes <- pool_predictions_inla(f_diabetes, 
                                             terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                             condition = c(n_population_times_4 = 100000))


p_diabetes <- preds_diabetes %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_diabetes_deaths_inla.pdf",
       p_diabetes)

p <- p_hypertensive + p_ischemic + p_stroke_cerebrovascular + p_diabetes
p

ggsave(filename = "figs/05_combined_plots_inla.pdf", p)

#########
# Interaction with physiographic region
#########

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_hypertensive <- pool_predictions_inla(f_hypertensive, 
                                            terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                            condition = c(n_population_times_4 = 100000))

p_hypertensive <- preds_hypertensive %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000",
    color = "Physiographic region according to USGS",
    fill = "Physiographic region according to USGS"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_hypertensive_deaths_ixn_physiographic_region_inla.pdf",
       p_hypertensive)

# Ischemic

f_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_ischemic <- pool_predictions_inla(f_ischemic, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                        condition = c(n_population_times_4 = 100000))

p_ischemic <- preds_ischemic %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000",
    color = "Physiographic region according to USGS",
    fill = "Physiographic region according to USGS"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_ischemic_deaths_ixn_physiographic_region_inla.pdf",
       p_ischemic)

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_stroke_cerebrovascular <- pool_predictions_inla(f_stroke_cerebrovascular, 
                                                      terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                                      condition = c(n_population_times_4 = 100000))

p_stroke_cerebrovascular <- preds_stroke_cerebrovascular %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000",
    color = "Physiographic region according to USGS",
    fill = "Physiographic region according to USGS"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_stroke_cerebrovascular_deaths_ixn_physiographic_region_inla.pdf",
       p_stroke_cerebrovascular)

# Diabetes

f_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")

preds_diabetes <- pool_predictions_inla(f_diabetes, 
                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                        condition = c(n_population_times_4 = 100000))

p_diabetes <- preds_diabetes %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000",
    color = "Physiographic region according to USGS",
    fill = "Physiographic region according to USGS"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_diabetes_deaths_ixn_physiographic_region_inla.pdf",
       p_diabetes)

#########
# Sensitivity analysis: restricting to second largest block groups
#########

# Hypertension
f_hypertensive_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")



preds_hypertensive_sensitivity_area <- pool_predictions_inla(f_hypertensive_sensitivity_area, 
                                                             terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                             condition = c(n_population_times_4 = 100000))

p_hypertensive_sensitivity_area <- preds_hypertensive_sensitivity_area %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_hypertensive_deaths_sensitivity_area_inla.pdf",
       p_hypertensive_sensitivity_area)

# Ischemic
f_ischemic_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

preds_ischemic_sensitivity_area <- pool_predictions_inla(f_ischemic_sensitivity_area, 
                                                         terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                         condition = c(n_population_times_4 = 100000))

p_ischemic_sensitivity_area <- preds_ischemic_sensitivity_area %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_ischemic_deaths_sensitivity_area_inla.pdf",
       p_ischemic_sensitivity_area)

# Stroke/cerebrovascular
f_stroke_cerebrovascular_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

preds_stroke_cerebrovascular_sensitivity_area <- pool_predictions_inla(f_stroke_cerebrovascular_sensitivity_area, 
                                                                       terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                                       condition = c(n_population_times_4 = 100000))

p_stroke_cerebrovascular_sensitivity_area <- preds_stroke_cerebrovascular_sensitivity_area %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_stroke_cerebrovascular_deaths_sensitivity_area_inla.pdf",
       p_stroke_cerebrovascular_sensitivity_area)

# Diabetes
f_diabetes_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")

preds_diabetes_sensitivity_area <- pool_predictions_inla(f_diabetes_sensitivity_area, 
                                                         terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                                         condition = c(n_population_times_4 = 100000))

p_diabetes_sensitivity_area <- preds_diabetes_sensitivity_area %>% 
  plot_inla() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_diabetes_deaths_sensitivity_area_inla.pdf",
       p_diabetes_sensitivity_area)

p_sensitivity_area <- p_hypertensive_sensitivity_area + p_ischemic_sensitivity_area + p_stroke_cerebrovascular_sensitivity_area + p_diabetes_sensitivity_area
p_sensitivity_area

ggsave(filename = "figs/05_combined_plots_sensitivity_area_inla.pdf", p_sensitivity_area)
