library(tidyverse)
library(patchwork)
library(broom.mixed)

source("r/plot_inla.R")

# Prediction plots

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

# Hypertension

preds_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_inla.rds")

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

ggsave("figs/05_01_hypertensive_deaths_inla.pdf",
       p_hypertensive, width= 6, height=4)

# Ischemic

preds_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_inla.rds")

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

ggsave("figs/05_01_ischemic_deaths_inla.pdf",
       p_ischemic, width= 6, height=4)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_inla.rds")

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

ggsave("figs/05_01_stroke_cerebrovascular_deaths_inla.pdf",
       p_stroke_cerebrovascular, width= 6, height=4)

# Diabetes

preds_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_inla.rds")

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

ggsave("figs/05_01_diabetes_deaths_inla.pdf",
       p_diabetes, width= 6, height=4)

p <- p_hypertensive + p_ischemic + p_stroke_cerebrovascular + p_diabetes
p

ggsave(filename = "figs/05_01_combined_plots_inla.pdf", p, width= 12, height=5.5)

#########
# Interaction with physiographic region
#########

# Hypertension

preds_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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

ggsave("figs/05_01_hypertensive_deaths_ixn_physiographic_region_inla.pdf",
       p_hypertensive)

# Ischemic

preds_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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

ggsave("figs/05_01_ischemic_deaths_ixn_physiographic_region_inla.pdf",
       p_ischemic)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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

ggsave("figs/05_01_stroke_cerebrovascular_deaths_ixn_physiographic_region_inla.pdf",
       p_stroke_cerebrovascular)

# Diabetes


preds_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_ixn_physiographic_region_inla.rds")

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

ggsave("figs/05_01_diabetes_deaths_ixn_physiographic_region_inla.pdf",
       p_diabetes)

p <- p_hypertensive + p_ischemic + p_stroke_cerebrovascular + p_diabetes
p

ggsave(filename = "figs/05_01_combined_plots_deaths_ixn_physiographic_region_inla.pdf", p)
#########
# Sensitivity analysis: restricting to second largest block groups
#########

# Hypertension

preds_hypertensive_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

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

ggsave("figs/05_01_hypertensive_deaths_sensitivity_area_inla.pdf",
       p_hypertensive_sensitivity_area)

# Ischemic

preds_ischemic_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

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

ggsave("figs/05_01_ischemic_deaths_sensitivity_area_inla.pdf",
       p_ischemic_sensitivity_area)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

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

ggsave("figs/05_01_stroke_cerebrovascular_deaths_sensitivity_area_inla.pdf",
       p_stroke_cerebrovascular_sensitivity_area)

# Diabetes

preds_diabetes_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")

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

ggsave("figs/05_01_diabetes_deaths_sensitivity_area_inla.pdf",
       p_diabetes_sensitivity_area)

p_sensitivity_area <- p_hypertensive_sensitivity_area + p_ischemic_sensitivity_area + p_stroke_cerebrovascular_sensitivity_area + p_diabetes_sensitivity_area
p_sensitivity_area

ggsave(filename = "figs/05_01_combined_plots_sensitivity_area_inla.pdf", p_sensitivity_area)

