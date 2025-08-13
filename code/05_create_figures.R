library(tidyverse)
library(patchwork)
library(broom.mixed)
library(ggeffects)

# Prediction plots

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_main.rds")

preds_hypertensive <- pool_predictions(lapply(f_hypertensive, 
                        predict_response, 
                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                        condition = c(n_population_times_4 = 100000)), 
                      )

p_hypertensive <- preds_hypertensive %>% 
  filter(group == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("../figs/05_hypertensive_deaths.pdf",
       p_hypertensive, width= 6, height=4)

# Ischemic

f_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_main.rds")

preds_ischemic <- pool_predictions(lapply(f_ischemic, 
                                              predict_response, 
                                              terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                              condition = c(n_population_times_4 = 100000)), 
)

p_ischemic <- preds_ischemic %>% 
  filter(group == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("../figs/05_ischemic_deaths.pdf",
       p_ischemic, width= 6, height=4)

# Stroke/cerebrovascular


f_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_main.rds")

preds_stroke_cerebrovascular <- pool_predictions(lapply(f_stroke_cerebrovascular, 
                                          predict_response, 
                                          terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                          condition = c(n_population_times_4 = 100000)), 
)

p_stroke_cerebrovascular <- preds_stroke_cerebrovascular %>% 
  filter(group == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("../figs/05_stroke_cerebrovascular_deaths.pdf",
       p_stroke_cerebrovascular, width= 6, height=4)

# Diabetes


f_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_main.rds")

preds_diabetes <- pool_predictions(lapply(f_diabetes, 
                                          predict_response, 
                                          terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                          condition = c(n_population_times_4 = 100000)), 
)

p_diabetes <- preds_diabetes %>% 
  filter(group == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("../figs/05_diabetes_deaths.pdf",
       p_diabetes, width= 6, height=4)
#########
# Sensitivity analysis: restricting to second largest block groups
#########

# Hypertension

preds_hypertensive_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_hypertension_deaths_poisson_model_sensitivity_area_inla.rds")

p_hypertensive_sensitivity_area <- preds_hypertensive_sensitivity_area %>% 
  filter(cat_age_group == "75 or over") %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("figs/05_hypertensive_deaths_sensitivity_area_inla.pdf",
       p_hypertensive_sensitivity_area, width= 6, height=4)

# Ischemic

preds_ischemic_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_ischemic_deaths_poisson_model_sensitivity_area_inla.rds")

p_ischemic_sensitivity_area <- preds_ischemic_sensitivity_area %>% 
  filter(cat_age_group == "75 or over") %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Ischemic deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("figs/05_ischemic_deaths_sensitivity_area_inla.pdf",
       p_ischemic_sensitivity_area, width= 6, height=4)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area_inla.rds")

p_stroke_cerebrovascular_sensitivity_area <- preds_stroke_cerebrovascular_sensitivity_area %>% 
  filter(cat_age_group == "75 or over") %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("figs/05_stroke_cerebrovascular_deaths_sensitivity_area_inla.pdf",
       p_stroke_cerebrovascular_sensitivity_area, width= 6, height=4)

# Diabetes

preds_diabetes_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_preds_diabetes_deaths_poisson_model_sensitivity_area_inla.rds")

p_diabetes_sensitivity_area <- preds_diabetes_sensitivity_area %>% 
  filter(cat_age_group == "75 or over") %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Diabetes deaths per 100,000",
    color = "Age group (years)",
    fill = "Age group (years)"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors) + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray"),
        legend.position = "none")

ggsave("figs/05_diabetes_deaths_sensitivity_area_inla.pdf",
       p_diabetes_sensitivity_area, width= 6, height=4)

p_sensitivity_area <- p_hypertensive_sensitivity_area + p_ischemic_sensitivity_area + p_stroke_cerebrovascular_sensitivity_area + p_diabetes_sensitivity_area
p_sensitivity_area

ggsave(filename = "figs/05_combined_plots_sensitivity_area_inla.pdf", p_sensitivity_area, width= 12, height=5.5)

