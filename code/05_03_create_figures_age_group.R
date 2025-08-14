library(tidyverse)
library(patchwork)
library(broom.mixed)

source("r/plot_inla.R")

# Prediction plots

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

#########
# Interaction with age group
#########

# Hypertension

preds_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_03_preds_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

p_hypertensive_ixn_age_group <- preds_hypertensive %>% 
  mutate(cat_age_group = as.factor(cat_age_group)) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Age group") +
  scale_fill_manual(values = au_colors, name = "Age group") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_03_hypertensive_deaths_ixn_age_group_inla.pdf",
       p_hypertensive_ixn_age_group, width= 6, height=4)

# Ischemic

preds_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_03_preds_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

p_ischemic_ixn_age_group <- preds_ischemic %>% 
  mutate(cat_age_group = as.factor(cat_age_group)) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Age group") +
  scale_fill_manual(values = au_colors, name = "Age group") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_03_ischemic_deaths_ixn_age_group_inla.pdf",
       p_ischemic_ixn_age_group, width= 6, height=4)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_03_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

p_stroke_cerebrovascular_ixn_age_group <- preds_stroke_cerebrovascular %>% 
  mutate(cat_age_group = as.factor(cat_age_group)) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Age group") +
  scale_fill_manual(values = au_colors, name = "Age group") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_03_stroke_cerebrovascular_deaths_ixn_age_group_inla.pdf",
       p_stroke_cerebrovascular_ixn_age_group, width= 6, height=4)

# Diabetes

preds_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_03_preds_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")

p_diabetes_ixn_age_group <- preds_diabetes %>% 
  mutate(cat_age_group = as.factor(cat_age_group)) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Age group") +
  scale_fill_manual(values = au_colors, name = "Age group") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_03_diabetes_deaths_ixn_age_group_inla.pdf",
       p_diabetes_ixn_age_group, width= 6, height=4)

p <- p_hypertensive_ixn_age_group + p_ischemic_ixn_age_group + p_stroke_cerebrovascular_ixn_age_group + p_diabetes_ixn_age_group
p

ggsave(filename = "figs/05_03_combined_plots_deaths_ixn_age_group_inla.pdf", p, width= 12, height=5.5)

p_no_diabetes <- wrap_plots(list(p_hypertensive_ixn_age_group, p_ischemic_ixn_age_group), ncol = 1)

ggsave(filename = "figs/05_03_combined_plots_deaths_ixn_age_group_inla_no_diabetes.png", p_no_diabetes, width= 5, height=4, dpi = 300)


