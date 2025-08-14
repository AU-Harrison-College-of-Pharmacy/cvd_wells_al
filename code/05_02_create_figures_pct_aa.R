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
# Interaction with percent reporting AA race alone
#########

# Hypertension

preds_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_hypertension_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_hypertensive_ixn_pct_aa <- preds_hypertensive %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
  filter(cat_age_group == "75 or over") %>%
  select(-cat_age_group) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_hypertensive_deaths_ixn_pct_aa_inla.pdf",
       p_hypertensive_ixn_pct_aa, width= 6, height=4)

# Ischemic

preds_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_ischemic_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_ischemic_ixn_pct_aa <- preds_ischemic %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
  filter(cat_age_group == "75 or over") %>%
  select(-cat_age_group) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_ischemic_deaths_ixn_pct_aa.pdf",
       p_ischemic_ixn_pct_aa, width= 6, height=4)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_stroke_cerebrovascular_ixn_pct_aa <- preds_stroke_cerebrovascular %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
  filter(cat_age_group == "75 or over") %>%
  select(-cat_age_group) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_stroke_cerebrovascular_deaths_ixn_pct_aa.pdf",
       p_stroke_cerebrovascular_ixn_pct_aa, width= 6, height=4)

# Diabetes

preds_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_diabetes_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_diabetes_ixn_pct_aa <- preds_diabetes %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
  filter(cat_age_group == "75 or over") %>%
  select(-cat_age_group) %>%
  plot_inla() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_diabetes_deaths_ixn_pct_aa.pdf",
       p_diabetes_ixn_pct_aa, width= 6, height=4) 

p <- p_hypertensive_ixn_pct_aa + p_ischemic_ixn_pct_aa + p_stroke_cerebrovascular_ixn_pct_aa + p_diabetes_ixn_pct_aa
p

ggsave(filename = "figs/05_02_combined_plots_deaths_ixn_pct_aa_inla.pdf", p, width= 12, height=5.5)

p_no_diabetes <- wrap_plots(list(p_hypertensive_ixn_pct_aa, p_ischemic_ixn_pct_aa), ncol = 1)

ggsave(filename = "figs/05_02_combined_plots_deaths_ixn_pct_aa_inla_no_diabetes.png", p_no_diabetes, width= 5, height=4, dpi = 300)
