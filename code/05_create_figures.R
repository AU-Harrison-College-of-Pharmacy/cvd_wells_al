library(tidyverse)
library(lme4)
library(ggeffects)
library(patchwork)
library(mice)
library(broom.mixed)
library(gt)

# Get the original well data so that we can show the results in terms of the original well percentages, not the centered and scaled well percentages

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")

pool(f_hypertensive) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death")

preds_hypertensive <- lapply(f_hypertensive, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_hypertensive <- preds_hypertensive %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_hypertensive_deaths.pdf",
       p_hypertensive)

# Ischemic

f_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model.rds")

pool(f_ischemic) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_ischemic <- lapply(f_ischemic, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                             condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_ischemic <- preds_ischemic %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_ischemic_deaths.pdf",
       p_ischemic)

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model.rds")

pool(f_stroke_cerebrovascular) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_stroke_cerebrovascular <- lapply(f_stroke_cerebrovascular, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_stroke_cerebrovascular <- preds_stroke_cerebrovascular %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_stroke_cerebrovascular_deaths.pdf",
       p_stroke_cerebrovascular)

# Diabetes

f_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model.rds")

pool(f_diabetes) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_diabetes <- lapply(f_diabetes, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                       condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_diabetes <- preds_diabetes %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_diabetes_deaths.pdf",
       p_diabetes)

p <- p_hypertensive + p_ischemic + p_stroke_cerebrovascular + p_diabetes
p

ggsave(filename = "figs/05_combined_plots.pdf", p)

#########
# Interaction with physiographic region
#########

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_hypertension_deaths_poisson_model_ixn_physiographic_region.rds")

pool(f_hypertensive) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_hypertensive <- lapply(f_hypertensive, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                             condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_hypertensive_ixn_physiographic_region <- preds_hypertensive %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_hypertensive_deaths_ixn_physiographic_region.pdf",
       p_hypertensive_ixn_physiographic_region)

# Ischemic

f_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_ischemic_deaths_poisson_model_ixn_physiographic_region.rds")

pool(f_ischemic) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_ischemic <- lapply(f_ischemic, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_ischemic_ixn_physiographic_region <- preds_ischemic %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_ischemic_deaths_ixn_physiographic_region.pdf",
       p_ischemic_ixn_physiographic_region)

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_stroke_cerebrovascular_deaths_poisson_model_ixn_physiographic_region.rds")

pool(f_stroke_cerebrovascular) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_stroke_cerebrovascular <- lapply(f_stroke_cerebrovascular, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                                       condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_stroke_cerebrovascular_ixn_physiographic_region <- preds_stroke_cerebrovascular %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_stroke_cerebrovascular_deaths_ixn_physiographic_region.pdf",
       p_stroke_cerebrovascular_ixn_physiographic_region)

# Diabetes

f_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_01_diabetes_deaths_poisson_model_ixn_physiographic_region.rds")

pool(f_diabetes) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high)

preds_diabetes <- lapply(f_diabetes, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_physiographic_region"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_diabetes_ixn_physiographic_region <- preds_diabetes %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_diabetes_deaths_ixn_physiographic_region.pdf",
       p_diabetes_ixn_physiographic_region)

p_ixn_physiographic_region <- p_hypertensive_ixn_physiographic_region + p_ischemic_ixn_physiographic_region + p_stroke_cerebrovascular_ixn_physiographic_region + p_diabetes_ixn_physiographic_region
p_ixn_physiographic_region

ggsave(filename = "figs/05_combined_plots_ixn_physiographic_region.pdf", p_ixn_physiographic_region)

#########
# Sensitivity analysis: restricting to second largest block groups
#########

# Hypertension
f_hypertensive_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model_sensitivity_area.rds")

pool(f_hypertensive_sensitivity_area) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death")

preds_hypertensive_sensitivity_area <- lapply(f_hypertensive_sensitivity_area, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                             condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_hypertensive_sensitivity_area <- preds_hypertensive_sensitivity_area %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_hypertensive_deaths_sensitivity_area.pdf",
       p_hypertensive_sensitivity_area)

# Ischemic
f_ischemic_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_ischemic_deaths_poisson_model_sensitivity_area.rds")

pool(f_ischemic_sensitivity_area) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death")

preds_ischemic_sensitivity_area <- lapply(f_ischemic_sensitivity_area, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                              condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_ischemic_sensitivity_area <- preds_ischemic_sensitivity_area %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_ischemic_deaths_sensitivity_area.pdf",
       p_ischemic_sensitivity_area)

# Stroke/cerebrovascular
f_stroke_cerebrovascular_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_stroke_cerebrovascular_deaths_poisson_model_sensitivity_area.rds")

pool(f_stroke_cerebrovascular_sensitivity_area) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death")

preds_stroke_cerebrovascular_sensitivity_area <- lapply(f_stroke_cerebrovascular_sensitivity_area, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                          condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_stroke_cerebrovascular_sensitivity_area <- preds_stroke_cerebrovascular_sensitivity_area %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_stroke_cerebrovascular_deaths_sensitivity_area.pdf",
       p_stroke_cerebrovascular_sensitivity_area)

# Diabetes
f_diabetes_sensitivity_area <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_diabetes_deaths_poisson_model_sensitivity_area.rds")

pool(f_diabetes_sensitivity_area) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death")

preds_diabetes_sensitivity_area <- lapply(f_diabetes_sensitivity_area, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "cat_age_group"),
                                          condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_diabetes_sensitivity_area <- preds_diabetes_sensitivity_area %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors) +
  scale_fill_manual(values = au_colors)

ggsave("figs/05_diabetes_deaths_sensitivity_area.pdf",
       p_diabetes_sensitivity_area)

p_sensitivity_area <- p_hypertensive_sensitivity_area + p_ischemic_sensitivity_area + p_stroke_cerebrovascular_sensitivity_area + p_diabetes_sensitivity_area
p_sensitivity_area

ggsave(filename = "figs/05_combined_plots_sensitivity_area.pdf", p_sensitivity_area)

#########
# Interaction with percent reporting AA race alone
#########

# Hypertension

f_hypertensive_ixn_pct_aa <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_hypertension_deaths_poisson_model_ixn_pct_aa.rds")

pool(f_hypertensive_ixn_pct_aa) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Hypertensive death")

preds_hypertensive <- lapply(f_hypertensive_ixn_pct_aa, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                             condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_hypertensive_ixn_pct_aa <- preds_hypertensive %>% 
  plot() +
  labs(
    x = "Percent private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only")

ggsave("figs/05_hypertensive_deaths_ixn_pct_aa.pdf",
       p_hypertensive_ixn_pct_aa)

# Ischemic

f_ischemic_ixn_pct_aa <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_ischemic_deaths_poisson_model_ixn_pct_aa.rds")

pool(f_ischemic_ixn_pct_aa) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Ischemic death")

preds_ischemic <- lapply(f_ischemic_ixn_pct_aa, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_ischemic_ixn_pct_aa <- preds_ischemic %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only")

ggsave("figs/05_ischemic_deaths_ixn_pct_aa.pdf",
       p_ischemic_ixn_pct_aa)

# Stroke/cerebrovascular

f_stroke_cerebrovascular_ixn_pct_aa <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa.rds")

pool(f_stroke_cerebrovascular_ixn_pct_aa) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Stroke/cerebrovascular death")

preds_stroke_cerebrovascular <- lapply(f_stroke_cerebrovascular_ixn_pct_aa, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                                       condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_stroke_cerebrovascular_ixn_pct_aa <- preds_stroke_cerebrovascular %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only")

ggsave("figs/05_stroke_cerebrovascular_deaths_ixn_pct_aa.pdf",
       p_stroke_cerebrovascular_ixn_pct_aa)

# Diabetes

f_diabetes_ixn_pct_aa <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_diabetes_deaths_poisson_model_ixn_pct_aa.rds")

pool(f_diabetes_ixn_pct_aa) %>%
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(outcome = "Diabetes death")

preds_diabetes <- lapply(f_diabetes_ixn_pct_aa, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_diabetes_ixn_pct_aa <- preds_diabetes %>% 
  plot() +
  labs(
    x = "Centered and scaled percent private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Centered/scaled percent AA only") +
  scale_fill_manual(values = au_colors, name = "Centered/scaled percent AA only")

ggsave("figs/05_diabetes_deaths_ixn_pct_aa.pdf",
       p_diabetes_ixn_pct_aa)

p_ixn_pct_aa <- p_hypertensive_ixn_pct_aa + p_ischemic_ixn_pct_aa + p_stroke_cerebrovascular_ixn_pct_aa + p_diabetes_ixn_pct_aa
p_ixn_pct_aa

ggsave(filename = "figs/05_combined_plots_ixn_pct_aa.pdf", p_ixn_pct_aa)
