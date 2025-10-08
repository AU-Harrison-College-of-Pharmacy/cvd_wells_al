library(tidyverse)
library(patchwork)
library(broom.mixed)
library(ggeffects)
library(sf)

# Prediction plots

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

m <- mean(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)
s <- sd(df$amt_mean_pct_wells_cbg, na.rm = TRUE) %>% round(1)

au_colors <- c("#ffc044", "#e86100", "#0093d2", "#0b2341", "#00a597")

#########
# Interaction with physiographic region
#########

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_hypertension_deaths_poisson_model_ixn_pct_aa.rds")

preds_hypertensive <- pool_predictions(lapply(f_hypertensive, 
                                              predict_response, 
                                              terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only [-1, 0, 1]", "cat_age_group"),
                                              condition = c(n_population_times_4 = 100000)), 
)

p_hypertensive <- preds_hypertensive %>% 
  filter(facet == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Hypertensive deaths per 100,000",
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Percent African American") +
  scale_fill_manual(values = au_colors, name = "Percent African American") + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_hypertensive_deaths_ixn_pct_aa.pdf",
       p_hypertensive, width= 6, height=4)

## Obtain estimates - estimates the ratio of the rate ratios among the physiographic regions. If the are near 1, then the rate ratios for the association with well percentage is similar across all physiographic regions
mice::pool(f_hypertensive) %>% 
  broom::tidy() %>%
  as_tibble() %>%
  filter(str_detect(term, "well")) %>%
  mutate(
    rr = exp(estimate),
    rr.low = exp(estimate - 2 * std.error),
    rr.high = exp(estimate + 2 * std.error)
  ) %>%
  select(term, contains("rr"))

# Ischemic

f_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_ischemic_deaths_poisson_model_ixn_pct_aa.rds")

preds_ischemic <- pool_predictions(lapply(f_ischemic, 
                                          predict_response, 
                                          terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only [-1, 0, 1]", "cat_age_group"),
                                          condition = c(n_population_times_4 = 100000)), 
)

p_ischemic <- preds_ischemic %>% 
  filter(facet == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Ischemic deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Percent African American") +
  scale_fill_manual(values = au_colors, name = "Percent African American") + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_ischemic_deaths_ixn_pct_aa.pdf",
       p_ischemic, width= 6, height=4)

## Obtain estimates - estimates the ratio of the rate ratios among the physiographic regions. If the are near 1, then the rate ratios for the association with well percentage is similar across all physiographic regions
mice::pool(f_ischemic) %>% 
  broom::tidy() %>%
  as_tibble() %>%
  filter(str_detect(term, "well")) %>%
  mutate(
    rr = exp(estimate),
    rr.low = exp(estimate - 2 * std.error),
    rr.high = exp(estimate + 2 * std.error)
  ) %>%
  select(term, contains("rr"))

# Stroke/cerebrovascular

f_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa.rds")

preds_stroke_cerebrovascular <- pool_predictions(lapply(f_stroke_cerebrovascular, 
                                                        predict_response, 
                                                        terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only [-1, 0, 1]", "cat_age_group"),
                                                        condition = c(n_population_times_4 = 100000)), 
)

p_stroke_cerebrovascular <- preds_stroke_cerebrovascular %>% 
  filter(facet == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Stroke/cerebrovascular deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Percent African American") +
  scale_fill_manual(values = au_colors, name = "Percent African American") + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_stroke_cerebrovascular_deaths_ixn_pct_aa.pdf",
       p_stroke_cerebrovascular, width= 6, height=4)

## Obtain estimates - estimates the ratio of the rate ratios among the physiographic regions. If the are near 1, then the rate ratios for the association with well percentage is similar across all physiographic regions
mice::pool(f_stroke_cerebrovascular) %>% 
  broom::tidy() %>%
  as_tibble() %>%
  filter(str_detect(term, "well")) %>%
  mutate(
    rr = exp(estimate),
    rr.low = exp(estimate - 2 * std.error),
    rr.high = exp(estimate + 2 * std.error)
  ) %>%
  select(term, contains("rr"))

# Diabetes

f_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_02_preds_diabetes_deaths_poisson_model_ixn_pct_aa.rds")

preds_diabetes <- pool_predictions(lapply(f_diabetes, 
                                          predict_response, 
                                          terms = c("amt_centered_scaled_mean_pct_wells_cbg [-0.78:2.9]", "amt_centered_scaled_pct_aa_only [-1, 0, 1]", "cat_age_group"),
                                          condition = c(n_population_times_4 = 100000)), 
)

p_diabetes <- preds_diabetes %>% 
  filter(facet == "75 or over") %>%
  plot() +
  labs(
    x = "Percentage private well use",
    y = "", 
    title = "Diabetes deaths per 100,000"
  ) +
  scale_x_continuous(labels = c(paste0(m - 0.78 * s), paste0(m), paste0(m + s), paste0(m + 2 * s))) +
  scale_color_manual(values = au_colors, name = "Percent African American") +
  scale_fill_manual(values = au_colors, name = "Percent African American") + 
  theme_minimal() +
  theme(axis.line = element_line(color = "lightgray")) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

ggsave("figs/05_02_diabetes_deaths_ixn_pct_aa.pdf",
       p_diabetes, width= 6, height=4)

## Obtain estimates - estimates the ratio of the rate ratios among the physiographic regions. If the are near 1, then the rate ratios for the association with well percentage is similar across all physiographic regions
mice::pool(f_diabetes) %>% 
  broom::tidy() %>%
  as_tibble() %>%
  filter(str_detect(term, "well")) %>%
  mutate(
    rr = exp(estimate),
    rr.low = exp(estimate - 2 * std.error),
    rr.high = exp(estimate + 2 * std.error)
  ) %>%
  select(term, contains("rr"))

# Combine plots

p <- wrap_plots(p_hypertensive + theme(legend.position = "left"), p_ischemic + theme(legend.position = "none"), p_stroke_cerebrovascular  + theme(legend.position = "none"), p_diabetes  + theme(legend.position = "none"))

ggsave("figs/05_02_combined_plots_ixn_pct_aa.pdf",
       p,
       width = 11, height = 5)
