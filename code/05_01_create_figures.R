
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
       p_hypertensive_sensitivity_area, width= 6, height=4)

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
       p_ischemic_sensitivity_area, width= 6, height=4)

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
       p_stroke_cerebrovascular_sensitivity_area, width= 6, height=4)

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
       p_diabetes_sensitivity_area, width= 6, height=4)

p_sensitivity_area <- p_hypertensive_sensitivity_area + p_ischemic_sensitivity_area + p_stroke_cerebrovascular_sensitivity_area + p_diabetes_sensitivity_area
p_sensitivity_area

ggsave(filename = "figs/05_01_combined_plots_sensitivity_area_inla.pdf", p_sensitivity_area, width= 12, height=5.5)

#########
# Interaction with percent reporting AA race alone
#########

# Hypertension

preds_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_hypertensive_ixn_pct_aa <- preds_hypertensive %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
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

ggsave("figs/05_01_hypertensive_deaths_ixn_pct_aa_inla.pdf",
       p_hypertensive_ixn_pct_aa, width= 6, height=4)

# Ischemic

preds_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_ischemic_ixn_pct_aa <- preds_ischemic %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
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

ggsave("figs/05_01_ischemic_deaths_ixn_pct_aa.pdf",
       p_ischemic_ixn_pct_aa, width= 6, height=4)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_stroke_cerebrovascular_ixn_pct_aa <- preds_stroke_cerebrovascular %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
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

ggsave("figs/05_01_stroke_cerebrovascular_deaths_ixn_pct_aa.pdf",
       p_stroke_cerebrovascular_ixn_pct_aa, width= 6, height=4)

# Diabetes

preds_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_ixn_pct_aa_inla.rds")

p_diabetes_ixn_pct_aa <- preds_diabetes %>% 
  mutate(amt_centered_scaled_pct_aa_only = as.factor(amt_centered_scaled_pct_aa_only)) %>%
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
  theme(axis.line = element_line(color = "lightgray"))

ggsave("figs/05_01_diabetes_deaths_ixn_pct_aa.pdf",
       p_diabetes_ixn_pct_aa, width= 6, height=4) +
  guides(
    color = guide_legend(reverse = TRUE),
    fill = guide_legend(reverse = TRUE)
  )

p <- p_hypertensive_ixn_pct_aa + p_ischemic_ixn_pct_aa + p_stroke_cerebrovascular_ixn_pct_aa + p_diabetes_ixn_pct_aa
p

ggsave(filename = "figs/05_01_combined_plots_deaths_ixn_pct_aa_inla.pdf", p, width= 12, height=5.5)

p_no_diabetes <- wrap_plots(list(p_hypertensive_ixn_pct_aa, p_ischemic_ixn_pct_aa), ncol = 1)

ggsave(filename = "figs/05_01_combined_plots_deaths_ixn_pct_aa_inla_no_diabetes.png", p_no_diabetes, width= 5, height=4, dpi = 300)


#########
# Interaction with age group
#########

# Hypertension

preds_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_hypertension_deaths_poisson_model_ixn_age_group_inla.rds")

p_hypertensive_ixn_pct_aa <- preds_hypertensive %>% 
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

ggsave("figs/05_01_hypertensive_deaths_ixn_age_group_inla.pdf",
       p_hypertensive_ixn_pct_aa, width= 6, height=4)

# Ischemic

preds_ischemic <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_ischemic_deaths_poisson_model_ixn_age_group_inla.rds")

p_ischemic_ixn_pct_aa <- preds_ischemic %>% 
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

ggsave("figs/05_01_ischemic_deaths_ixn_age_group_inla.pdf",
       p_ischemic_ixn_pct_aa, width= 6, height=4)

# Stroke/cerebrovascular

preds_stroke_cerebrovascular <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_stroke_cerebrovascular_deaths_poisson_model_ixn_age_group_inla.rds")

p_stroke_cerebrovascular_ixn_pct_aa <- preds_stroke_cerebrovascular %>% 
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

ggsave("figs/05_01_stroke_cerebrovascular_deaths_ixn_age_group_inla.pdf",
       p_stroke_cerebrovascular_ixn_pct_aa, width= 6, height=4)

# Diabetes

preds_diabetes <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/05_preds_diabetes_deaths_poisson_model_ixn_age_group_inla.rds")

p_diabetes_ixn_pct_aa <- preds_diabetes %>% 
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

ggsave("figs/05_01_diabetes_deaths_ixn_age_group_inla.pdf",
       p_diabetes_ixn_pct_aa, width= 6, height=4)