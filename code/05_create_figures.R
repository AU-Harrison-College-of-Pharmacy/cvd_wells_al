library(tidyverse)
library(lme4)
library(ggeffects)

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")

preds_hypertensive <- lapply(f_hypertensive, predict_response, terms = c("amt_centered_scaled_mean_pct_wells_cbg", "cat_age_group"),
                         condition = c(n_population_times_4 = 100000)) %>%
  pool_predictions()

p_hypertensive <- preds_hypertensive %>% 
  plot() +
  labs(
    x = "Centered and scaled percent prival well use",
    y = "Hypertensive deaths per 100,000", 
    title = ""
  )

ggsave("figs/05_hypertensive_deaths.pdf",
       p_hypertensive)