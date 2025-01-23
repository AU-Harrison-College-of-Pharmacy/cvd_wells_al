library(tidyverse)
library(Amelia)
library(lme4)

df_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/03_imputed_hypertensive_deaths.rds")

f_hypertensive <- lapply(1:length(df_hypertensive$imputations), function(i){
  f <- glmer(n_hypertensive_deaths ~ (1 | id_census_block_group) + cat_age_group + amt_centered_scaled_mean_pct_wells_cbg + offset(log(n_population_times_4)),
             family = poisson(link = "log"),
             data = df_hypertensive$imputations[[i]],
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

write_rds(f_hypertensive, 
          "/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")