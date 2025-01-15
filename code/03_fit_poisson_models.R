library(tidyverse)
library(Amelia)
library(lme4)
library(ggeffects)

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

a <- df %>% 
  mutate(#c = scale(mean_pct_wells_cbg), 
         p = population_estimate * 4,
         age_group = as_factor(age_group) %>% fct_relevel("45 - 54 yrs", "55 - 64 yrs", "65 - 74 yrs", "75 or over") %>% as.ordered() %>% as.integer()) %>% 
  filter(!(population_estimate == 0)) %>%
  select(census_block_group, mean_pct_wells_cbg, p, age_group, hypertensive_deaths)

bds_df <- matrix(c(5, 1, 5),
                 nrow = 1,
                 ncol = 3,
                 byrow = TRUE)

a_df <- amelia(as.data.frame(a),
               m = 10,
               idvars = c("census_block_group"),
               bounds = bds_df)

f_imputed <- lapply(1:10, function(i){
  f <- glmer(round(hypertensive_deaths) ~ (1 | census_block_group) + age_group + mean_pct_wells_cbg + offset(log(p)),
             family = poisson(link = "log"),
             data = a_df$imputations[[i]],
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
})

predict_response(f_imputed[[1]], 
                 terms = c("mean_percent_wells_cbg"),
                 condition = c(p = 100000)
                 ) %>%
  plot()

predict_response(f_imputed[[2]], 
                 terms = c("c"),
                 condition = c(p = 100000)
) %>%
  plot()

p_hypertensive <- lapply(f_imputed, predict_response, terms = c("mean_pct_wells_cbg [0:100]", "age_group"),
                         condition = c(p = 100000)) %>%
  pool_predictions()  # getting an error for some reason

p_hypertensive %>% plot()

