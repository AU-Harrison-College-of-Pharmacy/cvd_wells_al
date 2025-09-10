a = df_hypertensive$imputations[[1]]



library(WeightIt)

names(a)

m.out0 <- weightit(amt_centered_scaled_mean_pct_wells_cbg ~ cat_age_group + n_population_times_4 + cat_physiographic_region + amt_centered_scaled_pct_aa_only + cat_rural + amt_percent_agricultural_land_use_centered_scaled,
                  data = a,
                  method = "bart")

bal.tab(amt_centered_scaled_mean_pct_wells_cbg ~ cat_age_group +  n_population_times_4 + cat_physiographic_region + amt_centered_scaled_pct_aa_only + cat_rural + amt_percent_agricultural_land_use_centered_scaled,
        data = a,
        estimand = "ATT",
        thresholds = c(m = 0.05))

m.out0

library(cobalt)

bal.tab(m.out0, un = TRUE)

summary(m.out0)

t_w <- trim(m.out0, at = 0.95)

bal.tab(t_w, un = TRUE)

summary(t_w)

f <- glm_weightit(n_hypertensive_deaths ~ amt_centered_scaled_mean_pct_wells_cbg * (cat_age_group + cat_rural) + offset(log(n_population_times_4)),
                  data = a,
                  weightit = t_w)

summary(f)

library(marginaleffects)
avg_comparisons(f, variables = "amt_centered_scaled_mean_pct_wells_cbg")
