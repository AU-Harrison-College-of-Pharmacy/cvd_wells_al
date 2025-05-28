library(tidyverse)
library(sf)
library(spdep)

df <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")

df_st <- 

# Hypertension

f_hypertensive <- read_rds("/Volumes/Projects/usgs_cvd_wells_al/output/04_hypertension_deaths_poisson_model.rds")
str(f_hypertensive)
