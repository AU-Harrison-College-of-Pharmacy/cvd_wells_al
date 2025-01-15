library(tidyverse)
library(sf)
library(stars)

# Read in private well raster data


# Read in ADPH mortality data by CBG
mortality <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/01_clean_adph_census_primary_cod.rds")

# Join private well data to ADPH mortality CBG
wells <- read_csv(file = "/Volumes/Projects/usgs_cvd_wells_al/data/raw/2020_wells_20250115.csv") %>%
  filter(State == "Alabama") # there are only half as many CBGs in as we would expect. THere are 3,924 CBGs in 2020 Census but only 2,072 here. Apparently these estimates are for the 2010 CBGs, not 2020 CBGs, even though the predict well density is for the year 2020.

Hmisc::contents(wells)

wells_small <- wells %>%
  select(GEOID, State, `2020 Housing Units`,`2020 Population`, `% Well Use - 2020`) %>%
  rename(
    census_block_group = GEOID
  )

# Write out the dataset