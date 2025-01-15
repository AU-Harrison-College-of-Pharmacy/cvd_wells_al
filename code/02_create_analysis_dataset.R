library(tidyverse)
library(sjlabelled)

# Read in private well raster data


# Read in ADPH mortality data by CBG
mortality <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/01_clean_adph_census_primary_cod.rds")

# Join private well data to ADPH mortality CBG

wells_blocks <- read_csv("/Volumes/Projects/usgs_cvd_wells_al/data/raw/Well_Estimates_2020_Blocks.txt")

wells_block_groups <- wells_blocks %>%
  filter(State == "AL") %>% 
  arrange(GEOID_BlockGroup) %>% 
  group_by(GEOID_BlockGroup) %>% 
  summarise(mean_pct_wells_cbg = mean(Pct_Wells, na.rm = TRUE)) %>%
  rename(census_block_group = GEOID_BlockGroup)

df <- mortality %>%
  left_join(., wells_block_groups) %>%
  var_labels(mean_pct_wells_cbg = "Percent of housing units relying on private wells in the given CBG. This percent is a mean of the percents across all Census blocks from USEPA/ORD_Water_Source_2020 GitHub repository.")

# Write out the dataset

write_rds(df,
          file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")
