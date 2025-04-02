library(tidyverse)
library(sjlabelled)
library(sf)

# Read in private well raster data


# Read in ADPH mortality data by CBG
mortality <- read_rds(file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/01_clean_adph_census_primary_cod.rds")

# Join private well data to ADPH mortality CBG

wells_blocks <- read_csv("/Volumes/Projects/usgs_cvd_wells_al/data/raw/Well_Estimates_2020_Blocks.txt")

wells_block_groups <- wells_blocks %>%
  filter(State == "AL") %>% 
  arrange(GEOID_BlockGroup) %>% 
  group_by(GEOID_BlockGroup) %>% 
  summarise(amt_mean_pct_wells_cbg = mean(Pct_Wells, na.rm = TRUE)) %>%
  rename(id_census_block_group = GEOID_BlockGroup) %>%
  mutate(
    amt_centered_scaled_mean_pct_wells_cbg = scale(amt_mean_pct_wells_cbg)[, 1]
  ) %>%
  mutate_all(~ifelse(is.nan(.), NA_real_, .))

df <- mortality %>%
  left_join(., wells_block_groups) %>%
  mutate(
    is_population_greater_than_0 = if_else(n_population == 0, 0, 1),
    is_pct_wells_nonmissing = if_else(is.na(amt_mean_pct_wells_cbg), 0, 1),
    is_included_in_analysis = if_else(is_population_greater_than_0 == 1 &
                                      is_pct_wells_nonmissing == 1, 1, 0),
    amt_centered_scaled_area_land = scale(amt_area_land)[, 1]
  ) %>%
  var_labels(amt_mean_pct_wells_cbg = "Percent of housing units relying on private wells in the given CBG. This percent is a mean of the percents across all Census blocks from USEPA/ORD_Water_Source_2020 GitHub repository.",
             amt_centered_scaled_mean_pct_wells_cbg = "Centered and scaled version of amt_mean_pct_wells_cbg.",
             is_included_in_analysis = "Final exclusion variable to identify which CBG x age group combinations are included in analysis",
             amt_centered_scaled_area_land = "Centered and scaled version of CBG land area (originally in m^2)"
             )

# Join physiographic region data for each CBG
# There are 3,924 observations, which is the expected number based on the CBGs Census data. One CBG has a missing physiographic region (010979900000). It is Gaillard Island in the middle of Mobile Bay. It is already excluded from the analysis because it's population is 0 and it has missing well water data.
pr <- read_csv("/Volumes/Projects/usgs_cvd_wells_al/data/raw/centered_geospatial_data.csv") %>%
  janitor::clean_names() %>%
  select(geoid, pr) %>%
  mutate(pr = factor(pr)) %>%
  rename(
    id_census_block_group = geoid,
    cat_physiographic_region = pr
  ) %>%
  var_labels(
    id_census_block_group = "Census block group FIPS according to 2020 Census",
    cat_physiographic_region = "Physiographic region according to USGS"
  )

df_pr <- left_join(df, pr, by = "id_census_block_group")

# Test that the counts of physiographic regions are the same in the pr dataset and in this joined df_pr dataset. This test should return a Boolean vector of length 6 with all values equal to `TRUE`.
count(pr, cat_physiographic_region) %>% pull(n) == count(df_pr, cat_physiographic_region) %>% pull(n) / 4  # We divide by 4 because this larger dataset repeats the physiographic region four times for each CBG, since there are 4 different rows of age groups for each CBG

# Write out the dataset

write_rds(df_pr,
          file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/02_analysis_dataset.rds")
