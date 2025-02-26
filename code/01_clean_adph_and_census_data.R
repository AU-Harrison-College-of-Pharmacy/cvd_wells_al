library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)
library(sjlabelled)

# Download CBGs and their geometry from 2020 census. Use the cartographic boundary.
cbgs <- block_groups(state = "AL", cb = TRUE, year = 2020) %>%
  janitor::clean_names() %>%
  rename(census_block_group = geoid)

# Load in the raw data from ADPH and clean it
# Some of the death location residence codes are NOT ALABAMA. So you need to check for that. You should exclude deaths that are not in AL.
# Not all CBG x age_group combinations are represented in the data. If a combination is missing, it can safely be assumed that all 4 diseases have counts of 0.

adph_primary <- read_csv("/Volumes/Projects/usgs_cvd_wells_al/data/raw/2024-01-10/2016-2019CensusBlockGroupPrimaryCOD2.csv",
                         col_types = c("c", "c", "i", "i", "i", "i")) %>%
  janitor::clean_names() %>%
  mutate(cbg_age_included_by_adph = 1,
         census_block_group = as.character(census_block_group)) %>%
  filter(str_starts(census_block_group, "01")) %>%  # some deaths in Alabama were for residents of other states. keeping only CBGs in AL.
  complete(census_block_group, age_group) %>%
  mutate(
    cbg_age_included_by_adph = if_else(is.na(cbg_age_included_by_adph), 0, cbg_age_included_by_adph)
  )

# Test that all CBGs have 4 rows associated with them for the 4 age groups. Test should return a tibble with 0 rows.
adph_primary %>%
  group_by(census_block_group) %>%
  summarise(n = n()) %>%
  filter(n != 4)

# Test that all CBGs have length 12. Test should return a tibble with 0 rows.
adph_primary %>% mutate(l = str_length(census_block_group)) %>% filter(l != 12)

# Next we will get population data according to the 2020 Census. The Census organizes these files in a strange way, with counts by age group of males and females spread across different variables. By combining the male and female counts together, we can get a count for one age group. Also, sometimes the age groups are split up in strange ways.
cbg_age_populations <- get_decennial(geography = "cbg", 
                                     state = "AL",
                                     variables = c("P12_015N", "P12_016N", "P12_017N", "P12_018N", "P12_019N", "P12_020N", "P12_021N", "P12_022N", "P12_023N", "P12_024N", "P12_025N",
                                                   "P12_039N", "P12_040N", "P12_041N", "P12_042N", "P12_043N", "P12_044N", "P12_045N", "P12_046N", "P12_047N", "P12_048N", "P12_049N"),
                                     sumfile = "dhc")

df_age_populations <- cbg_age_populations %>%
  mutate(age_group = case_when(
    variable %in% c("P12_015N", "P12_016N", "P12_039N", "P12_040N") ~ "45 - 54 yrs",
    variable %in% c("P12_017N", "P12_018N", "P12_019N", "P12_041N", "P12_042N", "P12_043N") ~ "55 - 64 yrs",
    variable %in% c("P12_020N", "P12_021N", "P12_022N", "P12_044N", "P12_045N", "P12_046N") ~ "65 - 74 yrs",
    variable %in% c("P12_023N", "P12_024N", "P12_025N", "P12_047N", "P12_048N", "P12_049N") ~ "75 or over"
  )) %>%
  group_by(GEOID, age_group) %>%
  summarise(
    population_estimate = sum(value)
  ) %>%
  rename(census_block_group = GEOID) %>%
  ungroup()

df <- left_join(cbgs, adph_primary,
                by = "census_block_group") %>%
  
  # This is going to get a little weird. The problem is that now for CBGs that weren't in the ADPH data, the age group is now missing because a match for the CBG in census data couldn't be found in ADPH data. So we will assign the missing ones to be a specific value, then do the dplyr::complete() to complete all of the CBG x age_group combinations again.
  mutate(
    age_group = if_else(is.na(age_group), "45 - 54 yrs", age_group)
  ) %>%
  complete(census_block_group, age_group) %>%
  mutate(
    cbg_age_included_by_adph = if_else(is.na(cbg_age_included_by_adph), 0, cbg_age_included_by_adph)
  ) %>%
  select(-c(statefp, countyfp, tractce, blkgrpce, affgeoid, name, namelsad, lsad)) %>%
  mutate(hypertensive_deaths = if_else(cbg_age_included_by_adph == 0, 0, hypertensive_deaths),
         ischemic_deaths = if_else(cbg_age_included_by_adph == 0, 0, ischemic_deaths),
         stroke_cerebrovascular_deaths = if_else(cbg_age_included_by_adph == 0, 0, stroke_cerebrovascular_deaths),
         diabetes_deaths = if_else(cbg_age_included_by_adph == 0, 0, diabetes_deaths)) %>%
  left_join(., df_age_populations,
            by = c("census_block_group", "age_group")) %>%
  as_tibble() %>%
  rename(
    id_census_block_group = census_block_group,
    amt_area_land = aland,
    amt_area_water = awater,
    cat_age_group = age_group,
    n_hypertensive_deaths = hypertensive_deaths,
    n_ischemic_deaths = ischemic_deaths,
    n_stroke_cerebrovascular_deaths = stroke_cerebrovascular_deaths,
    n_diabetes_deaths = diabetes_deaths,
    ind_cbg_age_group_included_by_adph = cbg_age_included_by_adph,
    n_population = population_estimate
  ) %>%
  mutate(
    amt_population_density_per_km2_per_age_group = n_population / amt_area_land * 1000 * 1000,  # change to per 1 km^2
    cat_hypertensive_deaths = case_when(
      n_hypertensive_deaths == 0 ~ "0",
      n_hypertensive_deaths >= 6 ~ "6 or more",
      is.na(n_hypertensive_deaths) ~ "1 - 5"
    ),
    cat_ischemic_deaths = case_when(
      n_ischemic_deaths == 0 ~ "0",
      n_ischemic_deaths >= 6 ~ "6 or more",
      is.na(n_ischemic_deaths) ~ "1 - 5"
    ),
    cat_stroke_cerebrovascular_deaths = case_when(
      n_stroke_cerebrovascular_deaths == 0 ~ "0",
      n_stroke_cerebrovascular_deaths >= 6 ~ "6 or more",
      is.na(n_stroke_cerebrovascular_deaths) ~ "1 - 5"
    ),
    cat_diabetes_deaths = case_when(
      n_diabetes_deaths == 0 ~ "0",
      n_diabetes_deaths >= 6 ~ "6 or more",
      is.na(n_diabetes_deaths) ~ "1 - 5"
    )
  ) %>%
  mutate(
    cat_age_group = as_factor(cat_age_group, order) %>% fct_relevel("45 - 54 yrs", "55 - 64 yrs", "65 - 74 yrs") %>% as.ordered()
  ) %>%
  var_labels(
    id_census_block_group = "Census block group FIPS according to 2020 Census",
    amt_area_land = "Area of CBG that is land (m^2)",
    amt_area_water = "Are of CBG that is water (m^2)",
    cat_age_group = "Age group (years)",
    n_hypertensive_deaths = "Number of deaths from 2016 to 2019 with primary cause of death listed as hypertensive heart disease (I10 - I15). Missing values indicate a suppressed count between 1 and 5, inclusive.",
    n_ischemic_deaths = "Number of deaths from 2016 to 2019 with primary cause of death listed as ischemic heart disease (I20 - I25). Missing values indicate a suppressed count between 1 and 5, inclusive.",
    n_stroke_cerebrovascular_deaths = "Number of deaths from 2016 to 2019 with primary cause of death listed as stroke or cerebrovascular diseases (I60 - I69). Missing values indicate a suppressed count between 1 and 5, inclusive.",
    n_diabetes_deaths = "Number of deaths from 2016 to 2019 with primary cause of death listed as diabetes (E10 - E14). Missing values indicate a suppressed count between 1 and 5, inclusive.",
    ind_cbg_age_group_included_by_adph = "Indicator variable for whether this CBG x age_group combination was included in the ADPH data. All death counts are forced to 0 when the value of this variable is 0.",
    geometry = "The sf geometry polygon of the CBG according to 2020 Census.",
    n_population = "Total population estimate in CBG according to 2020 Census.",
    cat_hypertensive_deaths = "Categorical counts of hypertensive deaths: 0, 1 - 5, or >= 6",
    cat_ischemic_deaths = "Categorical counts of ischemic deaths: 0, 1 - 5, or >= 6",
    cat_stroke_cerebrovascular_deaths = "Categorical counts of stroke or cerebrovascular deaths: 0, 1 - 5, or >= 6",
    cat_diabetes_deaths = "Categorical counts of diabetes deaths: 0, 1 - 5, or >= 6",
    amt_population_density_per_km2_per_age_group = "Population density per 1 km^2 (by age group)"
  )

write_rds(df, file = "/Volumes/Projects/usgs_cvd_wells_al/data/clean/01_clean_adph_census_primary_cod.rds")
