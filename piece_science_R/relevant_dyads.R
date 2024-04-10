library(tidyverse)
library(peacesciencer)
library(stevemisc)
library(tictoc)
library(countrycode)
library(magrittr)

# based on min distance between states, tss_rivalries and COW majors create relevant dyads
# Dyad is relevant if at least one of the following conditions is met:
# 1. The min distance is less than 100 km
# 2. The dyad is a rivalry according to TSS data
# 3. At least one of the states is a major power according to COW data

# Dyad is created as follows:
# from cm_features_v0.4.csv read get the country features
# for each month (from 1990 to 2022) create a dyad for each pair of countries according to the conditions above
# The structure of the dyad is as follows:
# month_id, ccode1, ccode2, min_dist, rivalry, a_country_id, a_all_cm_features, b_country_id, b_all_cm_features, a_ged_sb, b_ged_sb
# all_cm_features are all the features from cm_features except
# where ged_sb are fields from cm_features. Remove them from country data and add to tail of the series as they are dependent variable.
# save the dyads to a csv file

# FEATURES: read cm_features_v0.4.csv
cm_features <- read_csv("data/cm_features_v0.4.csv")

# MIN DISTANCE:
gw_cow_years %>% write_csv("data/translation_table_cow_gw_name.csv")
# create_dyadyears and save to a variable
min_dist_dyads <- create_dyadyears(system = "cow", subset_years = c(1990:2022)) %>% add_minimum_distance()

# Add a column for country name
min_dist_dyads <- min_dist_dyads %>%
  mutate(
    country_name1 = countrycode(ccode1, 'cown', 'country.name'),
    country_name2 = countrycode(ccode2, 'cown', 'country.name')
  )

# drop rows where distance is NA
min_dist_dyads <- min_dist_dyads %>%
  filter(!is.na(mindist))

# TSS RIVALRIES:
rivalries <- tss_rivalries %>%
  filter(end >= 1990 & end <= 2022)

# substitute NA with 0
rivalries[is.na(rivalries)] <- 0

# MAJORS:
majors <- maoz_powers %>%
  filter((regenddate >= 1990 | globenddate >= 1990)) %>%
  mutate(
    country.name = countrycode(ccode, "cown", 'country.name'),
    is_major = ifelse(globenddate >= 1991, 1, 0),
    is_reg_power = ifelse(regenddate >= 1990, 1, 0)
  ) %>%
  mutate(across(c(is_major, is_reg_power), ~replace_na(., 0))) %>%  # Convert NA to 0 for 'is_major' and 'is_reg_power'
  filter(!(ccode == 365 & is_major == 1)) %>%  # Drop Russia as a major power but keep as regional power
  mutate(power_since = ifelse(is_reg_power == 1 & !is.na(regstdate), as.character(regstdate), as.character(globstdate))) %>%  # Add 'power_since' column
  select(-globenddate, -regenddate, -globstdate, -regstdate)  # Drop period columns

# create folder if it does not exist
dir.create("data/dyad", showWarnings = FALSE)

# save to CSV
min_dist_dyads %>% write_csv("data/dyad/min_dist_dyads.csv")
rivalries %>% write_csv("data/dyad/rivalries.csv")
majors %>% write_csv("data/dyad/maoz_powers.csv")