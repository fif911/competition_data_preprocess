library(magrittr)
library(tidyverse) # load this first for most/all things
library(peacesciencer) # the package of interest
library(stevemisc) # a dependency, but also used for standardizing variables for better interpretation
library(tictoc)
library(countrycode)
library(ggplot2)

# read cm_features with name
cm_features <- read_csv("data/cm_features_v0.4.csv")

manually_excluded_columns <- c("month_id", "country_id", "date", 'year', 'ccode', 'country', 'gw_statename', 'gleditsch_ward')

# Dynamically find columns that contain the word "decay|splag"
columns_with_decay <- names(cm_features)[grep("decay|splag", names(cm_features))]

dependent_variable_columns <- names(cm_features)[grep("ged|acled", names(cm_features))]

# Combine manually specified exclusions with those that contain "decay"
columns_to_exclude <- unique(c(manually_excluded_columns, columns_with_decay, dependent_variable_columns))

# Identify all other columns (i.e., those not to exclude)
columns_to_check <- setdiff(names(cm_features), columns_to_exclude)

# cat("Calculating ...")
# Filter rows where all specified columns are set to 0
# rows_with_all_zeros <- cm_features %>%
#   rowwise() %>%
#   filter(all(across(all_of(columns_to_check), ~.x == 0))) %>%
#   ungroup()
# # Print the result
# if (nrow(rows_with_all_zeros) > 0) {
#   print(rows_with_all_zeros)
# } else {
#   cat("No rows found where all columns are set to 0, excluding", paste(columns_to_exclude, collapse = ", "), ".\n")
# }


# cm_features_with_missing_percentage <- cm_features %>%
#   rowwise() %>%
#   mutate(Zero_Percentage = sum(across(all_of(columns_to_check), ~.x == 0)) / length(columns_to_check)) %>%
#   ungroup()


# cat("Calculating ...\n")
# # Filter rows where more than 90% of specified columns are set to 0
# rows_with_mostly_zeros <- cm_features %>%
#   rowwise() %>%
#   filter((sum(across(all_of(columns_to_check), ~.x == 0)) / length(columns_to_check)) > 0.9) %>%
#   ungroup()
#
# # Print the result
# if (nrow(rows_with_mostly_zeros) > 0) {
#   print(rows_with_mostly_zeros)
# } else {
#   cat("No rows found where more than 90% of columns are set to 0, excluding", paste(columns_to_exclude, collapse = ", "), ".\n")
# }


# unique months id
# kosovo = cm_features %>%
#   filter(country_id == 232) %>%
#   select(month_id) %>%
#   distinct()

# rows_with_all_zeros %>%
#   filter(country_id == 187) %>%
#   select(month_id) %>%
#   distinct() == cm_features %>%
#   filter(country_id == 187) %>%
#   select(month_id) %>%
#   distinct()

# find countries for which ged_sb, acled_sb, ged_os, acled_os are all 0 for all months
# Todo this:
# sum each column for each country over all months. If the sum is 0, then all values are 0 for that country
# so 5 sums should be 0 for a country to be selected
country_sums <- cm_features %>%
  group_by(country_id, gw_statename, ccode) %>%
  summarise(
    sum_ged_sb = sum(ged_sb),
    sum_acled_sb = sum(acled_sb),
    sum_ged_os = sum(ged_os),
    sum_acled_os = sum(acled_os),
    months_considered = n()
  ) %>%
  ungroup()

# Check if all sums are 0 for each country
countries_all_zeros <- country_sums %>%
  filter(
    sum_ged_sb == 0,
    sum_acled_sb == 0,
    sum_ged_os == 0,
    sum_acled_os == 0
  )

cm_features %>% filter(ccode == 860)
# TODO: drop east timor, cape verde,Malta, Luxembourg, Maldives, Fiji, German Federal Republic 3 months,
# Serbia 21 (investigare)
timor = cm_features %>% filter(ccode == 860)
serbia = cm_features %>% filter(ccode == 345)