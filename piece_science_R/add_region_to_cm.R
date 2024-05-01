library(magrittr)
library(tidyverse) # load this first for most/all things
library(peacesciencer) # the package of interest
library(stevemisc) # a dependency, but also used for standardizing variables for better interpretation
library(tictoc)
library(countrycode)
library(ggplot2)

# read cm_features.csv
cm_features <- read_csv("data/cm_features_v0.6.csv")

# based on countrycode package, add region to cm_features data
cm_features <- cm_features %>%
  mutate(region = countrycode::countrycode(ccode, origin = "cown", destination = "region")) %>%
  mutate(region23 = countrycode::countrycode(ccode, origin = "cown", destination = "region23"))

# set Southern Europe for Serbia ccode 345 for region23
cm_features <- cm_features %>%
  mutate(region23 = ifelse(ccode == 345, "Southern Europe", region23))

# get unique regions
unique_regions7 <- unique(cm_features$region)
unique_regions23 <- unique(cm_features$region23)

# print rows with NA region
rows_with_na_region <- cm_features %>%
  filter(is.na(region))
rows_with_na_region23 <- cm_features %>%
  filter(is.na(region23))


# assert there are no rows with NA region
stopifnot(nrow(rows_with_na_region) == 0)
stopifnot(nrow(rows_with_na_region23) == 0)
# print done
print("Success")
# save to csv
cm_features %>% write_csv("data/cm_features_v0.7.csv")
print("Saved to data/cm_features_v0.7.csv")