library(magrittr)
library(tidyverse) # load this first for most/all things
library(peacesciencer) # the package of interest
library(stevemisc) # a dependency, but also used for standardizing variables for better interpretation
library(tictoc)
library(countrycode)


create_dyadyears(directed = FALSE, subset_years = c(1990:2022)) %>%
  add_contiguity() %>% # add contiguity information (border type 0-5)
  add_cow_majors() %>% # add major power boolean
  add_atop_alliance() %>% # add atop alliance
  # subset data to politically relevant dyads (PRDs), pipe to next function
  filter_prd() %>%
  # add_gwcode_to_cow() %>%
  # add conflict information from GML-MID data, pipe to next function
  add_gml_mids(keep = NULL) %>%
  # add peace years ("spells"), pipe to next function
  # add_spells() %>%
  # add minimum distance between states, pipe to next function (redundant because of contingecy categorical type)
  add_minimum_distance() %>%
  # add capabilities data, pipe to next function
  # add_nmc() %>%
  # add some estimates about democracy for each state, pipe to next function
  # add_democracy() %>%
  # add information about alliance commitments in dyad-year
  add_cow_alliance() -> Data
# finish with information about population and GDP/SDP
# and then assign to object, called, minimally, 'Data'
# add_sdp_gdp()

Data