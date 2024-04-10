library(tidyverse) # load this first for most/all things
library(peacesciencer) # the package of interest
library(stevemisc) # a dependency, but also used for standardizing variables for better interpretation
library(tictoc)
library(countrycode)
library(magrittr)
# library("states") # for panel data
# cow_ddy %>% add_minimum_distance()

# install.packages("countrycode") # for converting country codes

gw_cow_years %>% write_csv("data/translation_table_cow_gw_name.csv")
# create_dyadyears and save to a variable
dyadyears <- create_dyadyears(system = "cow", subset_years = c(1990:2022)) %>% add_minimum_distance()

# Add a column for country name beside the GW country code
dyadyears <- dyadyears %>%
  mutate(
    # country_name1 = countrycode(gwcode1, 'gwn', 'country.name'),
    # country_name2 = countrycode(gwcode2, 'gwn', 'country.name')
    country_name1 = countrycode(ccode1, 'cown', 'country.name'),
    country_name2 = countrycode(ccode2, 'cown', 'country.name')
  )

# drop rows where distance is NA
dyadyears <- dyadyears %>%
  filter(!is.na(mindist))

dyadyears %>% write_csv("data/min dist COW.csv")
stop()

# check if the min distance between countries changes over time
temp_dyadyears <- dyadyears %>%
  group_by(ccode1, ccode2) %>%
  mutate(
    prev_mindist = lag(mindist),
    distance_change = mindist != prev_mindist
  ) %>%
  ungroup()

# Filter dyads where distance changes and select unique dyads
dyads_distance_changed <- temp_dyadyears %>%
  filter(distance_change) %>%
  distinct(ccode1, ccode2, .keep_all = TRUE)

# Optionally, print or write these dyads to a CSV
print(dyads_distance_changed)

# save to CSV
# dyadyears %>% write_csv("data/min dist COW.csv")
# stop after this line


missing_country_names <- dyadyears %>%
  filter(is.na(country_name1) | is.na(country_name2))

num_missing_rows <- nrow(missing_country_names)

# Calculate the total number of rows in the dataset
total_rows <- nrow(dyadyears)

# Calculate the percentage of missing values
percentage_missing <- (num_missing_rows / total_rows) * 100

# Print the results
cat("Number of rows with missing country names:", num_missing_rows, "\n")
cat("Percentage of rows with missing country names:", percentage_missing, "%\n")

# Collect the unique GW codes from both columns
missing_codes1 <- unique(missing_country_names$gwcode1[is.na(missing_country_names$country_name1)])
missing_codes2 <- unique(missing_country_names$gwcode2[is.na(missing_country_names$country_name2)])

# Combine and find the unique GW codes across both lists
all_missing_codes <- unique(c(missing_codes1, missing_codes2))

# Print the aggregated missing GW country codes
print(all_missing_codes)

missing_by_year <- missing_country_names %>%
  group_by(year) %>%
  summarise(count = n())

# Plot the distribution of missing country names over years
ggplot(missing_by_year, aes(x = year, y = count)) +
  geom_line() + # Line plot to show the trend
  geom_point() + # Points to highlight individual years
  theme_minimal() + # Minimal theme for a clean look
  labs(title = "Distribution of Missing Country Names by Year",
       x = "Year",
       y = "Count of Missing Country Names")

# save to CSV data(gwstates)
# data(gwstates) %>% write_csv("data/GW_STATES.csv")

# data(cowstates)
#
# create_dyadyears(system = "gw") %>% add_minimum_distance()


# create_stateyears(system ="gw") %>% add_minimum_distance()

# tic()
# create_stateyears(system = 'gw') %>%
#   filter(year %in% c(1946:2019)) %>%
#   add_ucdp_acd(type=c("intrastate"), only_wars = FALSE) %>%
#   add_peace_years() %>%
#   add_democracy() %>%
#   add_creg_fractionalization() %>%
#   add_sdp_gdp() %>%
#   add_rugged_terrain() -> Data
#
# create_stateyears(system = 'gw') %>%
#   filter(year %in% c(1946:2019)) %>%
#   add_ucdp_acd(type=c("intrastate"), only_wars = TRUE) %>%
#   add_peace_years() %>%
#   rename_at(vars(ucdpongoing:ucdpspell), ~paste0("war_", .)) %>%
#   left_join(Data, .) -> Data
#
# Data %>%
#   arrange(gwcode, year) %>%
#   group_by(gwcode) %>%
#   mutate_at(vars("xm_qudsest", "wbgdppc2011est",
#                  "wbpopest"), list(l1 = ~lag(., 1))) %>%
#   rename_at(vars(contains("_l1")),
#             ~paste("l1", gsub("_l1", "", .), sep = "_") ) -> Data
#
# modCW <- list()
# broom::tidy(modCW$"All UCDP Conflicts" <- glm(ucdponset ~ l1_wbgdppc2011est + l1_wbpopest  +
#                     l1_xm_qudsest + I(l1_xm_qudsest^2) +
#                     newlmtnest + ethfrac + relfrac +
#                     ucdpspell + I(ucdpspell^2) + I(ucdpspell^3), data=subset(Data),
#                   family = binomial(link="logit")))
# #> # A tibble: 11 × 5
# #>    term                 estimate std.error statistic  p.value
# #>    <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
# #>  1 (Intercept)        -5.10      1.35         -3.77  0.000160
# #>  2 l1_wbgdppc2011est  -0.285     0.110        -2.59  0.00953
# #>  3 l1_wbpopest         0.229     0.0672        3.41  0.000644
# #>  4 l1_xm_qudsest       0.257     0.181         1.43  0.154
# #>  5 I(l1_xm_qudsest^2) -0.726     0.211        -3.44  0.000574
# #>  6 newlmtnest          0.0549    0.0666        0.824 0.410
# #>  7 ethfrac             0.442     0.358         1.23  0.217
# #>  8 relfrac            -0.389     0.402        -0.969 0.333
# #>  9 ucdpspell          -0.0738    0.0393       -1.88  0.0601
# #> 10 I(ucdpspell^2)      0.00443   0.00205       2.16  0.0304
# #> 11 I(ucdpspell^3)     -0.0000602 0.0000280    -2.15  0.0316
#
# broom::tidy(modCW$"Wars Only"  <- glm(war_ucdponset ~ l1_wbgdppc2011est + l1_wbpopest  +
#                     l1_xm_qudsest + I(l1_xm_qudsest^2) +
#                     newlmtnest + ethfrac + relfrac +
#                     war_ucdpspell + I(war_ucdpspell^2) + I(war_ucdpspell^3), data=subset(Data),
#                   family = binomial(link="logit")))
# #> # A tibble: 11 × 5
# #>    term                 estimate std.error statistic p.value
# #>    <chr>                   <dbl>     <dbl>     <dbl>   <dbl>
# #>  1 (Intercept)        -6.59      2.08         -3.16  0.00156
# #>  2 l1_wbgdppc2011est  -0.343     0.172        -1.99  0.0463
# #>  3 l1_wbpopest         0.272     0.106         2.56  0.0105
# #>  4 l1_xm_qudsest      -0.0847    0.270        -0.313 0.754
# #>  5 I(l1_xm_qudsest^2) -0.761     0.352        -2.16  0.0307
# #>  6 newlmtnest          0.342     0.112         3.05  0.00226
# #>  7 ethfrac             0.333     0.554         0.601 0.548
# #>  8 relfrac            -0.281     0.593        -0.474 0.635
# #>  9 war_ucdpspell      -0.111     0.0562       -1.98  0.0478
# #> 10 I(war_ucdpspell^2)  0.00466   0.00252       1.85  0.0643
# #> 11 I(war_ucdpspell^3) -0.0000499 0.0000302    -1.65  0.0982
#
# toc()
# #> 2.315 sec elapsed