library(magrittr)
library(tidyverse) # load this first for most/all things
library(peacesciencer) # the package of interest
library(stevemisc) # a dependency, but also used for standardizing variables for better interpretation
library(tictoc)
library(countrycode)
library(ggplot2)

# read cm_features.csv
cm_features <- read_csv("data/cm_features_with_name.csv")
# read translation table
translation_table <- read_csv("data/translation_table_cow_gw_name.csv")
# prolong the translation table to 2022. the last year is 2020. Just copy the 2020 data and add 2021 and 2022

translation_table_2020 <- translation_table %>%
  filter(year == 2020)
translation_table_2021 <- translation_table_2020 %>%
  mutate(year = 2021)
translation_table_2022 <- translation_table_2020 %>%
  mutate(year = 2022)
translation_table <- rbind(translation_table, translation_table_2021, translation_table_2022)
# remove from memory
rm(translation_table_2020, translation_table_2021, translation_table_2022)

# zambia <- cm_features %>%
#   filter(country == "Zambia")

# add year column to cm_features based on date
cm_features <- cm_features %>%
  mutate(year = year(date))
# left_join(translation_table, by = c("gleditsch_ward" = "gwcode", "year" = "year"))

# based on the translation table, add the COW country codes to the cm_features data
# cm_features gw code column is called gleditsch_ward and the translation table gw code column is called gwcode
# account for year
# add only ccode column
cm_features <- cm_features %>%
  left_join(translation_table %>% select(gwcode, year, ccode, gw_statename), by = c("gleditsch_ward" = "gwcode", "year" = "year"))

# THERE ARE NO COUNTRIES WITH NO CCODE AND GED_OS NOT 0 or acled_os NOT 0
filtered_cm_features <- cm_features %>%
  filter(is.na(ccode) & (acled_os != 0 | ged_os != 0))

# FILTER the rows with no ccode and ged_os = 0. Usually just small islands
# print country names of states that do not have a ccode and ged_os = 0
# print((cm_features %>%
#   filter(!(is.na(ccode) & ged_os == 0)))$country)

cm_features_filtered_with_ccode <- cm_features %>%
  filter(!(is.na(ccode) & ged_os == 0))


# save to csv
cm_features_filtered_with_ccode %>% write_csv("data/cm_features_filtered_with_ccode.csv")
read_cm_features_filtered_with_ccode <- read_csv("data/cm_features_filtered_with_ccode.csv")
# check if the shape is the same
print(dim(cm_features_filtered_with_ccode))
print(dim(read_cm_features_filtered_with_ccode))


# calculate amount of observations (group by month_id) for each country and plot as a histogram

# observations_per_country_month <- cm_features %>%
#   group_by(country_id, country, gw_statename) %>%
#   summarise(n = n(), .groups = 'drop') # Count observations
#
# # Plot the histogram of observations
# ggplot(observations_per_country_month, aes(x = n, fill = country)) +
#   geom_histogram(binwidth = 1, color = "black") +
#   labs(title = "Histogram of Observations per Country",
#        x = "Number of Observations",
#        y = "Frequency") +
#   theme_minimal() +
#   scale_fill_viridis_d(begin = 0.5, end = 1, option = "C") + # Optional: Use a discrete color scale for clarity
#   theme(legend.position = "none") # Optional: Remove legend if it's overcrowded

# Optional: Print the top N countries with the most observations
# top_n_countries <- observations_per_country_month %>%
# arrange(asc(n)) %>%
# top_n(10, n)

# print(observations_per_country_month)
# library(tidyverse)
# library(lubridate)
#
# # Ensure the date column is in Date format
# cm_features$date <- as.Date(cm_features$date, format = "%Y-%m-%d")
#
# # Generate a sequence of all months from 1990 to 2022
# all_months <- seq(from = as.Date("1990-01-01"), to = as.Date("2022-12-01"), by = "month")
#
# # Create a unique identifier for each month-year combination in the data
# cm_features <- cm_features %>%
#   mutate(year_month = floor_date(date, "month"),
#          year = year(date),
#          month = month(date))

# Create a dataframe with every month for each country from 1990 to 2022
# expanded_cm_features <- cm_features %>%
#   select(country_id, gw_statename, year_month) %>%
#   distinct() %>%
#   arrange(gw_statename, year_month)

# Now create a complete timeline for visualization
# Note: This step is optimized by only using the distinct months where data is present for clarity and efficiency in plotting
# timeline_data <- expanded_cm_features %>%
#   group_by(country_id) %>%
#   mutate(data_present = 1) %>%
#   ungroup() %>%
#   arrange(country_id)
#
# # select only countries that have Russia or Zambia in their name
# timeline_data <- timeline_data %>%
#   filter(str_detect(gw_statename, "Russia|Zambia|Ukraine|Yemen"))
#
#
# # Plotting: Create the real timeline with data points for each month of data available
# ggplot(timeline_data, aes(x = year_month, y = fct_reorder(gw_statename, gw_statename, .desc = FALSE))) +
#   geom_point(aes(size = data_present), shape = 16, color = "blue", alpha = 0.5, size = 1) + # Adjusted size and alpha
#   labs(title = "Timeline of Data Availability per Country",
#        x = "Year",
#        y = "") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none") + # Hide the legend
#   scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
#   guides(size = FALSE) # Hide the size guide since it's not relevant anymore

# This creates a plot with each dot representing a month of data for the country, sorted alphabetically by country name.

# store unique ccodes in maoz_powers
# remove countries that have regenddate before 1990 or regenddate is NA
# majors <- maoz_powers
majors <- maoz_powers %>%
  filter((regenddate >= 1990 | globenddate >= 1990))
# add country name column using countrycode
majors$country.name <- countrycode(majors$ccode, "cown", 'country.name')


# countrycode(majors$ccode, "cown", 'country.name')


# rivalarties where end is between 1990 and 2022
riv <- tss_rivalries %>%
  filter(end >= 1990 & end <= 2022)