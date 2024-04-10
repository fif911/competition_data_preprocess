library(magrittr)
library(tidyverse) # load this first for most/all things
library(peacesciencer) # the package of interest
library(stevemisc) # a dependency, but also used for standardizing variables for better interpretation
library(tictoc)
library(countrycode)
library(ggplot2)


create_stateyears(subset_years = c(1990:2016), system = 'cow') %>%
  add_cow_majors() -> Data


# count amount of MAJOR (cowmaj=1) states in 2022
Data %>%
  filter(year == 2016) %>%
  count(cowmaj)

# get all major states in 2016
all_major_states <- Data %>%
  filter(cowmaj == 1) %>%
  select(ccode, year, cowmaj)

# duplicate the same major states for 2017-2022. Take 2016 as a template
for (i in 2017:2022) {
  all_major_states <- rbind(all_major_states, all_major_states %>%
    filter(year == 2016) %>%
    mutate(year = i))
}

# #  plot amount of major states per year. Must have cowmaj=1
# all_major_states %>%
#   filter(cowmaj == 1) %>%
#   ggplot(aes(x = year)) +
#   geom_bar(stat = "count") +
#   labs(title = "Amount of major states per year",
#        x = "Year",
#        y = "Amount of major states") +
#   theme_minimal()

# save to csv
# all_major_states %>% write_csv("data/all_major_states_90_16.csv")

# read cm_features_filtered_with_ccode
cm_features_filtered_with_ccode <- read_csv("data/cm_features_filtered_with_ccode.csv")

# merge all_major_states with cm_features_filtered_with_ccode based on ccode and year
# if country is a major state, cowmaj=1, else cowmaj=0
# TODO: FIX HERE
merged_data <- cm_features_filtered_with_ccode %>%
  left_join(all_major_states, by = c("ccode", "year")) %>%
  mutate(cowmaj = ifelse(is.na(cowmaj), 0, 1))

# print majors in 1990
# merged_data %>%
#   filter(year == 1990) %>% # now sort
#   # select(ccode, year, cowmaj) %>%
#   arrange(desc(cowmaj)) -> majors_1990


# Now, merged_data contains your original cm_features_filtered_with_ccode with cowmaj adjusted
# where cowmaj is 1 for major states and 0 for others.

# Plot amount of major states per year from the merged data
# merged_data %>%
#   filter(cowmaj == 1) %>%
#   ggplot(aes(x = year)) +
#   geom_bar(stat = "count") +
#   labs(title = "Amount of Major States per Year",
#        x = "Year",
#        y = "Amount of Major States") +
#   theme_minimal()
# scale_x_continuous(breaks = seq(min(merged_data$year), max(merged_data$year), by = 1)) # Adjust x-axis breaks to show every year
merged_data %>%
  filter(cowmaj == 1) %>%
  ggplot(aes(x = as.factor(month_id))) + # Convert month_id to factor for discrete x-axis values
  geom_bar(stat = "count", width = 0.5) + # Adjust width as needed for clarity
  labs(title = "Amount of Major States per Month",
       x = "Month ID",
       y = "Amount of Major States") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Rotate x-axis labels for better readability
        axis.title.x = element_text(margin = margin(t = 20)), # Adjust margin for x-axis title if needed due to label rotation
        panel.grid.major.x = element_blank(), # Optional: Remove vertical grid lines for cleaner look
        panel.grid.minor.x = element_blank()) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 50)]) # Adjust frequency of x-axis labels as needed for readability

# get months ids for which there are 8 major states and then print all major states for that month
# states <- merged_data %>%
#   filter(cowmaj == 1) %>%
#   group_by(month_id) %>%
#   filter(n() == 8) %>%
#   select(ccode, gw_statename, month_id) %>%
#   distinct()

# Step 1: Identify months with exactly 8 major states
months_with_8_majors <- merged_data %>%
  filter(cowmaj == 1) %>%
  group_by(month_id) %>%
  summarise(count = n()) %>%
  filter(count == 8) %>%
  pull(month_id) # Extract month_ids as a vector

# Step 2: Filter the original dataset for those months and print the major states
major_states_for_selected_months <- merged_data %>%
  filter(month_id %in% months_with_8_majors & cowmaj == 1) %>%
  arrange(month_id, ccode) # Arrange by month_id and ccode for readability

# Print the result
print(major_states_for_selected_months)