# Merge countries with duplicate names in the dataset to make one smooth series
# For cases where country has slowly moving variables backfill the values on the merge point

cm_features <- read_csv("data/cm_features_v0.4.csv")

observations_per_country_month <- cm_features %>%
  group_by(country_id, gw_statename) %>%
  summarise(n = n(), .groups = 'drop')

# find if there is a countries with the same name but different country_id. add these country_ids to a list
duplicate_country_names <- observations_per_country_month %>%
  group_by(gw_statename) %>%
  filter(n() > 1)

# sum by gw_statename
summed_duplicates <- duplicate_country_names %>%
  group_by(gw_statename) %>%
  summarise(n = sum(n))

# duplicate_names <- unique(duplicate_country_names$gw_statename)
#
# list_merged <- list()
# list_resolved_conflicts <- list()
#
# for(name in duplicate_names) {
#   country_data <- cm_features %>% filter(gw_statename == name)
#
#   # Identify month_id overlap among different country_ids
#   overlapping_months <- country_data %>%
#     group_by(month_id) %>%
#     summarise(n_country_ids = n_distinct(country_id), .groups = 'drop') %>%
#     filter(n_country_ids > 1) %>%
#     pull(month_id)
#
#   if(length(overlapping_months) > 0) {
#     # Process overlaps for numeric columns by taking the max value
#     resolved_numeric_data <- country_data %>%
#       filter(month_id %in% overlapping_months) %>%
#       group_by(month_id) %>%
#       summarise(across(where(is.numeric), max, na.rm = TRUE), .groups = 'drop')
#
#     # Handle non-numeric (text and date) fields by taking the first value
#     non_numeric_columns <- select(country_data, where(~!is.numeric(.))) %>%
#       names() %>%
#       setdiff(c("month_id")) # Exclude month_id as it's used for grouping
#
#     resolved_non_numeric_data <- country_data %>%
#       filter(month_id %in% overlapping_months) %>%
#       group_by(month_id) %>%
#       summarise(across(all_of(non_numeric_columns), ~first(.)), .groups = 'drop')
#
#     # Combine resolved numeric and non-numeric data
#     resolved_data <- left_join(resolved_numeric_data, resolved_non_numeric_data, by = "month_id")
#
#     # Combine resolved data with non-overlapping data, ensuring to remove original overlaps from country_data
#     non_overlapping_data <- country_data %>%
#       filter(!(month_id %in% overlapping_months))
#
#     final_data <- bind_rows(non_overlapping_data, resolved_data) %>%
#       arrange(month_id) # Optional: Arrange by month_id for readability
#
#     list_resolved_conflicts[[name]] <- final_data
#   } else {
#     # If no overlap, keep the original country data
#     list_merged[[name]] <- country_data
#   }
# }
#
# # At this point, you have resolved conflicts and preserved non-numeric fields.
# # Example: Printing resolved conflict details for review
# for(name in names(list_resolved_conflicts)) {
#   cat("Resolved conflicts for:", name, "\n")
#   print(list_resolved_conflicts[[name]])
#   cat("\n")
# }

# Function to backfill specific columns based on conditions
backfill_columns <- function(df, relevant_columns, merge_point) {
  for (col_name in relevant_columns) {
    if (col_name %in% names(df)) {
      # Find the first non-zero value after the merge point for the column
      first_non_zero_after_merge <- which(df[[col_name]] != 0 & df$month_id > merge_point)[1]

      # If there is a valid point to backfill from
      if (!is.na(first_non_zero_after_merge) && first_non_zero_after_merge > 1) {
        # Backfill values from this point backwards to the start of the series
        df[[col_name]][1:(first_non_zero_after_merge - 1)] <- df[[col_name]][first_non_zero_after_merge]
      }
    }
  }
  return(df)
}

duplicate_names <- unique(duplicate_country_names$gw_statename)

list_merged <- list()
list_resolved_conflicts <- list()

for (name in duplicate_names) {
  country_data <- cm_features %>% filter(gw_statename == name)

  # Identify month_id overlap among different country_ids
  overlapping_months <- country_data %>%
    group_by(month_id) %>%
    summarise(n_country_ids = n_distinct(country_id), .groups = 'drop') %>%
    filter(n_country_ids > 1) %>%
    pull(month_id)

  # Assume merge point is the earliest month_id in overlapping_months
  merge_point <- min(overlapping_months, na.rm = TRUE)
  relevant_columns <- names(country_data)[grepl("_48|vdem", names(country_data))]

  if (length(overlapping_months) > 0) {
    # Process overlaps for numeric columns by taking the max value
    resolved_numeric_data <- country_data %>%
      filter(month_id %in% overlapping_months) %>%
      group_by(month_id) %>%
      summarise(across(where(is.numeric), max, na.rm = TRUE), .groups = 'drop')

    # Handle non-numeric (text and date) fields by taking the first value
    non_numeric_columns <- select(country_data, where(~!is.numeric(.))) %>%
      names() %>%
      setdiff(c("month_id")) # Exclude month_id as it's used for grouping

    resolved_non_numeric_data <- country_data %>%
      filter(month_id %in% overlapping_months) %>%
      group_by(month_id) %>%
      summarise(across(all_of(non_numeric_columns), ~first(.)), .groups = 'drop')

    # Combine resolved numeric and non-numeric data
    resolved_data <- left_join(resolved_numeric_data, resolved_non_numeric_data, by = "month_id")

    # Combine resolved data with non-overlapping data, ensuring to remove original overlaps from country_data
    non_overlapping_data <- country_data %>%
      filter(!(month_id %in% overlapping_months))

    final_data <- bind_rows(non_overlapping_data, resolved_data) %>%
      arrange(month_id) # Optional: Arrange by month_id for readability

    # Now, apply backfill logic
    final_data <- backfill_columns(final_data, relevant_columns, merge_point)

    list_resolved_conflicts[[name]] <- final_data
  }else {
    # If no overlap, keep the original country data, but first check for a change in country_id as a merge point
    # Identify the first occurrence where country_id changes
    country_id_changes <- which(diff(country_data$country_id) != 0)
    if (length(country_id_changes) > 0) {
      # Assuming the change is significant, calculate the merge point month_id
      merge_point <- country_data$month_id[country_id_changes[1] + 1]

      # Now, apply backfill logic
      # Adjust for when merge_point is NA or not applicable
      if (!is.na(merge_point) && length(merge_point) > 0) {
        country_data <- backfill_columns(country_data, relevant_columns, merge_point)
      }
    }

    list_merged[[name]] <- country_data
  }
}

# At this point, you have resolved conflicts, preserved non-numeric fields, and applied backfill where appropriate.
# Example: Printing resolved conflict details for review
for (name in names(list_resolved_conflicts)) {
  cat("Resolved conflicts for:", name, "\n")
  print(list_resolved_conflicts[[name]])
  cat("\n")
}