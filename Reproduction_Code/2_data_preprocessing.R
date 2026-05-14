library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# Load cleaned data 
cleaned_df <- read_csv(
  "cleaned_data.csv",
  na = character()       # Treats NA as a value
)

cleaned_df$endtime <- as.Date(cleaned_df$endtime, format = "%Y-%m-%d")

# Build the mandate-start lookup
mandate_df <- read_csv(
  "mandate_start_dates.csv"
) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

states_date <- setNames(mandate_df$Date, mandate_df$RegionName)

# Add within_mandate_period flag 
# 1 if the respondent's endtime is on or after their state's mandate-start
# date, otherwise 0. States with no mandate entry default to 0.
cleaned_df <- cleaned_df %>%
  mutate(
    within_mandate_period = if_else(
      !is.na(states_date[state]) & endtime >= states_date[state],
      1L, 0L
    )
  )

# One-hot encode categorical variables
# Drops the first level of each variable to match pandas' drop_first=True
# behaviour, which avoids the dummy-variable trap.
convert_into_dummy_cols <- c(
  "state", "gender", "i9_health", "employment_status", "i11_health",
  "WCRex1", "WCRex2", "PHQ4_1", "PHQ4_2", "PHQ4_3", "PHQ4_4",
  "d1_comorbidities"
)

make_dummies <- function(df, col) {
  # Get sorted unique levels
  levels_sorted <- sort(unique(as.character(df[[col]])))
  # Drop the first level
  keep_levels <- levels_sorted[-1]

  for (lvl in keep_levels) {
    new_name <- paste0(col, "_", lvl)
    df[[new_name]] <- as.integer(as.character(df[[col]]) == lvl)
  }
  df[[col]] <- NULL
  df
}

for (col in convert_into_dummy_cols) {
  if (col %in% names(cleaned_df)) {
    cleaned_df <- make_dummies(cleaned_df, col)
  }
}

# Save as csv file
write_csv(cleaned_df, "cleaned_data_preprocessing.csv")

