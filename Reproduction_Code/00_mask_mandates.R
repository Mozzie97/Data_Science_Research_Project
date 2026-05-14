library(dplyr)
library(readr)
library(zoo)        # for rollmean
library(lubridate)  # for date parsing

# Load data
df <- read_csv(
  "OxCGRT_AUS_latest.csv",
)

# Keep only the columns we need
col_subsets <- c("RegionName", "RegionCode", "Date", "H6M_Facial Coverings")
df <- df %>% select(all_of(col_subsets))

# Convert the integer-format date (YYYYMMDD) into a proper Date object
df <- df %>%
  mutate(Date = ymd(as.character(Date)))

# Compute 14-day rolling average per state
rolling_days <- 14

df_rolling <- df %>%
  arrange(RegionName, Date) %>%
  group_by(RegionName) %>%
  mutate(
    `H6M_Facial Coverings` = rollmean(
      `H6M_Facial Coverings`,
      k = rolling_days,
      fill = NA,         # First 13 days has no 14 window to compute so NA
      align = "right"    # puts the average at the 14th day
    )
  ) %>%
  ungroup()

# Find the first date the rolling average reaches the mandate threshold
mandate_limit <- 3

df_mandates <- df_rolling %>%
  filter(`H6M_Facial Coverings` >= mandate_limit) %>%
  group_by(RegionName) %>%
  slice_head(n = 1) %>%   # Keeps only the first row which is the mandate start date
  ungroup() %>%
  select(RegionName, Date, `H6M_Facial Coverings`)

# Save as csv file
write_csv(df_mandates, "mandate_start_dates.csv")

