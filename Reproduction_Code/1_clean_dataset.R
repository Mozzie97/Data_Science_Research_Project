library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(matrixStats)  # for rowMedians

# Convert "DD/MM/YYYY HH:MM" (or just "DD/MM/YYYY") -> Date
convert_datetime <- function(dt) {
  date_part <- str_split_fixed(dt, " ", 2)[, 1]
  as.Date(date_part, format = "%d/%m/%Y")
}

# Map the household_size string responses to a numeric value.
# "8 or more" -> 8;  "Don't know" / "Prefer not to say" -> NA
household_convert <- function(size_str) {
  out <- suppressWarnings(as.numeric(size_str))
  out[size_str == "8 or more"] <- 8
  out[size_str %in% c("Prefer not to say", "Don't know")] <- NA_real_
  out
}

# Load raw data
df <- read_csv(
  "australia.csv",
  na = c("", " ", "__NA__", "NA"),
  guess_max = 100000
)

df$endtime <- convert_datetime(df$endtime)

# Drop columns with too many missing values
thresh_value <- 10781

missing_value_df <- read_csv(
  "missing_value_counts.csv",
)

columns_to_drop <- missing_value_df %>%
  filter(`Missing Value Count` > thresh_value) %>%
  pull(`Variable Name`)

df <- df %>% select(-any_of(columns_to_drop))

# Fill consent-restricted missing values with the string "N/A"
# During this window, consent for medical questions was not collected, so
# PHQ4_* and d1_health_* are missing. We treat "missing because
# not asked" as its own category by replacing NA with "N/A".
sdate <- as.Date("2021-02-10")
edate <- as.Date("2021-10-18")
mask  <- df$endtime >= sdate & df$endtime <= edate

phq_cols <- paste0("PHQ4_", 1:4)
d1_cols  <- c(paste0("d1_health_", 1:13), paste0("d1_health_", 98:99))

# Convert to character first so we can store the literal "N/A" alongside
# real responses, then fill NAs only within the masked window.
fill_na_in_mask <- function(x, mask) {
  x <- as.character(x)
  x[mask & is.na(x)] <- "N/A"
  x
}

for (col in phq_cols) {
  if (col %in% names(df)) df[[col]] <- fill_na_in_mask(df[[col]], mask)
}
for (col in d1_cols) {
  if (col %in% names(df)) df[[col]] <- fill_na_in_mask(df[[col]], mask)
}

# Drop remaining missing values
df <- df %>% filter(complete.cases(.))

# Recode r1_1 and r1_2 (perceived severity / susceptibility)
# Map the 7-point string scale to integers 1-7.
r1_map <- c(
  "1 \u2013 Disagree" = 1,  # en dash, as in Python source
  "1 - Disagree"      = 1,  # plain hyphen fallback
  "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
  "7 - Agree" = 7
)

recode_r1 <- function(x) {
  out <- r1_map[as.character(x)]
  as.integer(out)
}

for (i in 1:2) {
  col <- paste0("r1_", i)
  if (col %in% names(df)) df[[col]] <- recode_r1(df[[col]])
}

# Recode i12_health_* protective behaviour items
# 5-point frequency scale.
frequency_dict <- c(
  "Always"     = 5,
  "Frequently" = 4,
  "Sometimes"  = 3,
  "Rarely"     = 2,
  "Not at all" = 1
)

i12_cols <- grep("^i12_health_", names(df), value = TRUE)
for (col in i12_cols) {
  df[[col]] <- as.integer(frequency_dict[as.character(df[[col]])])
}

# Build face mask and protective-behaviour scales

# Face-mask scale: median across the 4 mask-related items
mask_items <- c("i12_health_1", "i12_health_22", "i12_health_23", "i12_health_25")
df$face_mask_behaviour_scale <- rowMedians(
  as.matrix(df[, mask_items]), na.rm = TRUE
)
df$face_mask_behaviour_binary <- ifelse(df$face_mask_behaviour_scale >= 4, "Yes", "No")

# General protective-behaviour scale: median across ALL i12_* items
protective_behaviour_cols <- grep("^i12_", names(df), value = TRUE)
df$protective_behaviour_scale <- rowMedians(
  as.matrix(df[, protective_behaviour_cols]), na.rm = TRUE
)
df$protective_behaviour_binary <- ifelse(df$protective_behaviour_scale >= 4, "Yes", "No")

# Protective behaviours OTHER than face mask: used as a feature (not target)
# when predicting face mask wearing.
protective_behaviour_nomask_cols <- setdiff(protective_behaviour_cols, mask_items)
df$protective_behaviour_nomask_scale <- rowMedians(
  as.matrix(df[, protective_behaviour_nomask_cols]), na.rm = TRUE
)

# Collapse comorbidities into a single d1_comorbidities variable
# Default: "Yes". Then override with "No", "NA", "Prefer_not_to_say" based on
# the d1_health_99 / d1_health_98 sentinel responses.
df$d1_comorbidities <- "Yes"
if ("d1_health_99" %in% names(df)) {
  df$d1_comorbidities[df$d1_health_99 == "Yes"]  <- "No"
  df$d1_comorbidities[df$d1_health_99 == "N/A"]  <- "NA"
}
if ("d1_health_98" %in% names(df)) {
  df$d1_comorbidities[df$d1_health_98 == "Yes"]  <- "Prefer_not_to_say"
}

d1_cols_present <- grep("^d1_", names(df), value = TRUE)
d1_cols_to_drop <- setdiff(d1_cols_present, "d1_comorbidities")
df <- df %>% select(-any_of(d1_cols_to_drop))

# Build a fortnight-based week_number
# The original survey's qweek variable changes definition over time, so we
# build our own count of fortnights since the first observation.
start_date <- min(df$endtime, na.rm = TRUE)
df$week_number <- as.integer(as.numeric(df$endtime - start_date) %/% 14) + 1L

# Convert household_size to numeric, then drop induced NAs
df$household_size <- household_convert(df$household_size)
df <- df %>% filter(complete.cases(.))

# Drop survey weighting and the intermediate i12_* columns
cols_to_drop_final <- c("qweek", "weight", protective_behaviour_cols)
df <- df %>% select(-any_of(cols_to_drop_final))

# Save as csv file
write_csv(df, "cleaned_data.csv")

