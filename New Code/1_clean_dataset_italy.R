library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(matrixStats)  # for rowMedians

filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate

convert_datetime <- function(dt) {
  date_part <- str_split_fixed(dt, " ", 2)[, 1]
  as.Date(date_part, format = "%d/%m/%Y")
}

household_convert <- function(size_str) {
  out <- suppressWarnings(as.numeric(size_str))
  out[size_str == "8 or more"] <- 8
  out[size_str %in% c("Prefer not to say", "Don't know")] <- NA_real_
  out
}

df <- read_csv(
  "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/italy.csv",
  na = c("", " ", "__NA__", "NA"),
  guess_max = 100000
)

df$endtime <- convert_datetime(df$endtime)

vac_map <- c(
  "No, neither"    = 0L,
  "Yes, one dose"  = 1L,
  "Yes, two doses" = 2L
)

if ("vac" %in% names(df)) {
  df$vaccination_status <- vac_map[as.character(df$vac)]
  df$vaccination_status[is.na(df$vaccination_status)] <- 3L
  df$vaccination_status <- as.integer(df$vaccination_status)
  df$vac <- NULL
}

# Italian threshold: 20% of 54,270 rows = 10,854
thresh_value <- 10854

missing_value_df <- read_csv("C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/missing_value_counts_italy.csv")

columns_to_drop <- missing_value_df %>%
  filter(`Missing Value Count` > thresh_value) %>%
  pull(`Variable Name`)

columns_to_drop <- setdiff(columns_to_drop, c("vac", "vaccination_status"))

df <- df %>% dplyr::select(-any_of(columns_to_drop))

# PHQ4 window: April 2020 (questions not yet in the survey)
# d1_health window: March 2022 (questions dropped in final wave)
phq_cols <- paste0("PHQ4_", 1:4)
d1_cols  <- c(paste0("d1_health_", 1:13), paste0("d1_health_", 98:99))

fill_na_in_mask <- function(x, mask) {
  x <- as.character(x)
  x[mask & is.na(x)] <- "N/A"
  x
}

phq_mask <- df$endtime < as.Date("2020-05-01")
d1_mask  <- df$endtime >= as.Date("2022-03-01")

for (col in phq_cols) {
  if (col %in% names(df)) df[[col]] <- fill_na_in_mask(df[[col]], phq_mask)
}
for (col in d1_cols) {
  if (col %in% names(df)) df[[col]] <- fill_na_in_mask(df[[col]], d1_mask)
}

df <- df %>% dplyr::filter(complete.cases(.))

r1_map <- c(
  "1 – Disagree" = 1,  
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

# Facemask scale: median across the 4 mask related items
mask_items <- c("i12_health_1", "i12_health_22", "i12_health_23", "i12_health_25")
df$face_mask_behaviour_scale <- rowMedians(
  as.matrix(df[, mask_items]), na.rm = TRUE
)
df$face_mask_behaviour_binary <- ifelse(df$face_mask_behaviour_scale >= 4, "Yes", "No")

protective_behaviour_cols <- grep("^i12_", names(df), value = TRUE)
df$protective_behaviour_scale <- rowMedians(
  as.matrix(df[, protective_behaviour_cols]), na.rm = TRUE
)
df$protective_behaviour_binary <- ifelse(df$protective_behaviour_scale >= 4, "Yes", "No")

protective_behaviour_nomask_cols <- setdiff(protective_behaviour_cols, mask_items)
df$protective_behaviour_nomask_scale <- rowMedians(
  as.matrix(df[, protective_behaviour_nomask_cols]), na.rm = TRUE
)

# Default "Yes" then override with "No", "NA", "Prefer_not_to_say" based
# on the d1_health_99 and d1_health_98 responses.
df$d1_comorbidities <- "Yes"
if ("d1_health_99" %in% names(df)) {
  df$d1_comorbidities[df$d1_health_99 == "Yes"] <- "No"
  df$d1_comorbidities[df$d1_health_99 == "N/A"] <- "NA"
}
if ("d1_health_98" %in% names(df)) {
  df$d1_comorbidities[df$d1_health_98 == "Yes"] <- "Prefer_not_to_say"
}

d1_cols_present <- grep("^d1_", names(df), value = TRUE)
d1_cols_to_drop <- setdiff(d1_cols_present, "d1_comorbidities")
df <- df %>% dplyr::select(-any_of(d1_cols_to_drop))

start_date <- min(df$endtime, na.rm = TRUE)
df$week_number <- as.integer(as.numeric(df$endtime - start_date) %/% 14) + 1L

df$household_size <- household_convert(df$household_size)
df <- df %>% dplyr::filter(complete.cases(.))

cols_to_drop_final <- c("qweek", "weight", protective_behaviour_cols)
df <- df %>% dplyr::select(-any_of(cols_to_drop_final))

# Mapping YouGov region labels to HLS19 macro-region categories
# "South" and "Islands" are combined to "South and Islands" as per the paper.
df <- df %>%
  mutate(
    region_hls19 = case_when(
      region == "North east" ~ "North East",
      region == "North west" ~ "North West",
      region == "Centre" ~ "Centre",
      region == "South" ~ "South and Islands",
      region == "Islands" ~ "South and Islands",
      TRUE ~ NA_character_
    )
  )

write_csv(df, "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/cleaned_data_italy.csv")

cat(sprintf("cleaned_data_italy.csv written: %d rows, %d columns\n",
            nrow(df), ncol(df)))
