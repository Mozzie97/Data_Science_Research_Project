library(dplyr)
library(readr)
library(lubridate)

filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate

N_DRAWS  <- 20
data_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"

# One-hot encoding of categorical variables
convert_into_dummy_cols <- c(
  "region", "gender", "i9_health", "employment_status", "i11_health",
  "WCRex1", "WCRex2", "PHQ4_1", "PHQ4_2", "PHQ4_3", "PHQ4_4",
  "d1_comorbidities", "vaccination_status"
)

# Columns to drop
base_drop_cols <- c(
  "hl_covid_cat",   # kept as hl_covid_num
  "vl_cat",         # kept as vl_num
  "age_group",      # kept as numeric age
  "region_hls19",   # kept as region dummies
  "draw_id",        # bookkeeping only
  "RecordNo",       # row identifier, not a feature
  "household_children"  
)

make_dummies <- function(df, col) {
  levels_sorted <- sort(unique(as.character(df[[col]])))
  keep_levels   <- levels_sorted[-1]   
  for (lvl in keep_levels) {
    new_name      <- paste0(col, "_", lvl)
    df[[new_name]] <- as.integer(as.character(df[[col]]) == lvl)
  }
  df[[col]] <- NULL
  df
}

# Processing each draw
for (draw in seq_len(N_DRAWS)) {
  
  in_path <- file.path(
    data_dir,
    sprintf("cleaned_data_italy_hl_draw_%02d.csv", draw)
  )
  
  df <- read_csv(in_path, na = character(), show_col_types = FALSE)
  df$endtime <- as.Date(df$endtime, format = "%Y-%m-%d")
  
  # One-hot encoding categorical variables
  if ("vaccination_status" %in% names(df)) {
    df$vaccination_status <- as.character(df$vaccination_status)
  }
  
  for (col in convert_into_dummy_cols) {
    if (col %in% names(df)) {
      df <- make_dummies(df, col)
    }
  }
  
  df <- df %>% dplyr::select(-any_of(base_drop_cols))
  
  # Output
  out_path <- file.path(
    data_dir,
    sprintf("preprocessed_italy_hl_draw_%02d.csv", draw)
  )
  write_csv(df, out_path)
  cat(sprintf("Draw %02d preprocessed: %d rows, %d columns -> %s\n",
              draw, nrow(df), ncol(df), out_path))
}

cat("\nAll 20 draws preprocessed.\n")
