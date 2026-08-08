library(dplyr)
library(readr)

N_DRAWS   <- 20
data_dir  <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
test_size <- 0.2
split_seed <- 20240417   # same seed as the Australian pipeline for comparability

label_encode <- function(x) {
  levels_sorted <- sort(unique(as.character(x)))
  as.integer(factor(as.character(x), levels = levels_sorted)) - 1L
}

# Resolved on the first draw once nrow() is known, then reused unchanged.
fixed_train_idx <- NULL
fixed_test_idx  <- NULL

write_model_split <- function(df, feature_cols, response_col,
                              model_number, draw) {
  
  suffix <- sprintf("%s_draw_%02d", model_number, draw)
  
  X_train <- df[fixed_train_idx, feature_cols, drop = FALSE]
  X_test  <- df[fixed_test_idx,  feature_cols, drop = FALSE]
  
  y_train <- label_encode(df[[response_col]][fixed_train_idx])
  y_test  <- label_encode(df[[response_col]][fixed_test_idx])
  
  # Safety check
  if (nrow(X_train) == 0 || nrow(X_test) == 0) {
    stop(sprintf("Empty split produced for %s -- check upstream filtering.", suffix))
  }
  
  write_csv(X_train, file.path(data_dir, sprintf("X_train_%s.csv", suffix)))
  write_csv(X_test,  file.path(data_dir, sprintf("X_test_%s.csv",  suffix)))
  write_csv(tibble(y_train = y_train),
            file.path(data_dir, sprintf("y_train_%s.csv", suffix)))
  write_csv(tibble(y_test = y_test),
            file.path(data_dir, sprintf("y_test_%s.csv",  suffix)))
  
  cat(sprintf("  %s: %d train / %d test rows written\n",
              suffix, nrow(X_train), nrow(X_test)))
}

base_drop_cols <- c(
  "RecordNo",
  "face_mask_behaviour_scale",
  "protective_behaviour_scale",
  "face_mask_behaviour_binary",
  "protective_behaviour_binary",
  "protective_behaviour_nomask_scale",
  "endtime",
  "hl_covid_cat",
  "vl_cat",
  "draw_id",
  "region_hls19",
  "age_group",
  "within_mandate_period",
  "household_children"    
)

# Processing each draw
for (draw in seq_len(N_DRAWS)) {
  
  in_path <- file.path(
    data_dir,
    sprintf("preprocessed_italy_hl_draw_%02d.csv", draw)
  )
  cleaned_df <- read_csv(in_path, na = character(), show_col_types = FALSE)
  cleaned_df$endtime <- as.Date(cleaned_df$endtime, format = "%Y-%m-%d")
  
  # Compute the fixed partition once, on the first draw, then reuse it as-is.
  if (is.null(fixed_train_idx)) {
    set.seed(split_seed)
    n_rows <- nrow(cleaned_df)
    fixed_test_idx  <- sort(sample(seq_len(n_rows),
                                   size = round(n_rows * test_size),
                                   replace = FALSE))
    fixed_train_idx <- setdiff(seq_len(n_rows), fixed_test_idx)
    cat(sprintf("Fixed partition computed from draw %02d: %d train / %d test rows\n",
                draw, length(fixed_train_idx), length(fixed_test_idx)))
  } else {
    stopifnot(nrow(cleaned_df) == length(fixed_train_idx) + length(fixed_test_idx))
  }
  
  all_cols     <- names(cleaned_df)
  feature_cols <- setdiff(all_cols, base_drop_cols)
  
  cat(sprintf("Draw %02d:\n", draw))
  
  # Model 1: face-mask wearing
  write_model_split(
    cleaned_df,
    feature_cols = feature_cols,
    response_col = "face_mask_behaviour_binary",
    model_number = "model_1",
    draw         = draw
  )
  
  # Model 2: general protective behaviour
  write_model_split(
    cleaned_df,
    feature_cols = feature_cols,
    response_col = "protective_behaviour_binary",
    model_number = "model_2",
    draw         = draw
  )
}

cat("\nAll 20 draw splits written (model_1 and model_2 only, one shared partition).\n")
