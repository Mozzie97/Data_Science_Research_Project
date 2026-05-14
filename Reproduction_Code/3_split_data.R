library(dplyr)
library(readr)

# Load preprocessed data
# `na = character()` keeps the literal string "NA" (used for the
# consent-restricted window) as a value rather than a missing indicator.
cleaned_df <- read_csv(
  "cleaned_data_preprocessing.csv",
  na = character()
)

cleaned_df$endtime <- as.Date(cleaned_df$endtime, format = "%Y-%m-%d")

# Stratified 80/20 train/test split
# Seed matches the Python source for documentation purposes
split_seed <- 20240417
test_size  <- 0.2

set.seed(split_seed)

strata <- cleaned_df$within_mandate_period
test_idx <- integer(0)

for (lvl in sort(unique(strata))) {
  in_stratum <- which(strata == lvl)
  n_test     <- round(length(in_stratum) * test_size)
  # sample() without replacement mirrors sklearn
  test_idx   <- c(test_idx, sample(in_stratum, size = n_test, replace = FALSE))
}

test_idx  <- sort(test_idx)
train_idx <- setdiff(seq_len(nrow(cleaned_df)), test_idx)

df_train <- cleaned_df[train_idx, ]
df_test  <- cleaned_df[test_idx, ]

write_csv(df_train, "df_train.csv")
write_csv(df_test,  "df_test.csv")

# Label encoder to match sklearn.LabelEncoder
label_encode <- function(x) {
  levels_sorted <- sort(unique(as.character(x)))
  as.integer(factor(as.character(x), levels = levels_sorted)) - 1L
}

# Write an (X, y) pair for a given model number
write_model_split <- function(df_train, df_test,
                              feature_cols, response_col,
                              train_mask, test_mask,
                              model_number) {
  X_train <- df_train[train_mask, feature_cols, drop = FALSE]
  X_test  <- df_test[test_mask,  feature_cols, drop = FALSE]

  y_train <- label_encode(df_train[[response_col]][train_mask])
  y_test  <- label_encode(df_test[[response_col]][test_mask])

  write_csv(X_train, sprintf("X_train_%s.csv", model_number))
  write_csv(X_test,  sprintf("X_test_%s.csv",  model_number))
  write_csv(tibble(y_train = y_train),
            sprintf("y_train_%s.csv", model_number))
  write_csv(tibble(y_test = y_test),
            sprintf("y_test_%s.csv",  model_number))
}

# Columns that will not be used as features
base_drop_cols <- c(
  "RecordNo",
  "face_mask_behaviour_scale",
  "protective_behaviour_scale",
  "face_mask_behaviour_binary",
  "protective_behaviour_binary",
  "endtime"
)

all_cols <- names(cleaned_df)

# Model 1: predicting face masks (all time points)
# Response: face_mask_behaviour_binary. All other protective-behaviour items remain as features.
feature_cols_m1 <- setdiff(all_cols, base_drop_cols)

write_model_split(
  df_train, df_test,
  feature_cols   = feature_cols_m1,
  response_col   = "face_mask_behaviour_binary",
  train_mask     = rep(TRUE, nrow(df_train)),
  test_mask      = rep(TRUE, nrow(df_test)),
  model_number   = "model_1"
)

# Model 1a: face masks, early/pre-mandate time only
# `within_mandate_period` is dropped from the
# feature set because it is constant (0) on this subset.
mandate_starter <- as.Date("2022-01-01")

feature_cols_m1a <- setdiff(feature_cols_m1, "within_mandate_period")

train_mask_1a <- df_train$endtime < mandate_starter &
                 df_train$within_mandate_period == 0
test_mask_1a  <- df_test$endtime  < mandate_starter &
                 df_test$within_mandate_period  == 0

write_model_split(
  df_train, df_test,
  feature_cols = feature_cols_m1a,
  response_col = "face_mask_behaviour_binary",
  train_mask   = train_mask_1a,
  test_mask    = test_mask_1a,
  model_number = "model_1a"
)

# Model 1b: face masks, within-mandate period only
# `within_mandate_period` is dropped for the same reason (constant = 1).
feature_cols_m1b <- setdiff(feature_cols_m1, "within_mandate_period")

train_mask_1b <- df_train$within_mandate_period == 1
test_mask_1b  <- df_test$within_mandate_period  == 1

write_model_split(
  df_train, df_test,
  feature_cols = feature_cols_m1b,
  response_col = "face_mask_behaviour_binary",
  train_mask   = train_mask_1b,
  test_mask    = test_mask_1b,
  model_number = "model_1b"
)

# Model 2: predicting general protective behaviour (all time points)
# `protective_behaviour_nomask_scale` is dropped because it overlaps with
# the response.
feature_cols_m2 <- setdiff(
  feature_cols_m1,
  "protective_behaviour_nomask_scale"
)

write_model_split(
  df_train, df_test,
  feature_cols = feature_cols_m2,
  response_col = "protective_behaviour_binary",
  train_mask   = rep(TRUE, nrow(df_train)),
  test_mask    = rep(TRUE, nrow(df_test)),
  model_number = "model_2"
)

# Model 2a: protective behaviour, early/pre-mandate only
feature_cols_m2a <- setdiff(feature_cols_m2, "within_mandate_period")

train_mask_2a <- df_train$endtime < mandate_starter &
                 df_train$within_mandate_period == 0
test_mask_2a  <- df_test$endtime  < mandate_starter &
                 df_test$within_mandate_period  == 0

write_model_split(
  df_train, df_test,
  feature_cols = feature_cols_m2a,
  response_col = "protective_behaviour_binary",
  train_mask   = train_mask_2a,
  test_mask    = test_mask_2a,
  model_number = "model_2a"
)

# Model 2b: protective behaviour, within-mandate period only
feature_cols_m2b <- setdiff(feature_cols_m2, "within_mandate_period")

train_mask_2b <- df_train$within_mandate_period == 1
test_mask_2b  <- df_test$within_mandate_period  == 1

write_model_split(
  df_train, df_test,
  feature_cols = feature_cols_m2b,
  response_col = "protective_behaviour_binary",
  train_mask   = train_mask_2b,
  test_mask    = test_mask_2b,
  model_number = "model_2b"
)
