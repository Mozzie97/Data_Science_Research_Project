library(dplyr)
library(readr)
library(tibble)

# Parameters
file_count   <- "01"
model_number <- "model_1"
model_types  <- c("logistic_reg", "binary_tree", "xgboost", "rf")

pop_se <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 1) return(NA_real_)
  pop_sd <- sqrt(mean((x - mean(x))^2))
  pop_sd / sqrt(n)
}

# Building one summary row per model type
summarise_one_model <- function(model_type) {
  M <- readRDS(sprintf("../Reproduction Code/results/%s_%s.rds", model_number, model_type))

  tibble(
    model_number  = model_number,
    model_type    = model_type,
    precision     = mean(M$test_precision),
    precision_std = pop_se(M$test_precision),
    recall        = mean(M$test_recall),
    recall_std    = pop_se(M$test_recall),
    roc_auc       = mean(M$test_roc_auc),
    roc_auc_std   = pop_se(M$test_roc_auc),
    accuracy      = mean(M$test_accuracy),
    accuracy_std  = pop_se(M$test_accuracy),
    f1            = mean(M$test_f1),
    f1_std        = pop_se(M$test_f1)
  )
}

final_df <- bind_rows(lapply(model_types, summarise_one_model))

# Save
write_csv(
  final_df,
  sprintf("../Reproduction Code/results/%s_%s_final_results.csv", file_count, model_number)
)
