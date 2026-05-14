library(readr)
library(xgboost)
library(ranger)
library(pROC)

compute_metrics <- function(y_true, prob_pos, threshold = 0.5) {
  y_true <- as.integer(y_true)
  y_pred <- as.integer(prob_pos >= threshold)

  tp <- sum(y_true == 1 & y_pred == 1)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)
  tn <- sum(y_true == 0 & y_pred == 0)

  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  f1        <- if ((precision + recall) == 0) 0
               else 2 * precision * recall / (precision + recall)
  accuracy  <- (tp + tn) / length(y_true)

  roc_auc <- suppressMessages(
    as.numeric(pROC::auc(pROC::roc(
      response  = y_true,
      predictor = prob_pos,
      levels    = c(0, 1),
      direction = "<"
    )))
  )

  list(
    test_precision = precision,
    test_recall    = recall,
    test_roc_auc   = roc_auc,
    test_accuracy  = accuracy,
    test_f1        = f1
  )
}

# Predicts P(y = 1) for XGBoost models.
predict_xgb <- function(fit, X) {
  Xm    <- as.matrix(X)
  dtest <- xgboost::xgb.DMatrix(data = Xm)
  as.numeric(predict(fit, newdata = dtest))
}

# Predicts P(y = 1) for ranger models.
predict_rf <- function(fit, X) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)
  p <- predict(fit, data = df)$predictions
  if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, ncol(p)])
}

# Main evaluation loop
model_numbers <- c("model_1", "model_2", "model_1a", "model_2a", "model_1b", "model_2b")
model_types   <- c("xgboost", "rf")

res_list <- vector("list", length(model_numbers) * length(model_types))
idx <- 1L

for (model_number in model_numbers) {
  X <- read_csv(sprintf("X_test_%s.csv", model_number),
                na = character(), show_col_types = FALSE)
  y <- read_csv(sprintf("y_test_%s.csv", model_number),
                show_col_types = FALSE)$y_test

  for (model_type in model_types) {
    model_path <- sprintf(
      "../Reproduction Code/models/%s_%s.rds", model_number, model_type
    )
    M <- readRDS(model_path)

    if (model_type == "xgboost") {
      prob_pos <- predict_xgb(M, X)
    } else {
      prob_pos <- predict_rf(M, X)
    }

    m <- compute_metrics(y, prob_pos)

    res_list[[idx]] <- data.frame(
      model_number   = model_number,
      model_type     = model_type,
      test_precision = m$test_precision,
      test_recall    = m$test_recall,
      test_roc_auc   = m$test_roc_auc,
      test_accuracy  = m$test_accuracy,
      test_f1        = m$test_f1,
      stringsAsFactors = FALSE
    )

    cat(sprintf(
      "%s - %s | AUC=%.3f  prec=%.3f  rec=%.3f  acc=%.3f  F1=%.3f\n",
      model_number, model_type,
      m$test_roc_auc, m$test_precision, m$test_recall,
      m$test_accuracy, m$test_f1
    ))

    idx <- idx + 1L
  }
}

final_df <- do.call(rbind, res_list)

out_path <- "../Reproduction Code/results/final_model_test_metrics.csv"
write_csv(final_df, out_path)
cat(sprintf("\nTest metrics written to %s\n", out_path))
