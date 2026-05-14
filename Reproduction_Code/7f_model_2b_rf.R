library(dplyr)
library(readr)
library(ranger)
library(pROC)
library(jsonlite)

# Parameters
model_number     <- "model_2b"
model_type       <- "rf"
n_splits         <- 5
seed             <- 20240627
cv_test_fraction <- 1 / n_splits
upsample         <- TRUE
n_estimators     <- 250

stratified_shuffle_split <- function(y, n_splits, test_fraction, seed) {
  y <- as.character(y)
  splits <- vector("list", n_splits)
  classes <- sort(unique(y))
  for (i in seq_len(n_splits)) {
    set.seed(seed + i)
    val_idx <- integer(0)
    for (cls in classes) {
      in_cls <- which(y == cls)
      n_val  <- round(length(in_cls) * test_fraction)
      val_idx <- c(val_idx, sample(in_cls, size = n_val, replace = FALSE))
    }
    train_idx  <- setdiff(seq_along(y), val_idx)
    splits[[i]] <- list(train = train_idx, val = sort(val_idx))
  }
  splits
}

random_over_sample <- function(X, y, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  y <- as.integer(y)
  tab <- table(y)
  max_n <- max(tab)
  keep_rows <- integer(0)
  for (cls in names(tab)) {
    in_cls <- which(y == as.integer(cls))
    if (length(in_cls) < max_n) {
      extra <- sample(in_cls, size = max_n - length(in_cls), replace = TRUE)
      keep_rows <- c(keep_rows, in_cls, extra)
    } else {
      keep_rows <- c(keep_rows, in_cls)
    }
  }
  list(X = X[keep_rows, , drop = FALSE], y = y[keep_rows])
}

fit_rf <- function(X, y, params) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)   # FIX A
  df$.y <- factor(as.integer(y), levels = c(0, 1))
  p <- ncol(X)

  ranger::ranger(
    formula        = .y ~ .,
    data           = df,
    num.trees      = n_estimators,
    max.depth      = as.integer(params$max_depth),
    min.bucket     = as.integer(params$min_samples_leaf),
    min.node.size  = 2L,
    mtry           = max(1L, floor(sqrt(p))),
    replace        = TRUE,
    probability    = TRUE,
    classification = TRUE,
    num.threads    = 1,
    verbose        = FALSE
  )
}

predict_prob <- function(fit, X) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)   # FIX A
  p  <- predict(fit, data = df)$predictions
  if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, ncol(p)])
}

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
      response  = y_true, predictor = prob_pos,
      levels    = c(0, 1), direction = "<"
    )))
  )

  list(test_precision = precision, test_recall = recall,
       test_roc_auc = roc_auc, test_accuracy = accuracy, test_f1 = f1)
}

score_one_combo <- function(X, y, splits, params, upsample) {
  fold_aucs <- numeric(length(splits))
  for (i in seq_along(splits)) {
    train_idx <- splits[[i]]$train
    val_idx   <- splits[[i]]$val
    X_train <- X[train_idx, , drop = FALSE]; y_train <- y[train_idx]
    X_val   <- X[val_idx,   , drop = FALSE]; y_val   <- y[val_idx]
    if (upsample) {
      os <- random_over_sample(X_train, y_train, seed = seed + 1000 + i)
      X_train <- os$X; y_train <- os$y
    }
    fit  <- fit_rf(X_train, y_train, params)
    prob <- predict_prob(fit, X_val)
    fold_aucs[i] <- suppressMessages(
      as.numeric(pROC::auc(pROC::roc(
        response = as.integer(y_val), predictor = prob,
        levels = c(0, 1), direction = "<"
      )))
    )
  }
  list(value = mean(fold_aucs),
       std_err = sd(fold_aucs) / sqrt(length(fold_aucs)))
}

# Loads training data
X <- read_csv(sprintf("X_train_%s.csv", model_number),
              na = character(), show_col_types = FALSE)
y <- read_csv(sprintf("y_train_%s.csv", model_number),
              show_col_types = FALSE)$y_train

splits <- stratified_shuffle_split(
  y = y, n_splits = n_splits,
  test_fraction = cv_test_fraction, seed = seed
)

# Tuning grid for model_2b's two influential hyperparameters
grid <- expand.grid(
  max_depth        = c(2, 4, 6, 8, 12, 16, 24, 32),
  min_samples_leaf = c(1, 2, 5, 10, 25, 50, 100, 200),
  KEEP.OUT.ATTRS   = FALSE,
  stringsAsFactors = FALSE
)

cat(sprintf("Tuning %s (%s): %d grid combinations x %d folds\n",
            model_number, model_type, nrow(grid), n_splits))


tuning_results <- vector("list", nrow(grid))
start_time <- Sys.time()
for (k in seq_len(nrow(grid))) {
  params <- as.list(unlist(grid[k, , drop = FALSE]))
  res    <- score_one_combo(X, y, splits, params, upsample = upsample)
  tuning_results[[k]] <- data.frame(
    number           = k - 1L,
    max_depth        = as.integer(params$max_depth),
    min_samples_leaf = as.integer(params$min_samples_leaf),
    value            = res$value,
    std_err          = res$std_err,
    stringsAsFactors = FALSE
  )
  if (k %% 10 == 0 || k == nrow(grid)) {
    cat(sprintf("  trial %3d/%d  AUC = %.4f\n", k, nrow(grid), res$value))
  }
}
cat(sprintf("Tuning time: %s\n", format(Sys.time() - start_time)))

trials_df <- bind_rows(tuning_results)
write_csv(trials_df, sprintf("../Reproduction Code/results/%s_%s_trials.csv", model_number, model_type))

best_idx <- which.max(trials_df$value)
best_trial <- list(
  number           = trials_df$number[best_idx],
  max_depth        = trials_df$max_depth[best_idx],
  min_samples_leaf = trials_df$min_samples_leaf[best_idx],
  value            = trials_df$value[best_idx],
  std_err          = trials_df$std_err[best_idx]
)
write_json(best_trial,
           sprintf("../Reproduction Code/results/%s_%s_trial_best.json", model_number, model_type),
           auto_unbox = TRUE, pretty = TRUE)

within_one_se <- trials_df$value > (best_trial$value - best_trial$std_err)
candidates <- trials_df[within_one_se, ]

sort_params <- c("max_depth", "min_samples_leaf")
candidates <- candidates[order(
  candidates$max_depth,
  candidates$min_samples_leaf
), ]

chosen <- list(
  number           = candidates$number[1],
  max_depth        = candidates$max_depth[1],
  min_samples_leaf = candidates$min_samples_leaf[1],
  value            = candidates$value[1],
  std_err          = candidates$std_err[1]
)

write_json(chosen,
           sprintf("../Reproduction Code/results/%s_%s_best_within_one.json", model_number, model_type),
           auto_unbox = TRUE, pretty = TRUE)

cat("\nChosen (parsimonious within 1 SE):\n")
for (nm in sort_params) {
  cat(sprintf("  %-20s = %s\n", nm, format(chosen[[nm]])))
}
cat(sprintf("  %-20s = %.4f (best = %.4f, SE = %.4f)\n",
            "tuning AUC", chosen$value,
            best_trial$value, best_trial$std_err))

# Final 5-fold CV with the chosen hyperparameters
cv_scores <- list(
  fold = integer(0), test_precision = numeric(0), test_recall = numeric(0),
  test_roc_auc = numeric(0), test_accuracy = numeric(0), test_f1 = numeric(0)
)

final_params <- list(
  max_depth        = chosen$max_depth,
  min_samples_leaf = chosen$min_samples_leaf
)

for (i in seq_along(splits)) {
  train_idx <- splits[[i]]$train
  val_idx   <- splits[[i]]$val
  X_train <- X[train_idx, , drop = FALSE]; y_train <- y[train_idx]
  X_val   <- X[val_idx,   , drop = FALSE]; y_val   <- y[val_idx]
  if (upsample) {
    os <- random_over_sample(X_train, y_train, seed = seed + 1000 + i)
    X_train <- os$X; y_train <- os$y
  }
  fit  <- fit_rf(X_train, y_train, final_params)
  prob <- predict_prob(fit, X_val)
  m    <- compute_metrics(y_val, prob)

  cv_scores$fold           <- c(cv_scores$fold, i - 1L)
  cv_scores$test_precision <- c(cv_scores$test_precision, m$test_precision)
  cv_scores$test_recall    <- c(cv_scores$test_recall,    m$test_recall)
  cv_scores$test_roc_auc   <- c(cv_scores$test_roc_auc,   m$test_roc_auc)
  cv_scores$test_accuracy  <- c(cv_scores$test_accuracy,  m$test_accuracy)
  cv_scores$test_f1        <- c(cv_scores$test_f1,        m$test_f1)
}

cat(sprintf("\n%s-%s\n", model_type, model_number))
cat("Mean recall:   ", round(mean(cv_scores$test_recall),   3), "\n")
cat("Mean roc:      ", round(mean(cv_scores$test_roc_auc),  3), "\n")
cat("Mean accuracy: ", round(mean(cv_scores$test_accuracy), 3), "\n")

saveRDS(cv_scores, file = sprintf("../Reproduction Code/results/%s_%s.rds", model_number, model_type))
