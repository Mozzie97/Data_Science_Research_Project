library(dplyr)
library(readr)
library(glmnet)
library(pROC)

# Parameters
n_splits         <- 5
seed             <- 20240627
cv_test_fraction <- 1 / n_splits  
model_fitting    <- "logistic_reg"

dir.create("../Reproduction Code/results", showWarnings = FALSE, recursive = TRUE)

# stratified 80/20 split of the rows of `y`. 
# Seeded for reproducibility.
stratified_shuffle_split <- function(y, n_splits, test_fraction, seed) {
  y <- as.character(y)
  splits <- vector("list", n_splits)
  classes <- sort(unique(y))

  for (i in seq_len(n_splits)) {
    # Distinct seed per fold so the folds differ from each other.
    set.seed(seed + i)
    val_idx <- integer(0)
    for (cls in classes) {
      in_cls  <- which(y == cls)
      n_val   <- round(length(in_cls) * test_fraction)
      val_idx <- c(val_idx, sample(in_cls, size = n_val, replace = FALSE))
    }
    train_idx  <- setdiff(seq_along(y), val_idx)
    splits[[i]] <- list(train = train_idx, val = sort(val_idx))
  }
  splits
}

# Random oversampling
# We seed it per call for reproducibility
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

# Fit L2 logistic regression
fit_l2_logreg <- function(X, y) {
  Xm <- as.matrix(X)
  n  <- nrow(Xm)
  lambda_target <- 1 / n
  lambda_path <- c(lambda_target * 10, lambda_target)
  glmnet::glmnet(
    x       = Xm,
    y       = as.factor(y),
    family  = "binomial",
    alpha   = 0,            
    lambda  = lambda_path,
    standardize = FALSE     
  )
}

# Predicting class probabilities (positive class).
predict_prob <- function(fit, X) {
  Xm <- as.matrix(X)
  # s = second lambda = target lambda = 1/n
  p  <- predict(fit, newx = Xm, type = "response", s = fit$lambda[2])
  as.numeric(p)
}

# Computes all five metrics
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

# Cross-validating a single model
cross_validate_model <- function(model_number, upsample = FALSE) {
  X <- read_csv(sprintf("X_train_%s.csv", model_number),
                na = character(), show_col_types = FALSE)
  y <- read_csv(sprintf("y_train_%s.csv", model_number),
                show_col_types = FALSE)$y_train

  splits <- stratified_shuffle_split(
    y            = y,
    n_splits     = n_splits,
    test_fraction = cv_test_fraction,
    seed         = seed
  )

  cv_scores <- list(
    fold           = integer(0),
    test_precision = numeric(0),
    test_recall    = numeric(0),
    test_roc_auc   = numeric(0),
    test_accuracy  = numeric(0),
    test_f1        = numeric(0)
  )

  for (i in seq_along(splits)) {
    train_idx <- splits[[i]]$train
    val_idx   <- splits[[i]]$val

    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_val   <- X[val_idx,   , drop = FALSE]
    y_val   <- y[val_idx]

    if (upsample) {
      # Seed per fold so upsampling is reproducible but differs across folds.
      os      <- random_over_sample(X_train, y_train, seed = seed + 1000 + i)
      X_train <- os$X
      y_train <- os$y
    }

    fit   <- fit_l2_logreg(X_train, y_train)
    prob  <- predict_prob(fit, X_val)
    m     <- compute_metrics(y_val, prob)

    cv_scores$fold           <- c(cv_scores$fold, i - 1L)
    cv_scores$test_precision <- c(cv_scores$test_precision, m$test_precision)
    cv_scores$test_recall    <- c(cv_scores$test_recall,    m$test_recall)
    cv_scores$test_roc_auc   <- c(cv_scores$test_roc_auc,   m$test_roc_auc)
    cv_scores$test_accuracy  <- c(cv_scores$test_accuracy,  m$test_accuracy)
    cv_scores$test_f1        <- c(cv_scores$test_f1,        m$test_f1)
  }

  # Reports the same three summaries as the Python source.
  cat(model_number, "\n")
  cat("Mean recall:   ", round(mean(cv_scores$test_recall),   3), "\n")
  cat("Mean roc:      ", round(mean(cv_scores$test_roc_auc),  3), "\n")
  cat("Mean accuracy: ", round(mean(cv_scores$test_accuracy), 3), "\n\n")

  saveRDS(
    cv_scores,
    file = sprintf("../Reproduction Code/results/%s_%s.rds", model_number, model_fitting)
  )

  invisible(cv_scores)
}

# Models 1 and 2 are balanced enough so upsampling is unnecessary.
# The four subset models (1a, 1b, 2a, 2b) are upsampled.
cross_validate_model("model_1")
cross_validate_model("model_1a", upsample = TRUE)
cross_validate_model("model_1b", upsample = TRUE)
cross_validate_model("model_2")
cross_validate_model("model_2a", upsample = TRUE)
cross_validate_model("model_2b", upsample = TRUE)
