library(dplyr)
library(readr)
library(xgboost)
library(pROC)
library(jsonlite)

N_DRAWS          <- 20
n_splits         <- 5
seed             <- 20240627
cv_test_fraction <- 1 / n_splits
n_estimators     <- 250
upsample         <- TRUE        
model_type       <- "xgboost"

data_dir    <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"

stratified_shuffle_split <- function(y, n_splits, test_fraction, seed) {
  y <- as.character(y)
  splits <- vector("list", n_splits)
  classes <- sort(unique(y))
  
  for (i in seq_len(n_splits)) {
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

fit_xgb <- function(X, y, params, scale_pos_weight) {
  Xm <- as.matrix(X)
  dtrain <- xgboost::xgb.DMatrix(data = Xm, label = as.numeric(y))
  
  xgb_params <- list(
    objective         = "binary:logistic",
    eval_metric       = "auc",
    eta               = params$learning_rate,
    max_depth         = as.integer(params$max_depth),
    subsample         = params$subsample,
    colsample_bytree  = params$colsample_bytree,
    scale_pos_weight  = scale_pos_weight,
    nthread           = 1
  )
  
  xgboost::xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = n_estimators,
    verbose = 0
  )
}

predict_prob <- function(fit, X) {
  Xm <- as.matrix(X)
  dtest <- xgboost::xgb.DMatrix(data = Xm)
  as.numeric(predict(fit, newdata = dtest))
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

score_one_combo <- function(X, y, splits, params, upsample) {
  fold_aucs <- numeric(length(splits))
  
  for (i in seq_along(splits)) {
    train_idx <- splits[[i]]$train
    val_idx   <- splits[[i]]$val
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_val   <- X[val_idx,   , drop = FALSE]
    y_val   <- y[val_idx]
    
    if (upsample) {
      os      <- random_over_sample(X_train, y_train, seed = seed + 1000 + i)
      X_train <- os$X
      y_train <- os$y
    }
    
    spw <- sum(y_train == 0) / max(1, sum(y_train == 1))
    
    fit  <- fit_xgb(X_train, y_train, params, scale_pos_weight = spw)
    prob <- predict_prob(fit, X_val)
    fold_aucs[i] <- suppressMessages(
      as.numeric(pROC::auc(pROC::roc(
        response = as.integer(y_val), predictor = prob,
        levels = c(0, 1), direction = "<"
      )))
    )
  }
  
  list(
    value   = mean(fold_aucs),
    std_err = sd(fold_aucs) / sqrt(length(fold_aucs))
  )
}

# Tuning grid: 240 combinations
grid <- expand.grid(
  learning_rate    = c(0.05, 0.1, 0.2, 0.3, 0.5),
  subsample        = c(0.5, 0.7, 0.85, 1.0),
  max_depth        = c(3, 5, 7, 9),
  colsample_bytree = c(0.6, 0.75, 0.9),
  KEEP.OUT.ATTRS   = FALSE,
  stringsAsFactors = FALSE
)
sort_params <- c("learning_rate", "subsample", "max_depth", "colsample_bytree")

# Tune once per model, on draw 01. Returns the chosen hyperparameters
# and the CV fold indices, both reused across all 20 draws.
tune_one_model <- function(model_number) {
  X <- read_csv(file.path(data_dir, sprintf("X_train_%s_draw_01.csv", model_number)),
                na = character(), show_col_types = FALSE)
  y <- read_csv(file.path(data_dir, sprintf("y_train_%s_draw_01.csv", model_number)),
                show_col_types = FALSE)$y_train
  
  splits <- stratified_shuffle_split(
    y = y, n_splits = n_splits, test_fraction = cv_test_fraction, seed = seed
  )
  
  cat(sprintf("\nTuning %s (%s) on draw 01: %d grid combinations x %d folds\n",
              model_number, model_type, nrow(grid), n_splits))
  
  tuning_results <- vector("list", nrow(grid))
  start_time <- Sys.time()
  for (k in seq_len(nrow(grid))) {
    params <- as.list(grid[k, ])
    res    <- score_one_combo(X, y, splits, params, upsample = upsample)
    tuning_results[[k]] <- c(
      list(number = k - 1L), params,
      list(value = res$value, std_err = res$std_err)
    )
    if (k %% 20 == 0 || k == nrow(grid)) {
      cat(sprintf("  trial %3d/%d  AUC = %.4f\n", k, nrow(grid), res$value))
    }
  }
  cat(sprintf("Tuning time: %s\n", format(Sys.time() - start_time)))
  
  trials_df <- bind_rows(tuning_results)
  write_csv(trials_df, file.path(
    results_dir, sprintf("italy_%s_%s_draw_01_trials.csv", model_number, model_type)
  ))
  
  best_idx <- which.max(trials_df$value)
  best_trial <- as.list(trials_df[best_idx, ])
  write_json(best_trial, file.path(
    results_dir, sprintf("italy_%s_%s_draw_01_best.json", model_number, model_type)
  ), auto_unbox = TRUE, pretty = TRUE)
  
  within_one_se <- trials_df$value > (best_trial$value - best_trial$std_err)
  candidates <- trials_df[within_one_se, ]
  candidates <- candidates[do.call(order, candidates[sort_params]), ]
  chosen <- as.list(candidates[1, ])
  
  write_json(chosen, file.path(
    results_dir, sprintf("italy_%s_%s_chosen_params.json", model_number, model_type)
  ), auto_unbox = TRUE, pretty = TRUE)
  
  cat("Chosen (parsimonious within 1 SE), reused for all 20 draws:\n")
  for (nm in sort_params) {
    cat(sprintf("  %-20s = %s\n", nm, format(chosen[[nm]])))
  }
  cat(sprintf("  %-20s = %.4f (best = %.4f, SE = %.4f)\n",
              "tuning AUC", chosen$value, best_trial$value, best_trial$std_err))
  
  list(params = chosen[sort_params], splits = splits)
}

# Applies the chosen hyperparameters and fold indices to one draw
run_draw <- function(model_number, draw, chosen_params, splits) {
  X <- read_csv(file.path(data_dir, sprintf("X_train_%s_draw_%02d.csv", model_number, draw)),
                na = character(), show_col_types = FALSE)
  y <- read_csv(file.path(data_dir, sprintf("y_train_%s_draw_%02d.csv", model_number, draw)),
                show_col_types = FALSE)$y_train
  
  cv_scores <- list(
    fold = integer(0), test_precision = numeric(0), test_recall = numeric(0),
    test_roc_auc = numeric(0), test_accuracy = numeric(0), test_f1 = numeric(0)
  )
  
  for (i in seq_along(splits)) {
    train_idx <- splits[[i]]$train
    val_idx   <- splits[[i]]$val
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_val   <- X[val_idx,   , drop = FALSE]
    y_val   <- y[val_idx]
    
    if (upsample) {
      os      <- random_over_sample(X_train, y_train, seed = seed + 1000 + i)
      X_train <- os$X
      y_train <- os$y
    }
    
    spw <- sum(y_train == 0) / max(1, sum(y_train == 1))
    
    fit  <- fit_xgb(X_train, y_train, chosen_params, scale_pos_weight = spw)
    prob <- predict_prob(fit, X_val)
    m    <- compute_metrics(y_val, prob)
    
    cv_scores$fold           <- c(cv_scores$fold, i - 1L)
    cv_scores$test_precision <- c(cv_scores$test_precision, m$test_precision)
    cv_scores$test_recall    <- c(cv_scores$test_recall,    m$test_recall)
    cv_scores$test_roc_auc   <- c(cv_scores$test_roc_auc,   m$test_roc_auc)
    cv_scores$test_accuracy  <- c(cv_scores$test_accuracy,  m$test_accuracy)
    cv_scores$test_f1        <- c(cv_scores$test_f1,        m$test_f1)
  }
  
  saveRDS(cv_scores, file = file.path(
    results_dir, sprintf("italy_%s_%s_draw_%02d.rds", model_number, model_type, draw)
  ))
  cat(sprintf("  draw %02d: mean AUC = %.4f, mean accuracy = %.4f\n",
              draw, mean(cv_scores$test_roc_auc), mean(cv_scores$test_accuracy)))
}

# Tune once, then evaluate all 20 draws, for both models
for (model_number in c("model_1", "model_2")) {
  tuned <- tune_one_model(model_number)
  
  cat(sprintf("\nApplying chosen %s hyperparameters across all %d draws:\n",
              model_number, N_DRAWS))
  for (draw in seq_len(N_DRAWS)) {
    run_draw(model_number, draw, tuned$params, tuned$splits)
  }
}

cat("\nDone: 6_italy_xgboost.R\n")
