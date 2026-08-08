library(dplyr)
library(readr)
library(ranger)
library(pROC)
library(jsonlite)
library(parallel)

N_DRAWS          <- 20
n_splits         <- 5
seed             <- 20240627
cv_test_fraction <- 1 / n_splits
n_estimators     <- 250
upsample         <- TRUE        
model_type       <- "rf"

data_dir    <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"

n_cores <- max(1, parallel::detectCores() - 1)
cat(sprintf("Detected %d logical cores; using %d worker processes.\n",
            parallel::detectCores(), n_cores))

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

resolve_mtry <- function(max_features, p) {
  if (max_features == "sqrt") max(1L, floor(sqrt(p)))
  else if (max_features == "log2") max(1L, floor(log2(p)))
  else p   # "all" -> sklearn's None
}

fit_rf <- function(X, y, params) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)   # FIX A
  df$.y <- factor(as.integer(y), levels = c(0, 1))
  p <- ncol(X)
  
  mtry_val <- resolve_mtry(params$max_features, p)
  
  ranger::ranger(
    formula        = .y ~ .,
    data           = df,
    num.trees      = n_estimators,
    max.depth      = as.integer(params$max_depth),
    min.bucket     = as.integer(params$min_samples_leaf),
    min.node.size  = as.integer(params$min_samples_split),
    mtry           = mtry_val,
    replace        = TRUE,
    probability    = TRUE,
    classification = TRUE,
    num.threads    = 1,   # kept at 1 deliberately
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
    
    fit  <- fit_rf(X_train, y_train, params)
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
  max_depth         = c(4, 8, 16, 32),
  min_samples_leaf  = c(1, 5, 20, 50, 100),
  max_features      = c("sqrt", "log2", "all"),
  min_samples_split = c(2, 10, 50, 150),
  KEEP.OUT.ATTRS    = FALSE,
  stringsAsFactors  = FALSE
)
sort_params <- c("max_depth", "min_samples_leaf", "max_features", "min_samples_split")

# Tune once per model, on draw 01. The 240 x 5-fold grid search is run in
# parallel across workers; each worker scores one grid combination (all 5
# folds) and returns its row of results.
tune_one_model <- function(model_number) {
  X <- read_csv(file.path(data_dir, sprintf("X_train_%s_draw_01.csv", model_number)),
                na = character(), show_col_types = FALSE)
  y <- read_csv(file.path(data_dir, sprintf("y_train_%s_draw_01.csv", model_number)),
                show_col_types = FALSE)$y_train
  
  splits <- stratified_shuffle_split(
    y = y, n_splits = n_splits, test_fraction = cv_test_fraction, seed = seed
  )
  
  cat(sprintf("\nTuning %s (%s) on draw 01: %d grid combinations x %d folds [%d workers]\n",
              model_number, model_type, nrow(grid), n_splits, n_cores))
  
  # Push this model's data/splits out to the already-running cluster (built
  # once, below, and reused for every tuning + draw step in the script).
  clusterExport(cl, varlist = c("X", "y", "splits"), envir = environment())
  
  start_time <- Sys.time()
  tuning_results <- parLapply(cl, seq_len(nrow(grid)), function(k) {
    params <- as.list(unlist(grid[k, , drop = FALSE]))
    res    <- score_one_combo(X, y, splits, params, upsample = upsample)
    data.frame(
      number            = k - 1L,
      max_depth         = as.integer(params$max_depth),
      min_samples_leaf  = as.integer(params$min_samples_leaf),
      max_features      = params$max_features,
      min_samples_split = as.integer(params$min_samples_split),
      value             = res$value,
      std_err           = res$std_err,
      stringsAsFactors  = FALSE
    )
  })
  cat(sprintf("Tuning time: %s\n", format(Sys.time() - start_time)))
  
  trials_df <- bind_rows(tuning_results)
  write_csv(trials_df, file.path(
    results_dir, sprintf("italy_%s_%s_draw_01_trials.csv", model_number, model_type)
  ))
  
  best_idx <- which.max(trials_df$value)
  best_trial <- list(
    number            = trials_df$number[best_idx],
    max_depth         = trials_df$max_depth[best_idx],
    min_samples_leaf  = trials_df$min_samples_leaf[best_idx],
    max_features      = trials_df$max_features[best_idx],
    min_samples_split = trials_df$min_samples_split[best_idx],
    value             = trials_df$value[best_idx],
    std_err           = trials_df$std_err[best_idx]
  )
  write_json(best_trial, file.path(
    results_dir, sprintf("italy_%s_%s_draw_01_best.json", model_number, model_type)
  ), auto_unbox = TRUE, pretty = TRUE)
  
  within_one_se <- trials_df$value > (best_trial$value - best_trial$std_err)
  candidates <- trials_df[within_one_se, ]
  candidates <- candidates[order(
    candidates$max_depth,
    candidates$min_samples_leaf,
    candidates$max_features,
    candidates$min_samples_split
  ), ]
  
  chosen <- list(
    number            = candidates$number[1],
    max_depth         = candidates$max_depth[1],
    min_samples_leaf  = candidates$min_samples_leaf[1],
    max_features      = candidates$max_features[1],
    min_samples_split = candidates$min_samples_split[1],
    value             = candidates$value[1],
    std_err           = candidates$std_err[1]
  )
  
  write_json(chosen, file.path(
    results_dir, sprintf("italy_%s_%s_chosen_params.json", model_number, model_type)
  ), auto_unbox = TRUE, pretty = TRUE)
  
  cat("Chosen (parsimonious within 1 SE), reused for all 20 draws:\n")
  for (nm in sort_params) {
    cat(sprintf("  %-20s = %s\n", nm, format(chosen[[nm]])))
  }
  cat(sprintf("  %-20s = %.4f (best = %.4f, SE = %.4f)\n",
              "tuning AUC", chosen$value, best_trial$value, best_trial$std_err))
  
  final_params <- list(
    max_depth         = chosen$max_depth,
    min_samples_leaf  = chosen$min_samples_leaf,
    max_features      = chosen$max_features,
    min_samples_split = chosen$min_samples_split
  )
  
  list(params = final_params, splits = splits)
}

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
    
    fit  <- fit_rf(X_train, y_train, chosen_params)
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

cl <- makeCluster(n_cores, type = "PSOCK", outfile = "")
clusterEvalQ(cl, { library(readr); library(ranger); library(pROC) })
clusterExport(cl, varlist = c(
  "seed", "upsample", "n_estimators", "grid", "data_dir", "results_dir", "model_type",
  "resolve_mtry", "fit_rf", "predict_prob", "compute_metrics",
  "random_over_sample", "score_one_combo", "run_draw"
))

for (model_number in c("model_1", "model_2")) {
  tuned <- tune_one_model(model_number)
  
  cat(sprintf("\nApplying chosen %s hyperparameters across all %d draws [%d workers]:\n",
              model_number, N_DRAWS, n_cores))
  
  chosen_params <- tuned$params
  splits        <- tuned$splits
  clusterExport(cl, varlist = c("model_number", "chosen_params", "splits"), envir = environment())
  
  invisible(parLapply(cl, seq_len(N_DRAWS), function(draw) {
    run_draw(model_number, draw, chosen_params, splits)
  }))
}

stopCluster(cl)
cat("\nDone: 6_italy_rf.R\n")
