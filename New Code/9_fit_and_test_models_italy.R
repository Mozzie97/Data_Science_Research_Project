library(dplyr)
library(readr)
library(tidyr)
library(jsonlite)
library(xgboost)
library(ranger)
library(pROC)

N_DRAWS      <- 20
n_splits     <- 5         
n_estimators <- 250
upsample_seed <- 2024L  
upsample     <- TRUE    

data_dir    <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"
models_dir  <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/models"
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

metric_names <- c("test_precision", "test_recall", "test_roc_auc",
                  "test_accuracy", "test_f1")

random_over_sample <- function(X, y, seed = upsample_seed) {
  set.seed(seed)
  y <- as.integer(y)
  tab <- table(y); max_n <- max(tab)
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

load_params <- function(model_number, model_type) {
  path <- file.path(
    results_dir, sprintf("italy_%s_%s_chosen_params.json", model_number, model_type)
  )
  p <- jsonlite::fromJSON(path)
  p[c("number", "value", "std_err")] <- NULL
  p
}

resolve_mtry <- function(max_features, p) {
  if (is.null(max_features)) return(max(1L, floor(sqrt(p))))
  switch(
    as.character(max_features),
    "sqrt" = max(1L, floor(sqrt(p))),
    "log2" = max(1L, floor(log2(p))),
    "all"  = p,
    "None" = p,
    p
  )
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
  
  list(test_precision = precision, test_recall = recall, test_roc_auc = roc_auc,
       test_accuracy = accuracy, test_f1 = f1)
}

fit_xgb <- function(X, y, params) {
  spw <- sum(y == 0) / max(1L, sum(y == 1))
  Xm  <- as.matrix(X)
  dtrain <- xgboost::xgb.DMatrix(data = Xm, label = as.numeric(y))
  
  xgb_params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = if (!is.null(params$learning_rate))    params$learning_rate         else 0.3,
    max_depth        = if (!is.null(params$max_depth))        as.integer(params$max_depth) else 6L,
    subsample        = if (!is.null(params$subsample))        params$subsample             else 1,
    colsample_bytree = if (!is.null(params$colsample_bytree)) params$colsample_bytree      else 1,
    scale_pos_weight = spw,
    nthread          = 1
  )
  
  xgboost::xgb.train(
    params  = xgb_params, data = dtrain,
    nrounds = n_estimators, verbose = 0
  )
}

fit_rf <- function(X, y, params) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(as.integer(y), levels = c(0, 1))
  p <- ncol(X)
  
  max_depth_val  <- if (!is.null(params$max_depth))        as.integer(params$max_depth)        else 0L
  min_bucket_val <- if (!is.null(params$min_samples_leaf)) as.integer(params$min_samples_leaf) else 1L
  min_node_val   <- if (!is.null(params$min_samples_split)) as.integer(params$min_samples_split) else 1L
  mtry_val       <- resolve_mtry(params$max_features, p)
  
  ranger::ranger(
    formula        = .y ~ .,
    data           = df,
    num.trees      = n_estimators,
    max.depth      = max_depth_val,
    min.bucket     = min_bucket_val,
    min.node.size  = min_node_val,
    mtry           = mtry_val,
    replace        = TRUE,
    probability    = TRUE,
    classification = TRUE,
    importance     = "impurity",
    num.threads    = 1,
    verbose        = FALSE
  )
}

predict_xgb <- function(fit, X) {
  Xm <- as.matrix(X)
  dtest <- xgboost::xgb.DMatrix(data = Xm)
  as.numeric(predict(fit, newdata = dtest))
}

predict_rf <- function(fit, X) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)
  p <- predict(fit, data = df)$predictions
  if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, ncol(p)])
}

fit_and_test_one_draw <- function(model_number, model_type, draw) {
  X_train <- read_csv(file.path(data_dir, sprintf("X_train_%s_draw_%02d.csv", model_number, draw)),
                      na = character(), show_col_types = FALSE)
  y_train <- read_csv(file.path(data_dir, sprintf("y_train_%s_draw_%02d.csv", model_number, draw)),
                      show_col_types = FALSE)$y_train
  X_test  <- read_csv(file.path(data_dir, sprintf("X_test_%s_draw_%02d.csv", model_number, draw)),
                      na = character(), show_col_types = FALSE)
  y_test  <- read_csv(file.path(data_dir, sprintf("y_test_%s_draw_%02d.csv", model_number, draw)),
                      show_col_types = FALSE)$y_test
  
  params <- load_params(model_number, model_type)
  
  if (upsample) {
    os <- random_over_sample(X_train, y_train, seed = upsample_seed)
    X_train <- os$X
    y_train <- os$y
  }
  
  if (model_type == "xgboost") {
    fit      <- fit_xgb(X_train, y_train, params)
    prob_pos <- predict_xgb(fit, X_test)
  } else {
    fit      <- fit_rf(X_train, y_train, params)
    prob_pos <- predict_rf(fit, X_test)
  }
  
  saveRDS(fit, file = file.path(
    models_dir, sprintf("italy_%s_%s_draw_%02d.rds", model_number, model_type, draw)
  ))
  
  m <- compute_metrics(y_test, prob_pos)
  
  # Within-draw variance for each metric
  cv_path <- file.path(
    results_dir, sprintf("italy_%s_%s_draw_%02d.rds", model_number, model_type, draw)
  )
  cv_scores <- readRDS(cv_path)
  
  se <- sapply(metric_names, function(mn) {
    sd(cv_scores[[mn]]) / sqrt(n_splits)
  })
  names(se) <- paste0(metric_names, "_se")
  
  c(
    list(model_number = model_number, model_type = model_type, draw = draw),
    m,
    as.list(se)
  )
}

model_numbers <- c("model_1", "model_2")
model_types   <- c("xgboost", "rf")

per_draw_rows <- list()
idx <- 1L

for (model_number in model_numbers) {
  for (model_type in model_types) {
    cat(sprintf("\n%s - %s\n", model_number, model_type))
    for (draw in seq_len(N_DRAWS)) {
      res <- fit_and_test_one_draw(model_number, model_type, draw)
      per_draw_rows[[idx]] <- as.data.frame(res, stringsAsFactors = FALSE)
      idx <- idx + 1L
      cat(sprintf(
        "  draw %02d | AUC=%.3f  prec=%.3f  rec=%.3f  acc=%.3f  F1=%.3f\n",
        draw, res$test_roc_auc, res$test_precision, res$test_recall,
        res$test_accuracy, res$test_f1
      ))
    }
  }
}

per_draw_df <- bind_rows(per_draw_rows)
write_csv(per_draw_df, file.path(results_dir, "italy_test_metrics_per_draw.csv"))
cat(sprintf("\nWrote italy_test_metrics_per_draw.csv (%d rows)\n", nrow(per_draw_df)))

pooled_rows <- list()
idx <- 1L

for (model_number in model_numbers) {
  for (model_type in model_types) {
    subset_df <- per_draw_df %>%
      dplyr::filter(model_number == !!model_number, model_type == !!model_type)
    
    for (metric in metric_names) {
      Q_i <- subset_df[[metric]]
      U_i <- subset_df[[paste0(metric, "_se")]]^2
      M   <- length(Q_i)
      
      Q_bar <- mean(Q_i)
      U_bar <- mean(U_i)
      B     <- var(Q_i)
      T_total <- U_bar + (1 + 1 / M) * B
      pooled_se <- sqrt(T_total)
      
      pooled_rows[[idx]] <- data.frame(
        model_number = model_number,
        model_type   = model_type,
        metric       = metric,
        Q_bar        = Q_bar,
        U_bar        = U_bar,
        B            = B,
        pooled_se    = pooled_se,
        ci_lower95   = Q_bar - 1.96 * pooled_se,
        ci_upper95   = Q_bar + 1.96 * pooled_se,
        n_draws      = M,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
}

pooled_df <- bind_rows(pooled_rows)
write_csv(pooled_df, file.path(results_dir, "italy_test_metrics_pooled.csv"))
cat(sprintf("Wrote italy_test_metrics_pooled.csv (%d rows)\n", nrow(pooled_df)))

cat("\nDone: 10_11_fit_and_test_italy.R\n")
