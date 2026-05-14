library(readr)
library(jsonlite)
library(xgboost)
library(ranger)

# Parameters
n_estimators <- 250L   # fixed in the paper and Python pipeline
upsample_seed <- 2024L # matches Python's RandomOverSampler(random_state=2024)

dir.create("../Reproduction Code/models", showWarnings = FALSE, recursive = TRUE)

# Random oversampling of the minority class with a fixed seed.
random_over_sample <- function(X, y, seed = upsample_seed) {
  set.seed(seed)
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

# Loading tuned hyperparameters from JSON.
load_params <- function(model_number, model_type) {
  path <- sprintf(
    "../Reproduction Code/results/%s_%s_best_within_one.json",
    model_number, model_type
  )
  p <- jsonlite::fromJSON(path)
  p[c("number", "value", "std_err")] <- NULL
  p
}

# 
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
    min_child_weight = if (!is.null(params$min_child_weight)) as.integer(params$min_child_weight) else 1L,
    gamma            = if (!is.null(params$gamma))            params$gamma                 else 0,
    scale_pos_weight = spw,
    nthread          = 1
  )

  xgboost::xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = n_estimators,
    verbose = 0
  )
}

fit_rf <- function(X, y, params) {
  df <- as.data.frame(X)
  names(df) <- make.names(names(df), unique = TRUE)
  df$.y <- factor(as.integer(y), levels = c(0, 1))
  p <- ncol(X)

  max_depth_val    <- if (!is.null(params$max_depth))        as.integer(params$max_depth)        else 0L
  min_bucket_val   <- if (!is.null(params$min_samples_leaf)) as.integer(params$min_samples_leaf) else 1L
  min_node_val     <- if (!is.null(params$min_samples_split)) as.integer(params$min_samples_split) else 1L
  mtry_val         <- resolve_mtry(params$max_features, p)

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

# Fitting on the raw training set (no upsampling)
fit_model_full_data <- function(model_number, model_type) {
  X <- read_csv(sprintf("X_train_%s.csv", model_number),
                na = character(), show_col_types = FALSE)
  y <- read_csv(sprintf("y_train_%s.csv", model_number),
                show_col_types = FALSE)$y_train

  params <- load_params(model_number, model_type)

  if (model_type == "xgboost") {
    fit <- fit_xgb(X, y, params)
  } else {
    fit <- fit_rf(X, y, params)
  }

  out_path <- sprintf(
    "../Reproduction Code/models/%s_%s.rds", model_number, model_type
  )
  saveRDS(fit, file = out_path)
  cat(sprintf("Saved %s\n", out_path))
}

# Upsampling minority class then fit
fit_model_with_upsample <- function(model_number, model_type) {
  X <- read_csv(sprintf("X_train_%s.csv", model_number),
                na = character(), show_col_types = FALSE)
  y <- read_csv(sprintf("y_train_%s.csv", model_number),
                show_col_types = FALSE)$y_train

  params <- load_params(model_number, model_type)

  os <- random_over_sample(X, y, seed = upsample_seed)
  X_up <- os$X
  y_up <- os$y

  if (model_type == "xgboost") {
    fit <- fit_xgb(X_up, y_up, params)
  } else {
    fit <- fit_rf(X_up, y_up, params)
  }

  out_path <- sprintf(
    "../Reproduction Code/models/%s_%s.rds", model_number, model_type
  )
  saveRDS(fit, file = out_path)
  cat(sprintf("Saved %s\n", out_path))
}

# Fitting all models

# model_1 and model_2: full training set, no upsampling
for (mn in c("model_1", "model_2")) {
  for (mt in c("xgboost", "rf")) {
    cat(sprintf("\nFitting %s - %s (full data)\n", mn, mt))
    fit_model_full_data(model_number = mn, model_type = mt)
  }
}

# model_1a, model_2a, model_1b, model_2b: upsampled minority class
for (mn in c("model_1a", "model_2a", "model_1b", "model_2b")) {
  for (mt in c("xgboost", "rf")) {
    cat(sprintf("\nFitting %s - %s (with upsample)\n", mn, mt))
    fit_model_with_upsample(model_number = mn, model_type = mt)
  }
}

cat("\nDone.\n")
