library(dplyr)
library(readr)
library(tibble)
library(jsonlite)
library(xgboost)
library(ranger)
library(parallel)

parallel::detectCores()

# Parameters
num_perm     <- 10          # set to 10000 to match the paper exactly
n_cores      <- 6           # adjust to match the local machine
n_estimators <- 250         # fixed in the paper

start_time <- Sys.time()

# Random oversampling of the minority class on the full training set.
random_over_sample <- function(X, y) {
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

# Loading tuned hyperparameters from the JSON files.
load_tuned_params <- function(model_number, model_type) {
  json_path <- sprintf("../Reproduction Code/results/%s_%s_best_within_one.json",
                       model_number, model_type)
  params <- jsonlite::fromJSON(json_path)
  params[c("number", "value", "std_err")] <- NULL
  params
}

coerce_int <- function(x) {
  if (is.null(x)) NULL else as.integer(x)
}

resolve_mtry <- function(max_features, p) {
  if (is.null(max_features)) return(floor(sqrt(p)))
  switch(
    as.character(max_features),
    "sqrt" = max(1L, floor(sqrt(p))),
    "log2" = max(1L, floor(log2(p))),
    "all"  = p,
    "None" = p,
    p     # fall back to using all features for any unrecognised string
  )
}

xgboost_one_perm <- function(X, y, params) {
  upsampled <- random_over_sample(X, y)
  X_up <- as.matrix(upsampled$X)
  y_up <- as.numeric(upsampled$y)

  dtrain <- xgboost::xgb.DMatrix(data = X_up, label = y_up)

  xgb_params <- list(
    objective        = "binary:logistic",
    eval_metric      = "logloss",
    eta              = if (!is.null(params$learning_rate))    params$learning_rate         else 0.3,
    max_depth        = if (!is.null(params$max_depth))        as.integer(params$max_depth) else 6L,
    subsample        = if (!is.null(params$subsample))        params$subsample             else 1,
    colsample_bytree = if (!is.null(params$colsample_bytree)) params$colsample_bytree      else 1,
    min_child_weight = if (!is.null(params$min_child_weight)) as.integer(params$min_child_weight) else 1L,
    gamma            = if (!is.null(params$gamma))            params$gamma                 else 0,
    nthread          = 1   # outer loop is parallel, inner stays serial
  )

  fit <- xgboost::xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = n_estimators,
    verbose = 0
  )

  imp <- xgboost::xgb.importance(model = fit)
  out <- setNames(numeric(ncol(X)), colnames(X))
  matched <- intersect(imp$Feature, names(out))
  out[matched] <- imp$Gain[match(matched, imp$Feature)]
  # Normalise to sum to 1 to match sklearn's feature_importances_ convention
  s <- sum(out)
  if (s > 0) out <- out / s
  as.list(out)
}

rf_one_perm <- function(X, y, params) {
  upsampled <- random_over_sample(X, y)
  X_up  <- upsampled$X
  y_up  <- factor(as.integer(upsampled$y), levels = c(0, 1))

  p <- ncol(X_up)
  mtry_val <- resolve_mtry(params$max_features, p)

  max_depth_val <- coerce_int(params$max_depth)
  if (is.null(max_depth_val)) max_depth_val <- 0L  # ranger: 0 means unlimited

  min_node_size <- coerce_int(params$min_samples_split)
  if (is.null(min_node_size)) min_node_size <- 1L
  min_bucket <- coerce_int(params$min_samples_leaf)
  if (is.null(min_bucket)) min_bucket <- 1L

  df <- as.data.frame(X_up)
  orig_names   <- names(df)                              # original CSV names
  safe_names   <- make.names(orig_names, unique = TRUE)  # sanitised names
  names(df)    <- safe_names
  df$.y        <- y_up

  fit <- ranger::ranger(
    formula        = .y ~ .,
    data           = df,
    num.trees      = n_estimators,
    mtry           = mtry_val,
    max.depth      = max_depth_val,
    min.node.size  = min_node_size,
    min.bucket     = min_bucket,
    importance     = "impurity",
    classification = TRUE,
    num.threads    = 1,   # outer loop is parallel; keeps ranger serial
    verbose        = FALSE
  )

  imp_safe <- fit$variable.importance
  imp_safe <- imp_safe[setdiff(names(imp_safe), ".y")]

  # Building a lookup: safe_name -> orig_name
  name_map <- setNames(orig_names, safe_names)

  out <- setNames(numeric(ncol(X)), colnames(X))
  for (sn in names(imp_safe)) {
    on <- name_map[sn]
    if (!is.na(on) && on %in% names(out)) {
      out[on] <- imp_safe[sn]
    }
  }
  s <- sum(out)
  if (s > 0) out <- out / s
  as.list(out)
}

permute_one_model <- function(model_number, model_type) {
  cat(sprintf("\n%s - %s\n", model_type, model_number))

  X <- read_csv(sprintf("X_train_%s.csv", model_number),
                na = character(), show_col_types = FALSE)
  y <- read_csv(sprintf("y_train_%s.csv", model_number),
                show_col_types = FALSE)$y_train

  params <- load_tuned_params(model_number, model_type)

  one_perm_fn <- if (model_type == "xgboost") xgboost_one_perm else rf_one_perm

  use_parallel <- (.Platform$OS.type == "unix") && (n_cores > 1)

  if (use_parallel) {
    rows <- parallel::mclapply(
      seq_len(num_perm),
      function(i) one_perm_fn(X, y, params),
      mc.cores       = n_cores,
      mc.preschedule = TRUE
    )
  } else {
    rows <- lapply(seq_len(num_perm), function(i) one_perm_fn(X, y, params))
  }

  df <- bind_rows(rows)
  out_path <- sprintf("../Reproduction Code/results/%s_%s_feature_importance.csv",
                      model_number, model_type)
  write_csv(df, out_path)
  cat(sprintf("  wrote %s (%d iterations)\n", out_path, nrow(df)))
}

# Running all 12 (model_number, model_type) combinations.
model_numbers <- c("model_1", "model_2",
                   "model_1a", "model_2a",
                   "model_1b", "model_2b")
model_types   <- c("xgboost", "rf")

for (mn in model_numbers) {
  for (mt in model_types) {
    permute_one_model(mn, mt)
  }
}

cat(sprintf("\nTime taken: %s\n", format(Sys.time() - start_time)))
