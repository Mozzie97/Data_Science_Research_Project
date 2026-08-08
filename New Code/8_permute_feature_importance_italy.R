library(dplyr)
library(readr)
library(tibble)
library(jsonlite)
library(xgboost)
library(ranger)
library(parallel)

num_perm     <- 100     
n_estimators <- 250

N_DRAWS <- 20

data_dir    <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"

n_cores <- max(1, parallel::detectCores() - 1)
cat(sprintf("Detected %d logical cores; using %d worker processes.\n",
            parallel::detectCores(), n_cores))

start_time <- Sys.time()

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

load_tuned_params <- function(model_number, model_type) {
  json_path <- file.path(
    results_dir, sprintf("italy_%s_%s_chosen_params.json", model_number, model_type)
  )
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
    p
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
    nthread          = 1
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
  if (is.null(max_depth_val)) max_depth_val <- 0L
  
  min_node_size <- coerce_int(params$min_samples_split)
  if (is.null(min_node_size)) min_node_size <- 1L
  min_bucket <- coerce_int(params$min_samples_leaf)
  if (is.null(min_bucket)) min_bucket <- 1L
  
  df <- as.data.frame(X_up)
  orig_names   <- names(df)
  safe_names   <- make.names(orig_names, unique = TRUE)
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
    num.threads    = 1,
    verbose        = FALSE
  )
  
  imp_safe <- fit$variable.importance
  imp_safe <- imp_safe[setdiff(names(imp_safe), ".y")]
  
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

permute_one_draw <- function(model_number, model_type, draw) {
  cat(sprintf("\n%s - %s - draw %02d [%d workers]\n", model_type, model_number, draw, n_cores))
  
  X <- read_csv(file.path(data_dir, sprintf("X_train_%s_draw_%02d.csv", model_number, draw)),
                na = character(), show_col_types = FALSE)
  y <- read_csv(file.path(data_dir, sprintf("y_train_%s_draw_%02d.csv", model_number, draw)),
                show_col_types = FALSE)$y_train
  
  params <- load_tuned_params(model_number, model_type)
  
  clusterExport(cl, varlist = c("X", "y", "params", "model_type"), envir = environment())
  
  rows <- parLapply(cl, seq_len(num_perm), function(i) {
    one_perm_fn <- if (model_type == "xgboost") xgboost_one_perm else rf_one_perm
    one_perm_fn(X, y, params)
  })
  
  iter_df <- bind_rows(rows)
  iter_path <- file.path(
    results_dir,
    sprintf("italy_%s_%s_draw_%02d_feature_importance.csv", model_number, model_type, draw)
  )
  write_csv(iter_df, iter_path)
  
  summary_df <- tibble(
    feature           = names(iter_df),
    mean_importance   = sapply(iter_df, mean),
    median_importance = sapply(iter_df, median),
    sd_importance      = sapply(iter_df, sd)
  ) %>% arrange(desc(median_importance))
  
  summary_path <- file.path(
    results_dir,
    sprintf("italy_%s_%s_draw_%02d_summary.csv", model_number, model_type, draw)
  )
  write_csv(summary_df, summary_path)
  
  cat(sprintf("  wrote %s and %s (%d iterations)\n",
              basename(iter_path), basename(summary_path), nrow(iter_df)))
}

cl <- makeCluster(n_cores, type = "PSOCK", outfile = "")
clusterEvalQ(cl, { library(xgboost); library(ranger) })
clusterExport(cl, varlist = c(
  "n_estimators", "random_over_sample", "resolve_mtry", "coerce_int",
  "xgboost_one_perm", "rf_one_perm"
))

model_numbers <- c("model_1", "model_2")
model_types   <- c("xgboost", "rf")

tryCatch({
  for (mn in model_numbers) {
    for (mt in model_types) {
      for (draw in seq_len(N_DRAWS)) {
        permute_one_draw(mn, mt, draw)
      }
    }
  }
}, finally = {
  stopCluster(cl)
  cat("\nCluster stopped.\n")
})

cat(sprintf("\nTime taken: %s\n", format(Sys.time() - start_time)))
