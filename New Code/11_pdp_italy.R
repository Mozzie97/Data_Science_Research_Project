library(readr)
library(dplyr)
library(pdp)
library(xgboost)
library(ranger)

n_grid_points <- 51L   
top_n         <- 10L   # number of top-by-importance features
N_DRAWS       <- 20L

data_dir    <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"
models_dir  <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/models"

one_hot_families <- list(
  vaccination_status = c("vaccination_status_1", "vaccination_status_2", "vaccination_status_3"),
  i9_health           = c("i9_health_Not sure", "i9_health_Yes"),
  i11_health          = c("i11_health_Not sure", "i11_health_Somewhat unwilling",
                          "i11_health_Somewhat willing", "i11_health_Very unwilling",
                          "i11_health_Very willing"),
  employment_status   = c("employment_status_Full time student", "employment_status_Not working",
                          "employment_status_Other", "employment_status_Part time employment",
                          "employment_status_Retired", "employment_status_Unemployed"),
  WCRex1              = c("WCRex1_Somewhat badly", "WCRex1_Somewhat well",
                          "WCRex1_Very badly", "WCRex1_Very well"),
  WCRex2              = c("WCRex2_A lot of confidence", "WCRex2_Don't know",
                          "WCRex2_No confidence at all", "WCRex2_Not very much confidence"),
  PHQ4_1              = c("PHQ4_1_Nearly every day", "PHQ4_1_Not at all",
                          "PHQ4_1_Prefer not to say", "PHQ4_1_Several days"),
  PHQ4_2              = c("PHQ4_2_Nearly every day", "PHQ4_2_Not at all",
                          "PHQ4_2_Prefer not to say", "PHQ4_2_Several days"),
  PHQ4_3              = c("PHQ4_3_Nearly every day", "PHQ4_3_Not at all",
                          "PHQ4_3_Prefer not to say", "PHQ4_3_Several days"),
  PHQ4_4              = c("PHQ4_4_Nearly every day", "PHQ4_4_Not at all",
                          "PHQ4_4_Prefer not to say", "PHQ4_4_Several days"),
  d1_comorbidities    = c("d1_comorbidities_Prefer_not_to_say", "d1_comorbidities_Yes"),
  region              = c("region_Islands", "region_North east",
                          "region_North west", "region_South")
)

always_include <- c("vaccination_status", "hl_covid_num", "vl_num")

resolve_to_group <- function(feat, families) {
  for (fam_name in names(families)) {
    if (feat %in% families[[fam_name]]) return(fam_name)
  }
  feat
}

get_top_features_pooled <- function(model_number, model_type, top_n = 10L) {
  medians_by_draw <- lapply(seq_len(N_DRAWS), function(draw) {
    p <- file.path(
      results_dir,
      sprintf("italy_%s_%s_draw_%02d_summary.csv", model_number, model_type, draw)
    )
    s <- read_csv(p, show_col_types = FALSE)
    setNames(s$median_importance, s$feature)
  })
  
  all_features <- names(medians_by_draw[[1]])
  avg_medians <- sapply(all_features, function(f) {
    mean(sapply(medians_by_draw, function(v) v[[f]]))
  })
  
  names(sort(avg_medians, decreasing = TRUE))[seq_len(min(top_n, length(avg_medians)))]
}

build_categorical_grid <- function(family_cols) {
  k <- length(family_cols)
  ref_row <- as.data.frame(matrix(0L, nrow = 1, ncol = k))
  names(ref_row) <- family_cols
  
  cat_rows <- lapply(seq_len(k), function(i) {
    row <- as.data.frame(matrix(0L, nrow = 1, ncol = k))
    names(row) <- family_cols
    row[[i]] <- 1L
    row
  })
  
  grid <- do.call(rbind, c(list(ref_row), cat_rows))
  rownames(grid) <- NULL
  grid
}

pred_fun_xgb <- function(object, newdata) {
  Xm    <- as.matrix(newdata)
  dtest <- xgboost::xgb.DMatrix(data = Xm)
  as.numeric(predict(object, newdata = dtest))
}

pred_fun_rf <- function(object, newdata) {
  df <- as.data.frame(newdata)
  names(df) <- make.names(names(df), unique = TRUE)
  p <- predict(object, data = df)$predictions
  if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, ncol(p)])
}

compute_pdp_for_group <- function(group, X_train, model_obj, pred_fun,
                                  families, n_grid_points) {
  if (group %in% names(families)) {
    family_cols <- intersect(families[[group]], names(X_train))
    if (length(family_cols) == 0) {
      warning(sprintf("None of group '%s's columns found in X_train; skipping.", group))
      return(NULL)
    }
    grid <- build_categorical_grid(family_cols)
    
    pd <- tryCatch(
      pdp::partial(
        object    = model_obj,
        pred.var  = family_cols,
        pred.grid = grid,
        pred.fun  = pred_fun,
        train     = as.data.frame(X_train),
        progress  = "none"
      ),
      error = function(e) {
        warning(sprintf("  pdp failed for group '%s': %s", group, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(pd)) return(NULL)
    
    category_labels <- c("(reference)", family_cols)
    data.frame(
      `_vname_` = group,
      `_x_`     = category_labels,
      `_yhat_`  = pd[["yhat"]],
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    if (!group %in% names(X_train)) {
      warning(sprintf("  Feature '%s' not found in X_train; skipping.", group))
      return(NULL)
    }
    pd <- tryCatch(
      pdp::partial(
        object          = model_obj,
        pred.var        = group,
        train           = as.data.frame(X_train),
        pred.fun        = pred_fun,
        grid.resolution = n_grid_points,
        progress        = "none"
      ),
      error = function(e) {
        warning(sprintf("  pdp failed for '%s': %s", group, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(pd)) return(NULL)
    
    data.frame(
      `_vname_` = group,
      `_x_`     = as.character(pd[[group]]),
      `_yhat_`  = pd[["yhat"]],
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

model_numbers <- c("model_1", "model_2")
model_types   <- c("rf", "xgboost")

for (model_number in model_numbers) {
  for (model_type in model_types) {
    cat(sprintf("\n=== %s - %s ===\n", model_number, model_type))
    
    top10_raw    <- get_top_features_pooled(model_number, model_type, top_n = top_n)
    top10_groups <- unique(sapply(top10_raw, resolve_to_group, families = one_hot_families))
    
    multi_draw_groups  <- unique(always_include)
    single_draw_groups <- setdiff(union(top10_groups, multi_draw_groups), multi_draw_groups)
    
    cat(sprintf("  top-%d by pooled importance resolved to groups: %s\n",
                top_n, paste(top10_groups, collapse = ", ")))
    cat(sprintf("  always-included (draws 1-%d): %s\n",
                N_DRAWS, paste(multi_draw_groups, collapse = ", ")))
    cat(sprintf("  single-draw-only (draw 1): %s\n",
                paste(single_draw_groups, collapse = ", ")))
    
    pred_fun <- if (model_type == "xgboost") pred_fun_xgb else pred_fun_rf
    label    <- sprintf("%s_%s_explainer", model_number, model_type)
    
    all_rows <- list()
    
    for (draw in seq_len(N_DRAWS)) {
      groups_needed <- if (draw == 1) {
        union(single_draw_groups, multi_draw_groups)
      } else {
        multi_draw_groups
      }
      if (length(groups_needed) == 0) next
      
      model_path <- file.path(
        models_dir, sprintf("italy_%s_%s_draw_%02d.rds", model_number, model_type, draw)
      )
      X_train <- read_csv(
        file.path(data_dir, sprintf("X_train_%s_draw_%02d.csv", model_number, draw)),
        na = character(), show_col_types = FALSE
      )
      M <- readRDS(model_path)
      
      for (group in groups_needed) {
        cat(sprintf("  draw %02d - %s\n", draw, group))
        res <- compute_pdp_for_group(group, X_train, M, pred_fun, one_hot_families, n_grid_points)
        if (!is.null(res)) {
          res$`_label_` <- label
          res$draw       <- draw
          all_rows[[length(all_rows) + 1]] <- res
        }
      }
    }
    
    out_df <- bind_rows(all_rows)
    out_path <- file.path(
      results_dir, sprintf("italy_%s_%s_pdp_results.csv", model_number, model_type)
    )
    write_csv(out_df, out_path)
    cat(sprintf("  wrote %s (%d rows)\n", out_path, nrow(out_df)))
  }
}

cat("\nDone.\n")
