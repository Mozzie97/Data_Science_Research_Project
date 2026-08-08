library(dplyr)
library(readr)
library(tibble)

N_DRAWS      <- 20
n_splits     <- 5     
model_numbers <- c("model_1", "model_2")
model_types   <- c("xgboost", "rf")
metric_names  <- c("test_precision", "test_recall", "test_roc_auc",
                   "test_accuracy", "test_f1")
# short names used in the final wide-format output, in the same order
metric_short  <- c("precision", "recall", "roc_auc", "accuracy", "f1")

results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"

per_draw_rows <- list()
idx <- 1L

for (mn in model_numbers) {
  for (mt in model_types) {
    for (draw in seq_len(N_DRAWS)) {
      rds_path <- file.path(
        results_dir, sprintf("italy_%s_%s_draw_%02d.rds", mn, mt, draw)
      )
      cv_scores <- readRDS(rds_path)
      
      for (m in seq_along(metric_names)) {
        fold_vals <- cv_scores[[metric_names[m]]]
        Q_i <- mean(fold_vals)
        U_i <- var(fold_vals) / n_splits   # variance of the mean of 5 folds
        
        per_draw_rows[[idx]] <- data.frame(
          model_number = mn,
          model_type   = mt,
          draw         = draw,
          metric       = metric_short[m],
          Q_i          = Q_i,
          U_i          = U_i,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }
}

per_draw_df <- bind_rows(per_draw_rows)
write_csv(per_draw_df, file.path(results_dir, "italy_cv_results_per_draw.csv"))
cat(sprintf("Wrote italy_cv_results_per_draw.csv (%d rows)\n", nrow(per_draw_df)))

# Pool across the 20 draws via Rubin's Rules, per (model_number, model_type, metric)
pool_rubins_rules <- function(Q_i, U_i) {
  M       <- length(Q_i)
  Q_bar   <- mean(Q_i)
  U_bar   <- mean(U_i)
  B       <- var(Q_i)
  T_total <- U_bar + (1 + 1 / M) * B
  list(Q_bar = Q_bar, pooled_se = sqrt(T_total))
}

for (mn in model_numbers) {
  
  wide_rows <- list()
  ridx <- 1L
  
  for (mt in model_types) {
    
    row <- list(model_number = mn, model_type = mt)
    
    for (m in metric_short) {
      sub <- per_draw_df %>%
        dplyr::filter(.data$model_number == mn,
                      .data$model_type   == mt,
                      .data$metric       == m)
      
      pooled <- pool_rubins_rules(sub$Q_i, sub$U_i)
      row[[m]]                 <- pooled$Q_bar
      row[[paste0(m, "_std")]] <- pooled$pooled_se
    }
    
    wide_rows[[ridx]] <- as.data.frame(row, stringsAsFactors = FALSE)
    ridx <- ridx + 1L
  }
  
  final_df <- bind_rows(wide_rows)
  
  ordered_cols <- c("model_number", "model_type",
                    unlist(lapply(metric_short, function(m) c(m, paste0(m, "_std")))))
  final_df <- final_df[, ordered_cols]
  
  out_path <- file.path(results_dir, sprintf("italy_%s_final_results.csv", mn))
  write_csv(final_df, out_path)
  cat(sprintf("Wrote %s\n", basename(out_path)))
}

cat("\nDone: 8_collect_results_italy.R\n")
