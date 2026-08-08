library(readr)
library(dplyr)
library(ggplot2)

results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"
plots_dir   <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/plots/pdp_italy"
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

n_common_grid <- 51L

outcome_label <- c(
  model_1 = "face mask wearing",
  model_2 = "general protective behaviour"
)

is_continuous <- function(x_vals) {
  all(!is.na(suppressWarnings(as.numeric(x_vals))))
}

pool_continuous <- function(sub_df, n_common_grid) {
  x_num <- as.numeric(sub_df[["_x_"]])
  draws <- sort(unique(sub_df$draw))
  
  x_common <- seq(min(x_num), max(x_num), length.out = n_common_grid)
  
  interp_mat <- sapply(draws, function(d) {
    this_draw <- sub_df[sub_df$draw == d, ]
    ord <- order(as.numeric(this_draw[["_x_"]]))
    stats::approx(
      x    = as.numeric(this_draw[["_x_"]])[ord],
      y    = this_draw[["_yhat_"]][ord],
      xout = x_common
    )$y
  })
  
  if (is.null(dim(interp_mat))) interp_mat <- matrix(interp_mat, ncol = 1)
  
  data.frame(
    x_common  = x_common,
    mean_yhat = rowMeans(interp_mat, na.rm = TRUE),
    sd_yhat   = apply(interp_mat, 1, function(r) if (length(r) > 1) sd(r, na.rm = TRUE) else 0)
  )
}

pool_categorical <- function(sub_df) {
  x_levels <- unique(sub_df[["_x_"]][sub_df$draw == min(sub_df$draw)])
  
  agg <- sub_df %>%
    group_by(x_cat = .data[["_x_"]]) %>%
    summarise(
      mean_yhat = mean(.data[["_yhat_"]], na.rm = TRUE),
      sd_yhat   = if (dplyr::n() > 1) sd(.data[["_yhat_"]], na.rm = TRUE) else 0,
      .groups = "drop"
    )
  
  agg$x_cat <- factor(agg$x_cat, levels = x_levels)
  agg[order(agg$x_cat), ]
}

plot_pooled_feature <- function(sub_df, model_number, model_type, feat, plots_dir, n_common_grid) {
  n_draws <- length(unique(sub_df$draw))
  y_lab <- sprintf("Predicted probability of %s", outcome_label[[model_number]])
  base_name <- sprintf("italy_%s_%s_pdp_%s", model_number, model_type, feat)
  
  continuous <- is_continuous(sub_df[["_x_"]])
  
  if (continuous) {
    pooled <- pool_continuous(sub_df, n_common_grid)
    p_pooled <- ggplot(pooled, aes(x = x_common, y = mean_yhat)) +
      geom_ribbon(aes(ymin = mean_yhat - sd_yhat, ymax = mean_yhat + sd_yhat),
                  fill = "#2166AC", alpha = 0.2) +
      geom_line(colour = "#2166AC", linewidth = 1) +
      labs(
        title    = sprintf("%s: %s (%s)", feat, y_lab, model_type),
        subtitle = if (n_draws > 1) sprintf("mean +/- 1 SD across %d draws", n_draws) else "single draw (draw 1)",
        x = feat, y = y_lab
      ) +
      theme_minimal()
    
  } else {
    pooled <- pool_categorical(sub_df)
    p_pooled <- ggplot(pooled, aes(x = x_cat, y = mean_yhat, group = 1)) +
      geom_line(colour = "#2166AC") +
      geom_point(colour = "#2166AC", size = 2) +
      geom_errorbar(aes(ymin = mean_yhat - sd_yhat, ymax = mean_yhat + sd_yhat), width = 0.1) +
      labs(
        title    = sprintf("%s: %s (%s)", feat, y_lab, model_type),
        subtitle = if (n_draws > 1) sprintf("mean +/- 1 SD across %d draws", n_draws) else "single draw (draw 1)",
        x = feat, y = y_lab
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }
  
  ggsave(file.path(plots_dir, paste0(base_name, "_pooled.png")), p_pooled,
         width = 7, height = 5, dpi = 150)
  
  cat(sprintf("  %s: wrote _pooled.png (%d draw%s)\n",
              feat, n_draws, if (n_draws > 1) "s" else ""))
}

model_numbers <- c("model_1", "model_2")
model_types   <- c("rf", "xgboost")

for (model_number in model_numbers) {
  for (model_type in model_types) {
    in_path <- file.path(results_dir, sprintf("italy_%s_%s_pdp_results.csv", model_number, model_type))
    if (!file.exists(in_path)) {
      cat(sprintf("Skipping %s / %s: %s not found.\n", model_number, model_type, basename(in_path)))
      next
    }
    
    cat(sprintf("\n=== %s - %s ===\n", model_number, model_type))
    pdp_df <- read_csv(in_path, show_col_types = FALSE)
    
    for (feat in unique(pdp_df[["_vname_"]])) {
      sub_df <- pdp_df[pdp_df[["_vname_"]] == feat, ]
      plot_pooled_feature(sub_df, model_number, model_type, feat, plots_dir, n_common_grid)
    }
  }
}

cat(sprintf("\nDone. Pooled plots written to %s\n", plots_dir))
