library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

text_size <- 16
height    <- 12
dpi       <- 600
N_DRAWS   <- 20

results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"
plots_dir   <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/plots/feature_importance"


grouping_list <- c(
  "Self protective behaviours",
  "Demographics",
  "Health, mental health and wellbeing",
  "Perception of illness threat",
  "Time",
  "Trust in government",
  "Vaccination & Health Literacy"
)

colour_palette <- setNames(
  brewer.pal(length(grouping_list), "Set1"),
  grouping_list
)

load_data_italy <- function(model_type, model_number, top_n = 10L) {
  raw_all <- bind_rows(lapply(seq_len(N_DRAWS), function(draw) {
    p <- file.path(
      results_dir,
      sprintf("italy_%s_%s_draw_%02d_feature_importance.csv", model_number, model_type, draw)
    )
    read_csv(p, col_types = cols())
  }))
  
  df <- raw_all %>%
    pivot_longer(everything(), names_to = "name", values_to = "value") %>%
    group_by(name) %>%
    summarise(
      mean_importance = median(value),
      q1              = quantile(value, 0.25),
      q2              = quantile(value, 0.75),
      .groups         = "drop"
    ) %>%
    filter(!grepl("region", name, ignore.case = TRUE)) %>%
    arrange(desc(mean_importance)) %>%
    slice_head(n = top_n) %>%
    mutate(name = forcats::fct_reorder(name, mean_importance))
  
  df <- df %>%
    mutate(group_var = case_when(
      grepl("vaccination_status|hl_covid_num|vl_num", name) ~ grouping_list[7],
      grepl("i2|i9|i11|protective",                   name) ~ grouping_list[1],
      grepl("age|house|employ|gender|region",         name) ~ grouping_list[2],
      grepl("PHQ|cantril|d1",                          name) ~ grouping_list[3],
      grepl("r1",                                      name) ~ grouping_list[4],
      grepl("week",                                    name) ~ grouping_list[5],
      grepl("WCR",                                     name) ~ grouping_list[6],
      TRUE ~ NA_character_
    ))
  
  df
}

labels_clean_italy <- function(labels_original) {
  labels <- gsub("_", " ", labels_original)
  labels <- tools::toTitleCase(labels)
  labels <- gsub(" Nomask Scale", "", labels)   # harmless no-op for Italy, kept for consistency
  
  # Gender (XX)
  idx <- grepl("Gender", labels)
  labels[idx] <- sub("Gender (.*)", "Gender (\\1)", gsub("Gender ", "Gender (", labels[idx]))
  labels[idx] <- paste0(labels[idx], ")")
  
  # Employment Status (XX)
  idx <- grepl("Employment", labels)
  labels[idx] <- paste0(labels[idx], ")")
  labels[idx] <- sub("Employment Status ", "Employment Status \n(", labels[idx])
  
  # i11 -> isolate when instructed / unwell label
  idx <- grepl("I11 Health", labels)
  labels[idx] <- paste0(sub("I11 Health ", "", labels[idx]), " To Isolate")
  
  # i9 -> Isolate If Unwell (XX)
  idx <- grepl("I9 Health", labels)
  labels[idx] <- paste0(labels[idx], ")")
  labels[idx] <- sub("I9 Health ", "Isolate If Unwell (", labels[idx])
  
  labels[grepl("I2 Health", labels)] <- "Non-Household Contacts"
  labels[grepl("D1",        labels)] <- "Has Comorbidities"
  labels[grepl("R1 1",      labels)] <- "Perceived Severity"
  labels[grepl("R1 2",      labels)] <- "Perceived Susceptibility"
  
  # PHQ-4 items
  idx <- grepl("Phq4", labels)
  labels[idx] <- paste0(labels[idx], ")")
  labels[idx] <- sub("Phq4 1 ", "Little interest or pleasure \n(", labels[idx])
  labels[idx] <- sub("Phq4 2 ", "Feeling down or depressed \n(",   labels[idx])
  labels[idx] <- sub("Phq4 3 ", "Feeling nervous or anxious \n(",  labels[idx])
  labels[idx] <- sub("Phq4 4 ", "Worrying (",                      labels[idx])
  
  # WCRex2 -> Confidence in response (XX)  [WCRex1 falls through to generic
  # cleanup, matching Australia's existing behaviour]
  idx <- grepl("Wcrex2", labels)
  labels[idx] <- paste0(labels[idx], ")")
  labels[idx] <- sub("Wcrex2 ", "Confidence in response\n(", labels[idx])
  
  # Italy-specific additions
  
  # Region (XX) -- Italy's equivalent of Australia's State
  idx <- grepl("^Region ", labels)
  labels[idx] <- paste0("Region (", sub("^Region ", "", labels[idx]), ")")
  
  # Vaccination Status (N) -- neutral label; adjust wording if you have the
  # actual codebook definition for what 1/2/3 represent.
  idx <- grepl("^Vaccination Status ", labels)
  labels[idx] <- paste0("Vaccination Status (", sub("^Vaccination Status ", "", labels[idx]), ")")
  
  labels[grepl("Hl Covid Num", labels)] <- "COVID Health Literacy Score"
  labels[grepl("Vl Num",       labels)] <- "Vaccine Literacy Score"
  
  labels
}

create_bar_plot_italy <- function(model_type, model_number) {
  df <- load_data_italy(model_type = model_type, model_number = model_number)
  
  labels_original <- levels(df$name)
  labels          <- labels_clean_italy(labels_original)
  
  df <- df %>%
    mutate(group_var = factor(group_var, levels = names(colour_palette)))
  
  ggplot(df, aes(x = mean_importance, y = name, fill = group_var)) +
    geom_col(show.legend = TRUE) +
    geom_errorbar(aes(xmin = q1, xmax = q2), width = 0.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_y_discrete(labels = labels) +
    scale_fill_manual(values = colour_palette, drop = FALSE) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      y = NULL,
      x = sprintf("Median feature importance (pooled across %d draws)", N_DRAWS),
      fill = NULL
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      text            = element_text(size = text_size),
      axis.text.y     = element_text(size = 10),
      plot.margin     = margin(t = 1, r = 15, b = 1, l = 1)
    )
}

for (model_number in c("model_1", "model_2")) {
  p_rf  <- create_bar_plot_italy(model_number = model_number, model_type = "rf")
  p_xgb <- create_bar_plot_italy(model_number = model_number, model_type = "xgboost") +
    theme(legend.position = "bottom")
  
  p <- (p_rf / p_xgb) +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
  
  out_path <- file.path(
    plots_dir, sprintf("italy_mean_feature_importance_%s_both_models.png", model_number)
  )
  ggsave(out_path, plot = p, height = height, width = height, dpi = dpi)
  cat(sprintf("wrote %s\n", out_path))
}

cat("\nDone.\n")
