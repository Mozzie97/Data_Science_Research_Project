# Purpose:
#   Create tornado (butterfly) plots of the top-ten stable feature importances
#   for the four final tree-based model pairs:
#     model_1  (face mask wearing)
#     model_2  (general protective health behaviour)
#   Each pair combines the random forest (panel a) and XGBoost (panel b)
#   tornado plots into a single saved figure, matching Figures 1 and 2 of
#   the paper (rsos.241941).
#
#   For each model pair and model type the script:
#     1. Loads the permutation-importance CSVs written by
#        9_permute_feature_importance.R for the pre- and post-mandate periods
#        (model_<N>a_<type>_feature_importance.csv and
#         model_<N>b_<type>_feature_importance.csv).
#     2. Computes per-feature median importance and interquartile range across
#        all permutation iterations (Section 2.5.3).
#     3. Filters out the state feature, retains the top 10 per period, and
#        mirrors the pre-mandate bars to the left (negative x) for the tornado
#        layout.
#     4. Assigns each feature to one of six named groups and colours bars
#        accordingly.
#     5. Saves the combined (rf / xgboost) figure to
#        ../Reproduction Code/figures/mean_feature_importance_<model_number>_both_models.png
#
#   Design notes:
#   * Uses library() calls only -- no pacman, matching the rest of the pipeline.
#   * Path construction uses sprintf() for consistency with the rest of the
#     pipeline (no here:: or glue::).
#   * Colour palette: RColorBrewer "Set1" with six colours, one per group,
#     replacing the harrypotter dependency while preserving the same six named
#     groups shown in the paper legend.
#   * Output dimensions: height = 12 in, width = 12 in, dpi = 600, matching
#     the original script.

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------
text_size <- 16
height    <- 12
dpi       <- 600

# Six groups matching the paper legend (Figures 1 and 2)
grouping_list <- c(
  "Self protective behaviours",
  "Demographics",
  "Health, mental health and wellbeing",
  "Perception of illness threat",
  "Time",
  "Trust in government"
)

colour_palette <- setNames(
  brewer.pal(length(grouping_list), "Set1"),
  grouping_list
)

# ---------------------------------------------------------------------------
# Helper: load and summarise one model's importance CSVs
# ---------------------------------------------------------------------------
load_data <- function(model_type, model_number) {
  summarise_csv <- function(path, period_label) {
    df <- read_csv(path, col_types = cols())
    df %>%
      select(-1) %>%                         # drop iteration-index column
      pivot_longer(everything(), names_to = "name", values_to = "value") %>%
      group_by(name) %>%
      summarise(
        mean_importance = median(value),
        q1              = quantile(value, 0.25),
        q2              = quantile(value, 0.75),
        .groups         = "drop"
      ) %>%
      mutate(time = period_label)
  }

  df_pre  <- summarise_csv(
    sprintf("../Reproduction Code/results/%sa_%s_feature_importance.csv",
            model_number, model_type),
    "pre-mandate"
  )
  df_post <- summarise_csv(
    sprintf("../Reproduction Code/results/%sb_%s_feature_importance.csv",
            model_number, model_type),
    "mandate"
  )

  df <- bind_rows(
    df_pre  %>% arrange(desc(mean_importance)),
    df_post %>% arrange(desc(mean_importance))
  ) %>%
    filter(!grepl("state", name, ignore.case = TRUE)) %>%
    group_by(time) %>%
    slice_head(n = 10) %>%
    ungroup() %>%
    mutate(
      name = forcats::fct_reorder(name, mean_importance),
      time = factor(
        ifelse(time == "mandate", "After mandates", "Before mandates"),
        levels = c("Before mandates", "After mandates")
      )
    )

  # Mirror pre-mandate bars to the left
  df$mean_importance[df$time == "Before mandates"] <-
    -df$mean_importance[df$time == "Before mandates"]
  df$q1[df$time == "Before mandates"] <-
    -df$q1[df$time == "Before mandates"]
  df$q2[df$time == "Before mandates"] <-
    -df$q2[df$time == "Before mandates"]

  # Assign feature groups
  df <- df %>%
    mutate(group_var = case_when(
      grepl("i2|i9|i11|protective", name)        ~ grouping_list[1],
      grepl("age|house|employ|gender|state", name) ~ grouping_list[2],
      grepl("PHQ|cantril|d1",  name)              ~ grouping_list[3],
      grepl("r1",              name)              ~ grouping_list[4],
      grepl("week",            name)              ~ grouping_list[5],
      grepl("WCR",             name)              ~ grouping_list[6]
    ))

  df
}

# ---------------------------------------------------------------------------
# Helper: clean feature-name labels for the y-axis
# ---------------------------------------------------------------------------
labels_clean <- function(labels_original) {
  labels <- gsub("_", " ", labels_original)
  labels <- tools::toTitleCase(labels)
  labels <- gsub(" Nomask Scale", "", labels)

  # State (XX)
  idx <- grepl("State", labels)
  labels[idx] <- sub("State (.*)", "State (\\1)", gsub("State ", "State (", labels[idx]))
  labels[idx] <- paste0(labels[idx], ")")

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

  labels[grepl("I2 Health",  labels)] <- "Non-Household Contacts"
  labels[grepl("D1",         labels)] <- "Has Comorbidities"
  labels[grepl("R1 1",       labels)] <- "Perceived Severity"
  labels[grepl("R1 2",       labels)] <- "Perceived Susceptibility"

  # PHQ-4 items
  idx <- grepl("Phq4", labels)
  labels[idx] <- paste0(labels[idx], ")")
  labels[idx] <- sub("Phq4 1 ", "Little interest or pleasure \n(", labels[idx])
  labels[idx] <- sub("Phq4 2 ", "Feeling down or depressed \n(",   labels[idx])
  labels[idx] <- sub("Phq4 3 ", "Feeling nervous or anxious \n(",  labels[idx])
  labels[idx] <- sub("Phq4 4 ", "Worrying (",                      labels[idx])

  # WCRex2 -> Confidence in response (XX)
  idx <- grepl("Wcrex2", labels)
  labels[idx] <- paste0(labels[idx], ")")
  labels[idx] <- sub("Wcrex2 ", "Confidence in response\n(", labels[idx])

  labels
}

# ---------------------------------------------------------------------------
# Build one tornado panel
# ---------------------------------------------------------------------------
create_tornado_plot <- function(model_type, model_number) {
  df <- load_data(model_type = model_type, model_number = model_number)

  # Invisible point to keep x-axis symmetric in each facet
  x_limit <- round(max(abs(df$q1), abs(df$q2)) + 0.005, 2)
  df_tmp <- df %>%
    group_by(time) %>%
    slice_head(n = 1) %>%
    mutate(mean_importance = ifelse(time == "Before mandates", -x_limit, x_limit)) %>%
    ungroup()

  labels_original <- levels(df$name)
  labels          <- labels_clean(labels_original)

  df <- df %>%
    mutate(group_var = factor(group_var, levels = names(colour_palette)))

  ggplot(df, aes(x = mean_importance, y = name, fill = group_var)) +
    geom_col(show.legend = TRUE) +
    geom_point(data = df_tmp, colour = NA) +
    geom_errorbar(aes(xmin = q1, xmax = q2), width = 0.5) +
    facet_wrap(~time, scales = "free_x") +
    scale_x_continuous(
      expand = c(0, 0),
      labels = function(x) signif(abs(x), 3)
    ) +
    scale_y_discrete(labels = labels) +
    scale_fill_manual(values = colour_palette) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(y = NULL, x = "Median feature importance", fill = NULL) +
    theme_bw() +
    theme(
      panel.spacing.x  = unit(0, "mm"),
      legend.position  = "none",
      text             = element_text(size = text_size, family = "Times New Roman"),
      axis.text.y      = element_text(size = 10),
      strip.text       = element_text(size = 22),
      legend.text      = element_text(size = 14),
      plot.margin      = margin(t = 1, r = 15, b = 1, l = 1)
    )
}

# ---------------------------------------------------------------------------
# Produce and save combined figures for model_1 and model_2
# ---------------------------------------------------------------------------
for (model_number in c("model_1", "model_2")) {
  p_rf  <- create_tornado_plot(model_number = model_number, model_type = "rf")
  p_xgb <- create_tornado_plot(model_number = model_number, model_type = "xgboost") +
    theme(legend.position = "bottom")

  p <- (p_rf / p_xgb) +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

  ggsave(
    sprintf("../Reproduction Code/figures/mean_feature_importance_%s_both_models.png",
            model_number),
    plot   = p,
    height = height,
    width  = height,
    dpi    = dpi
  )
}
