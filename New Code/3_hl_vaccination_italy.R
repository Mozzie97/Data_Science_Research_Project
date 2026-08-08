library(dplyr)
library(readr)
library(MASS)
library(tibble)
library(ggplot2)
library(tidyr)

filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate

N_DRAWS  <- 20
M        <- N_DRAWS
data_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"
results_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/results"
figures_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/figures"

tidy_polr <- function(fit, hl_measure, draw) {
  ct  <- coef(summary(fit))
  reg <- ct[!grepl("\\|", rownames(ct)), , drop = FALSE]
  tibble(draw = draw, hl_measure = hl_measure,
         term = rownames(reg), estimate = reg[, "Value"],
         std_error = reg[, "Std. Error"])
}

all_coef_draws <- vector("list", N_DRAWS * 2)
aic_bic_rows   <- vector("list", N_DRAWS)
lrt_rows       <- vector("list", N_DRAWS)
coef_idx       <- 1L

for (draw in seq_len(N_DRAWS)) {
  
  df <- read_csv(
    file.path(data_dir,
              sprintf("cleaned_data_italy_hl_draw_%02d.csv", draw)),
    na = character(), show_col_types = FALSE
  ) %>%
    dplyr::filter(vaccination_status %in% c(0L, 1L, 2L)) %>%
    dplyr::mutate(
      vax_ordered  = factor(vaccination_status,
                            levels = c(0L, 1L, 2L), ordered = TRUE),
      age_group    = case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 44 ~ "30-44",
        age >= 45 & age <= 64 ~ "45-64",
        age >= 65             ~ "65+",
        TRUE                  ~ NA_character_
      ),
      age_group    = factor(age_group,
                            levels = c("18-29", "30-44", "45-64", "65+")),
      region_hls19 = factor(region_hls19)
    ) %>%
    dplyr::filter(!is.na(age_group), !is.na(region_hls19))
  
  cat(sprintf("Draw %02d: %d rows\n", draw, nrow(df)))
  
  fit_base <- tryCatch(
    polr(vax_ordered ~ age_group + region_hls19,
         data = df, Hess = TRUE, method = "logistic"),
    error = function(e) { cat("  WARNING: base failed\n"); NULL }
  )
  fit_hl <- tryCatch(
    polr(vax_ordered ~ hl_covid_num + age_group + region_hls19,
         data = df, Hess = TRUE, method = "logistic"),
    error = function(e) { cat("  WARNING: HL failed\n"); NULL }
  )
  fit_vl <- tryCatch(
    polr(vax_ordered ~ vl_num + age_group + region_hls19,
         data = df, Hess = TRUE, method = "logistic"),
    error = function(e) { cat("  WARNING: VL failed\n"); NULL }
  )
  
  if (!is.null(fit_hl)) {
    all_coef_draws[[coef_idx]] <- tidy_polr(fit_hl, "HL_COVID", draw)
    coef_idx <- coef_idx + 1L
  }
  if (!is.null(fit_vl)) {
    all_coef_draws[[coef_idx]] <- tidy_polr(fit_vl, "VL", draw)
    coef_idx <- coef_idx + 1L
  }
  
  aic_bic_rows[[draw]] <- tibble(
    draw     = draw,
    model    = c("Base (age + region)",
                 "HL-COVID (+ hl_covid_num)",
                 "VL (+ vl_num)"),
    n_params = c(
      if (!is.null(fit_base)) length(coef(fit_base)) + length(fit_base$zeta) else NA_integer_,
      if (!is.null(fit_hl))   length(coef(fit_hl))   + length(fit_hl$zeta)   else NA_integer_,
      if (!is.null(fit_vl))   length(coef(fit_vl))   + length(fit_vl$zeta)   else NA_integer_
    ),
    AIC = c(
      if (!is.null(fit_base)) AIC(fit_base) else NA_real_,
      if (!is.null(fit_hl))   AIC(fit_hl)   else NA_real_,
      if (!is.null(fit_vl))   AIC(fit_vl)   else NA_real_
    ),
    BIC = c(
      if (!is.null(fit_base)) BIC(fit_base) else NA_real_,
      if (!is.null(fit_hl))   BIC(fit_hl)   else NA_real_,
      if (!is.null(fit_vl))   BIC(fit_vl)   else NA_real_
    )
  )
  
  make_lrt_row <- function(fit_full, label) {
    if (is.null(fit_base) || is.null(fit_full)) {
      return(tibble(draw = draw, hl_measure = label,
                    logLik_base = NA_real_, logLik_full = NA_real_,
                    chi_sq = NA_real_, df = NA_integer_, p_value = NA_real_))
    }
    ll_base <- as.numeric(logLik(fit_base))
    ll_full <- as.numeric(logLik(fit_full))
    chi_sq  <- 2 * (ll_full - ll_base)
    tibble(draw = draw, hl_measure = label,
           logLik_base = ll_base, logLik_full = ll_full,
           chi_sq = chi_sq, df = 1L,
           p_value = pchisq(chi_sq, df = 1L, lower.tail = FALSE))
  }
  
  lrt_rows[[draw]] <- bind_rows(
    make_lrt_row(fit_hl, "HL_COVID"),
    make_lrt_row(fit_vl, "VL")
  )
}

draw_results <- bind_rows(Filter(Negate(is.null), all_coef_draws))
aic_bic_df   <- bind_rows(aic_bic_rows)
lrt_df       <- bind_rows(lrt_rows)

write_csv(draw_results,
          file.path(results_dir, "3_hl_vaccination_draw_results.csv"))
write_csv(aic_bic_df,
          file.path(results_dir, "3_hl_vaccination_aic_bic.csv"))
write_csv(lrt_df,
          file.path(results_dir, "3_hl_vaccination_lrt.csv"))

cat(sprintf("\nCoefficients: %d rows\nAIC/BIC: %d rows\nLRT: %d rows\n",
            nrow(draw_results), nrow(aic_bic_df), nrow(lrt_df)))

pooled <- draw_results %>%
  dplyr::group_by(hl_measure, term) %>%
  dplyr::summarise(
    n_draws = n(),
    Q_bar   = mean(estimate),
    U_bar   = mean(std_error^2),
    B       = var(estimate),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    T_total    = U_bar + (1 + 1 / M) * B,
    pooled_se  = sqrt(T_total),
    z_stat     = Q_bar / pooled_se,
    p_value    = 2 * pnorm(abs(z_stat), lower.tail = FALSE),
    OR         = exp(Q_bar),
    OR_lower95 = exp(Q_bar - 1.96 * pooled_se),
    OR_upper95 = exp(Q_bar + 1.96 * pooled_se)
  ) %>%
  dplyr::select(hl_measure, term, OR, OR_lower95, OR_upper95,
                Q_bar, pooled_se, z_stat, p_value, n_draws)

write_csv(pooled,
          file.path(results_dir, "3_hl_vaccination_pooled_results.csv"))
cat(sprintf("Pooled results: %d rows\n\n", nrow(pooled)))


base_theme <- theme_bw(base_size = 12) +
  theme(
    strip.background  = element_rect(fill = "#2E75B6", colour = NA),
    strip.text        = element_text(colour = "white", face = "bold"),
    legend.position   = "bottom",
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, colour = "grey40"),
    panel.grid.minor  = element_blank()
  )

hl_colours <- c("HL_COVID" = "#2E75B6", "VL" = "#E36C09")
model_colours <- c(
  "Base (age + region)"       = "#7F7F7F",
  "HL-COVID (+ hl_covid_num)" = "#2E75B6",
  "VL (+ vl_num)"             = "#E36C09"
)

aic_long <- aic_bic_df %>%
  tidyr::pivot_longer(cols = c(AIC, BIC),
                      names_to = "criterion", values_to = "value")

p1 <- ggplot(aic_long, aes(x = draw, y = value,
                           colour = model, linetype = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ criterion, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = model_colours, name = "Model") +
  scale_linetype_manual(
    values = c("Base (age + region)"       = "dashed",
               "HL-COVID (+ hl_covid_num)" = "solid",
               "VL (+ vl_num)"             = "solid"),
    name = "Model"
  ) +
  scale_x_continuous(breaks = 1:20) +
  labs(
    title    = "Plot 1 — AIC and BIC across 20 simulation draws",
    subtitle = "Lower = better fit. Base model has one fewer parameter than the HL and VL models.",
    x = "Draw", y = "Information criterion"
  ) +
  base_theme +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

ggsave(file.path(figures_dir, "plot1_aic_bic.png"),
       plot = p1, width = 10, height = 5, dpi = 150)
cat("Plot 1 saved: plot1_aic_bic.png\n")

chi2_crit <- qchisq(0.95, df = 1)   # 3.841

p2 <- ggplot(lrt_df,
             aes(x = draw, y = chi_sq,
                 colour = hl_measure, shape = p_value < 0.05)) +
  geom_hline(yintercept = chi2_crit,
             linetype = "dashed", colour = "red", linewidth = 0.7) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 3) +
  annotate("text", x = 0.4, y = chi2_crit + 0.3,
           label = sprintf("Critical value = %.3f (df = 1, \u03b1 = 0.05)",
                           chi2_crit),
           colour = "red", size = 3, hjust = 0) +
  scale_colour_manual(values = hl_colours, name = "HL measure") +
  scale_shape_manual(
    values = c("TRUE" = 17, "FALSE" = 16),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
    name   = "Significance"
  ) +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Plot 2 — Likelihood Ratio Test: chi-squared statistic across 20 draws",
    subtitle = "H\u2080: adding HL does not improve fit over age + region. Triangles = significant at 5%.",
    x = "Draw", y = "Chi-squared statistic (df = 1)"
  ) +
  base_theme

ggsave(file.path(figures_dir, "plot2_lrt_chisq.png"),
       plot = p2, width = 10, height = 5, dpi = 150)
cat("Plot 2 saved: plot2_lrt_chisq.png\n")

pooled_coef <- pooled %>%
  dplyr::filter(!grepl("\\|", term)) %>%
  dplyr::mutate(
    term_label = case_when(
      term == "hl_covid_num"                  ~ "HL-COVID score",
      term == "vl_num"                        ~ "VL score",
      term == "age_group30-44"                ~ "Age 30-44 vs 18-29",
      term == "age_group45-64"                ~ "Age 45-64 vs 18-29",
      term == "age_group65+"                  ~ "Age 65+ vs 18-29",
      term == "region_hls19North East"        ~ "Region: North East vs Centre",
      term == "region_hls19North West"        ~ "Region: North West vs Centre",
      term == "region_hls19South and Islands" ~ "Region: South & Islands vs Centre",
      TRUE ~ term
    ),
    sort_key = case_when(
      grepl("HL-COVID score|VL score", term_label) ~ 1,
      grepl("Age",                     term_label) ~ 2,
      grepl("Region",                  term_label) ~ 3,
      TRUE ~ 4
    ),
    significant = p_value < 0.05
  ) %>%
  dplyr::arrange(sort_key, term_label) %>%
  dplyr::mutate(
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )

p3 <- ggplot(pooled_coef,
             aes(x = OR, y = term_label,
                 colour = hl_measure, shape = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = OR_lower95, xmax = OR_upper95),
                 height = 0.25, linewidth = 0.7,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = hl_colours, name = "HL measure") +
  scale_shape_manual(
    values = c("TRUE" = 17, "FALSE" = 16),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
    name   = "Significance"
  ) +
  scale_x_log10() +
  labs(
    title    = "Plot 3 — Pooled odds ratios and 95% confidence intervals",
    subtitle = "Rubin's Rules pooling across 20 draws. OR > 1: higher HL associated with more vaccination. Log scale.",
    x = "Odds ratio (log scale)", y = NULL
  ) +
  base_theme +
  theme(panel.grid.major.y = element_line(colour = "grey90"))

ggsave(file.path(figures_dir, "plot3_pooled_OR_forest.png"),
       plot = p3, width = 10, height = 6, dpi = 150)
cat("Plot 3 saved: plot3_pooled_OR_forest.png\n")

cat(sprintf("\nAll outputs saved to: %s\n", data_dir))
