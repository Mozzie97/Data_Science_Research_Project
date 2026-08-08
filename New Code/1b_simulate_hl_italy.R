library(dplyr)
library(readr)

sample(c("E","S","L"),3,replace=TRUE,prob = c(0.5,0.2,0.3))

set.seed(42)
N_DRAWS <- 20

df <- read_csv(
  "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/cleaned_data_italy.csv",
  na = character(),
  show_col_types = FALSE
)

# Bin age into HLS19 age groups
df <- df %>%
  mutate(
    age_group = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65             ~ "65+",
      TRUE                  ~ NA_character_
    )
  )

# HL lookup  tables. Probabilities are normalised.
hl_covid_age <- data.frame(
  age_group   = c("18-29", "30-44", "45-64", "65+"),
  p_excellent = c(0.566,   0.506,   0.491,   0.448),
  p_sufficient= c(0.216,   0.212,   0.185,   0.228),
  p_limited   = c(0.218,   0.282,   0.324,   0.325),
  stringsAsFactors = FALSE
)

hl_covid_region <- data.frame(
  region_hls19 = c("North West", "North East", "Centre", "South and Islands"),
  p_excellent  = c(0.503,        0.452,         0.542,    0.481),
  p_sufficient = c(0.193,        0.247,          0.206,    0.196),
  p_limited    = c(0.305,        0.301,          0.253,    0.323),
  stringsAsFactors = FALSE
)

# VL lookup tables
vl_age <- data.frame(
  age_group   = c("18-29", "30-44", "45-64", "65+"),
  p_good      = c(0.452,   0.457,   0.475,   0.500),
  p_sufficient= c(0.223,   0.191,   0.191,   0.214),
  p_limited   = c(0.325,   0.352,   0.334,   0.286),
  stringsAsFactors = FALSE
)

vl_region <- data.frame(
  region_hls19 = c("North West", "North East", "Centre", "South and Islands"),
  p_good       = c(0.502,        0.478,         0.512,    0.429),
  p_sufficient = c(0.193,        0.222,          0.193,    0.201),
  p_limited    = c(0.305,        0.300,          0.295,    0.370),
  stringsAsFactors = FALSE
)

# Joining lookup tables
df <- df %>%
  left_join(hl_covid_age,    by = "age_group") %>%
  rename(hl_age_exc = p_excellent,
         hl_age_suf = p_sufficient,
         hl_age_lim = p_limited) %>%
  left_join(hl_covid_region, by = "region_hls19") %>%
  rename(hl_reg_exc = p_excellent,
         hl_reg_suf = p_sufficient,
         hl_reg_lim = p_limited) %>%
  left_join(vl_age,          by = "age_group") %>%
  rename(vl_age_good = p_good,
         vl_age_suf  = p_sufficient,
         vl_age_lim  = p_limited) %>%
  left_join(vl_region,       by = "region_hls19") %>%
  rename(vl_reg_good = p_good,
         vl_reg_suf  = p_sufficient,
         vl_reg_lim  = p_limited)

# Average age + region then normalise each row so probabilities sum to 1.
normalise <- function(a, b, c) {
  s <- a + b + c
  list(p1 = a / s, p2 = b / s, p3 = c / s)
}

hl_probs <- normalise(
  (df$hl_age_exc + df$hl_reg_exc) / 2,
  (df$hl_age_suf + df$hl_reg_suf) / 2,
  (df$hl_age_lim + df$hl_reg_lim) / 2
)
df$hl_p_excellent  <- hl_probs$p1
df$hl_p_sufficient <- hl_probs$p2
df$hl_p_limited    <- hl_probs$p3

vl_probs <- normalise(
  (df$vl_age_good + df$vl_reg_good) / 2,
  (df$vl_age_suf  + df$vl_reg_suf)  / 2,
  (df$vl_age_lim  + df$vl_reg_lim)  / 2
)
df$vl_p_good      <- vl_probs$p1
df$vl_p_sufficient <- vl_probs$p2
df$vl_p_limited   <- vl_probs$p3

# Dropping working columns
working_cols <- c(
  "hl_age_exc", "hl_age_suf", "hl_age_lim",
  "hl_reg_exc", "hl_reg_suf", "hl_reg_lim",
  "vl_age_good", "vl_age_suf", "vl_age_lim",
  "vl_reg_good", "vl_reg_suf", "vl_reg_lim"
)
df <- df %>% select(-any_of(working_cols))

# 3 column probability matrix
assign_categories <- function(p1, p2, p3, labels) {
  n <- length(p1)
  out <- character(n)
  for (i in seq_len(n)) {
    out[i] <- sample(labels, size = 1L, prob = c(p1[i], p2[i], p3[i]))
  }
  out
}

# Generating 20 draws
out_dir <- "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data"

for (draw in seq_len(N_DRAWS)) {
  set.seed(42 + draw)   # distinct but reproducible seed per draw
  
  df_draw <- df %>%
    mutate(
      # HL-COVID: Excellent=3 > Sufficient=2 > Limited=1
      hl_covid_cat = assign_categories(
        hl_p_excellent, hl_p_sufficient, hl_p_limited,
        labels = c("Excellent", "Sufficient", "Limited")
      ),
      hl_covid_cat = factor(hl_covid_cat,
                            levels  = c("Limited", "Sufficient", "Excellent"),
                            ordered = TRUE),
      hl_covid_num = as.integer(hl_covid_cat),   # Limited=1, Sufficient=2, Excellent=3
      
      # VL: Good=3 > Sufficient=2 > Limited=1
      vl_cat = assign_categories(
        vl_p_good, vl_p_sufficient, vl_p_limited,
        labels = c("Good", "Sufficient", "Limited")
      ),
      vl_cat = factor(vl_cat,
                      levels  = c("Limited", "Sufficient", "Good"),
                      ordered = TRUE),
      vl_num = as.integer(vl_cat),               # Limited=1, Sufficient=2, Good=3
      
      draw_id = draw
    ) %>%
    # Removing the probability columns
    select(-hl_p_excellent, -hl_p_sufficient, -hl_p_limited,
           -vl_p_good, -vl_p_sufficient, -vl_p_limited)
  
  out_path <- file.path(
    out_dir,
    sprintf("cleaned_data_italy_hl_draw_%02d.csv", draw)
  )
  write_csv(df_draw, out_path)
  cat(sprintf("Draw %02d written: %d rows, %d columns -> %s\n",
              draw, nrow(df_draw), ncol(df_draw), out_path))
}

cat("\nAll 20 draws written.\n")
