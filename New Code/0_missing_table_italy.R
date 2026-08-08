library(dplyr)
library(readr)

df <- read_csv(
  "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/italy.csv",
  na = c("", " ", "__NA__", "NA"),
  guess_max = 100000     # checks all rows to see if there are different data types in the same column
)

missing_value_df <- tibble(
  `Variable Name`       = names(df),
  `Missing Value Count` = sapply(df, function(x) sum(is.na(x)))
) %>%
  arrange(`Missing Value Count`, `Variable Name`)

write_csv(missing_value_df, "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/missing_value_counts_italy.csv")
