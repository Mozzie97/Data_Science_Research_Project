library(dplyr)
library(readr)

# Load raw data
# Treat blank strings and the literal "__NA__" as missing
df <- read_csv(
  "australia.csv",
  na = c("", " ", "__NA__", "NA"),
  guess_max = 100000     # checks all rows to see if there are different data types 
)                        # in the same column

# Count missing values per column
missing_value_df <- tibble(
  `Variable Name`       = names(df),
  `Missing Value Count` = sapply(df, function(x) sum(is.na(x)))
) %>%
  arrange(`Missing Value Count`, `Variable Name`)

# Save as csv file 
write_csv(missing_value_df, "missing_value_counts.csv")

