library(dplyr)
library(readr)
library(zoo)        # for rollmean
library(lubridate)  # for date parsing

df <- read_csv("C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/OxCGRT_compact_national_v1.csv")

df <- df %>%
  filter(CountryCode == "ITA") %>%
  select(Date, `H6M_Facial Coverings`)

df <- df %>%
  mutate(Date = ymd(as.character(Date)))

rolling_days <- 14

df <- df %>%
  arrange(Date) %>%
  mutate(
    `H6M_Facial Coverings` = rollmean(
      `H6M_Facial Coverings`,
      k      = rolling_days,
      fill   = NA,        # First 13 days have no 14-day window so NA
      align  = "right"    # puts the average at the 14th day
    )
  )

mandate_limit <- 3

mandate_date <- df %>%
  filter(`H6M_Facial Coverings` >= mandate_limit) %>%
  slice_head(n = 1) %>%
  pull(Date)

cat(sprintf("Italian national mask mandate start date: %s\n", mandate_date))

write_csv(
  tibble(CountryCode = "ITA", Date = mandate_date),
  "C:/Users/Asus/Adelaide University/Anthony Mays - Covid_Siddika_M/New Code/data/mandate_start_date_italy.csv"
)
