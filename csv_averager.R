# =========================================================
# Monthly Mean SST from NOAA OISST — Gulf of Maine
# by Iain MacKillop
# =========================================================

library(dplyr)
library(lubridate)
library(readr)

input_path <- file.path("NOAA GoM OISST", "gom_noaa_oisst.csv")
output_path <- file.path("NOAA GoM OISST", "gom_noaa_oisst_monthly.csv")
sst_data <- read_csv(input_path, show_col_types = FALSE)

monthly_sst <- sst_data %>%
  mutate(
    date = as.Date(date),
    temp_c = temp_c * 100,
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(
    mean_sst = mean(temp_c, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year_month = sprintf("%04d-%02d", year, month)
  ) %>%
  select(year_month, mean_sst)

write_csv(monthly_sst, output_path)

cat("Monthly mean SST written to:", output_path, "\n")
