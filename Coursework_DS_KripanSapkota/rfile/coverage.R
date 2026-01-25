

library(tidyverse)
library(janitor)


coverage_raw <- read_csv(
  "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota/ObtainData/Internet/201809_fixed_pc_coverage_r01.csv"
)


coverage_clean <- coverage_raw %>%
  clean_names() %>%
  mutate(
    postcode = str_to_upper(str_trim(postcode)),
    postcode_area = str_extract(postcode, "^[A-Z]{1,2}")
  )


coverage_filtered <- coverage_clean %>%
  filter(
    postcode_area %in% c("CH", "CW", "CA"),
    !is.na(postcode),
    postcode != ""
  )


coverage_postcode_summary <- coverage_filtered %>%
  group_by(postcode) %>%
  summarise(
    sfbb_coverage_pct = round(
      mean(sfbb_availability_percent_premises, na.rm = TRUE), 2
    ),
    ufbb_coverage_pct = round(
      mean(ufbb_availability_percent_premises, na.rm = TRUE), 2
    ),
    fttp_coverage_pct = round(
      mean(fttp_availability_percent_premises, na.rm = TRUE), 2
    ),
    unable_30mb_pct = round(
      mean(percent_of_premises_unable_to_receive_30mbit_s, na.rm = TRUE), 2
    ),
    records = n(),
    .groups = "drop"
  )


print(coverage_postcode_summary)


write_csv(
  coverage_postcode_summary,
  "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota/CoverageInternet_Cleaned.csv"
)
