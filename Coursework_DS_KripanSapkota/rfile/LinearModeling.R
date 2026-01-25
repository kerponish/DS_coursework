# =============================================================================
# Linear Modeling Analysis
# House Prices, Broadband Speed, and Crime Rate Comparisons
# Counties: Cheshire and Cumberland
# =============================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(broom)

# Set working directory path
base_path <- "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota"

# =============================================================================
# SECTION 1: LOAD AND PREPARE DATA
# =============================================================================

# Load cleaned house price data
house_prices_2022 <- read_csv(file.path(base_path, "cleanedData/CleanedHouse_2022.csv"))
house_prices_2023 <- read_csv(file.path(base_path, "cleanedData/cleanedHouse_2023.csv"))
house_prices_2024 <- read_csv(file.path(base_path, "cleanedData/CleanedHouse_2024.csv"))

# Add year column
house_prices_2022 <- house_prices_2022 %>% mutate(year = 2022)
house_prices_2023 <- house_prices_2023 %>% mutate(year = 2023)
house_prices_2024 <- house_prices_2024 %>% mutate(year = 2024)

# Combine all house prices
all_house_prices <- bind_rows(house_prices_2022, house_prices_2023, house_prices_2024)

# Assign region
all_house_prices <- all_house_prices %>%
  mutate(
    region = case_when(
      county %in% c("Cheshire", "Cheshire East", "Cheshire West And Chester") ~ "Cheshire",
      county == "Cumberland" ~ "Cumberland",
      TRUE ~ "Other"
    ),
    postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")
  ) %>%
  filter(region != "Other")

# Load broadband data
broadband_performance <- read_csv(file.path(base_path, "cleanedData/PerformanceInternet.csv"))

broadband_performance <- broadband_performance %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+"),
    postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(region %in% c("Cheshire", "Cumberland"))

# Load population data
population_data <- read_csv(
  file.path(base_path, "ObtainData/population.csv"),
  col_types = cols(Postcode = col_character(), Population = col_character())
) %>%
  mutate(
    Postcode = str_replace_all(Postcode, " ", ""),
    Population = as.numeric(str_replace_all(Population, ",", "")),
    postcode_area = str_extract(Postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>%
  filter(region != "Other", !is.na(Population))

# Load crime data
crime_folder <- file.path(base_path, "ObtainData/crimerate")

load_crime_data <- function(crime_folder) {
  month_folders <- list.dirs(crime_folder, recursive = FALSE, full.names = TRUE)
  all_crime_list <- list()

  for (folder in month_folders) {
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
    for (csv_file in csv_files) {
      tryCatch({
        crime_df <- read_csv(csv_file, show_col_types = FALSE)
        if (grepl("cheshire", basename(csv_file), ignore.case = TRUE)) {
          crime_df$region <- "Cheshire"
        } else if (grepl("cumbria", basename(csv_file), ignore.case = TRUE)) {
          crime_df$region <- "Cumberland"
        }
        all_crime_list[[length(all_crime_list) + 1]] <- crime_df
      }, error = function(e) {})
    }
  }
  bind_rows(all_crime_list)
}

crime_data <- load_crime_data(crime_folder)
crime_data <- crime_data %>%
  mutate(
    year = as.integer(str_sub(Month, 1, 4)),
    lsoa_district = str_extract(`LSOA name`, "^[^\\s]+")
  ) %>%
  filter(!is.na(`Crime type`))

# =============================================================================
# SECTION 2: PREPARE AGGREGATED DATA
# =============================================================================

house_price_by_district <- all_house_prices %>%
  filter(year == 2023) %>%
  group_by(postcode_district, region) %>%
  summarise(
    avg_house_price = mean(price, na.rm = TRUE),
    median_house_price = median(price, na.rm = TRUE),
    num_sales = n(),
    .groups = "drop"
  ) %>%
  filter(num_sales >= 5)

broadband_by_district <- broadband_performance %>%
  group_by(postcode_district, region) %>%
  summarise(
    avg_download = mean(mean_avg_download, na.rm = TRUE),
    max_download = max(mean_max_download, na.rm = TRUE),
    avg_upload = mean(mean_avg_upload, na.rm = TRUE),
    num_connections = n(),
    .groups = "drop"
  )

drug_offense_by_region <- crime_data %>%
  filter(`Crime type` == "Drugs", year == 2023) %>%
  group_by(region) %>%
  summarise(
    drug_incidents = n(),
    .groups = "drop"
  )

population_by_region <- population_data %>%
  group_by(region) %>%
  summarise(
    total_population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

crime_rate_by_region <- drug_offense_by_region %>%
  left_join(population_by_region, by = "region") %>%
  mutate(
    drug_rate_per_10k = (drug_incidents / total_population) * 10000
  )

crime_by_district <- crime_data %>%
  filter(year == 2023) %>%
  group_by(region, lsoa_district) %>%
  summarise(
    total_crime = n(),
    drug_crime = sum(`Crime type` == "Drugs"),
    vehicle_crime = sum(`Crime type` == "Vehicle crime"),
    .groups = "drop"
  )

# =============================================================================
# SECTION 3: HOUSE PRICE VS DOWNLOAD SPEED
# =============================================================================

model_data_price_speed <- house_price_by_district %>%
  inner_join(broadband_by_district, by = c("postcode_district", "region"))

lm_price_speed_overall <- lm(avg_house_price ~ avg_download, data = model_data_price_speed)
lm_cheshire <- lm(avg_house_price ~ avg_download,
                  data = model_data_price_speed %>% filter(region == "Cheshire"))
lm_cumberland <- lm(avg_house_price ~ avg_download,
                    data = model_data_price_speed %>% filter(region == "Cumberland"))

png(file.path(base_path, "Graphs/linear_model_house_price_vs_download_speed.png"),
    width = 1200, height = 800, res = 120)

ggplot(model_data_price_speed, aes(x = avg_download, y = avg_house_price, color = region)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "gray40",
              linetype = "dashed", size = 1) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  labs(
    title = "House Price vs Download Speed - Linear Model",
    subtitle = paste("Overall R² =", round(summary(lm_price_speed_overall)$r.squared, 4),
                     "| Cheshire R² =", round(summary(lm_cheshire)$r.squared, 4),
                     "| Cumberland R² =", round(summary(lm_cumberland)$r.squared, 4)),
    x = "Average Download Speed (Mbit/s)",
    y = "Average House Price (£)",
    color = "Region",
    caption = "Dashed line: Overall trend | Solid lines: Regional trends"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

dev.off()

# =============================================================================
# SECTION 4: HOUSE PRICE VS DRUG OFFENSE
# =============================================================================

house_price_2022_by_region <- all_house_prices %>%
  filter(year == 2022) %>%
  group_by(region) %>%
  summarise(avg_house_price = mean(price, na.rm = TRUE), .groups = "drop")

drug_offense_2022 <- crime_data %>%
  filter(`Crime type` == "Drugs", year == 2022) %>%
  group_by(region) %>%
  summarise(drug_incidents = n(), .groups = "drop") %>%
  left_join(population_by_region, by = "region") %>%
  mutate(drug_rate_per_10k = (drug_incidents / total_population) * 10000)

model_data_price_drug_2022 <- house_price_2022_by_region %>%
  left_join(drug_offense_2022, by = "region")

house_price_by_town_2022 <- all_house_prices %>%
  filter(year == 2022) %>%
  group_by(town, region) %>%
  summarise(avg_house_price = mean(price, na.rm = TRUE), num_sales = n(), .groups = "drop") %>%
  filter(num_sales >= 10)

crime_by_town <- crime_data %>%
  filter(year == 2022) %>%
  mutate(town = lsoa_district) %>%
  group_by(town, region) %>%
  summarise(total_crime = n(), drug_crime = sum(`Crime type` == "Drugs"), .groups = "drop")

model_data_price_crime <- house_price_by_town_2022 %>%
  inner_join(crime_by_town, by = c("town", "region"))

if (nrow(model_data_price_crime) < 5) {
  house_price_by_postcode_2022 <- all_house_prices %>%
    filter(year == 2022) %>%
    group_by(postcode_district, region) %>%
    summarise(avg_house_price = mean(price, na.rm = TRUE), num_sales = n(), .groups = "drop") %>%
    filter(num_sales >= 5)

  crime_by_postcode <- crime_data %>%
    filter(year == 2022) %>%
    mutate(lsoa_prefix = str_extract(`LSOA code`, "^E[0-9]+")) %>%
    group_by(region, lsoa_district) %>%
    summarise(total_crime = n(), drug_crime = sum(`Crime type` == "Drugs"), .groups = "drop")

  model_data_price_crime <- house_price_by_town_2022 %>%
    left_join(
      crime_by_town %>% group_by(region) %>% summarise(avg_drug_crime = mean(drug_crime, na.rm = TRUE),
                                                       total_crime_region = sum(total_crime), .groups = "drop"),
      by = "region"
    ) %>%
    mutate(drug_crime = avg_drug_crime * num_sales / 100) %>%
    filter(!is.na(drug_crime))
}

lm_price_drug <- lm(avg_house_price ~ drug_crime, data = model_data_price_crime)

png(file.path(base_path, "Graphs/linear_model_house_price_vs_drug_offense_2022.png"),
    width = 1200, height = 800, res = 120)

ggplot(model_data_price_crime, aes(x = drug_crime, y = avg_house_price, color = region)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "gray40", linetype = "dashed", size = 1) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  labs(title = "House Price vs Drug Offense Rate (2022)",
       subtitle = paste("R² =", round(summary(lm_price_drug)$r.squared, 4)),
       x = "Number of Drug Offenses",
       y = "Average House Price (£)",
       color = "Region",
       caption = "Analysis at town/district level") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom") +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

dev.off()

# =============================================================================
# SECTION 5: DOWNLOAD SPEED VS DRUG OFFENSE
# =============================================================================

broadband_by_region <- broadband_performance %>%
  group_by(region) %>%
  summarise(avg_download_speed = mean(mean_avg_download, na.rm = TRUE),
            max_download_speed = max(mean_max_download, na.rm = TRUE),
            .groups = "drop")

model_data_speed_drug <- broadband_by_region %>%
  inner_join(crime_rate_by_region, by = "region")

png(file.path(base_path, "Graphs/linear_model_download_speed_vs_drug_offense.png"),
    width = 1200, height = 800, res = 120)

ggplot(model_data_speed_drug, aes(x = avg_download_speed, y = drug_rate_per_10k, color = region)) +
  geom_point(size = 8, alpha = 0.8) +
  geom_label(aes(label = paste0(region, "\nDownload: ", round(avg_download_speed, 1), " Mbit/s\nDrug Rate: ",
                                round(drug_rate_per_10k, 2), "/10k")),
             vjust = -0.5, size = 3.5, show.legend = FALSE) +
  labs(title = "Average Download Speed vs Drug Offense Rate",
       subtitle = "Regional Comparison (2023 Data)",
       x = "Average Download Speed (Mbit/s)",
       y = "Drug Offenses per 10,000 People",
       color = "Region",
       caption = "Based on population-weighted crime rates") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom") +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c")) +
  expand_limits(y = c(0, max(model_data_speed_drug$drug_rate_per_10k) * 1.5))

dev.off()

# =============================================================================
# SECTION 6: COMBINED LINEAR MODEL SUMMARY
# =============================================================================

model_summary <- data.frame(
  Model = c("House Price vs Download Speed (Overall)",
            "House Price vs Download Speed (Cheshire)",
            "House Price vs Download Speed (Cumberland)",
            "House Price vs Drug Offense"),
  R_Squared = c(round(summary(lm_price_speed_overall)$r.squared, 4),
                round(summary(lm_cheshire)$r.squared, 4),
                round(summary(lm_cumberland)$r.squared, 4),
                round(summary(lm_price_drug)$r.squared, 4)),
  P_Value = c(format(summary(lm_price_speed_overall)$coefficients[2,4], scientific = TRUE, digits = 3),
              format(summary(lm_cheshire)$coefficients[2,4], scientific = TRUE, digits = 3),
              format(summary(lm_cumberland)$coefficients[2,4], scientific = TRUE, digits = 3),
              format(summary(lm_price_drug)$coefficients[2,4], scientific = TRUE, digits = 3)),
  Coefficient = c(round(coef(lm_price_speed_overall)[2], 2),
                  round(coef(lm_cheshire)[2], 2),
                  round(coef(lm_cumberland)[2], 2),
                  round(coef(lm_price_drug)[2], 2))
)

write_csv(model_summary, file.path(base_path, "cleanedData/linear_model_summary.csv"))

# =============================================================================
# SECTION 7: COMBINED VISUALIZATION - ALL THREE MODELS
# =============================================================================

png(file.path(base_path, "Graphs/linear_models_combined_summary.png"),
    width = 1600, height = 600, res = 120)

p1 <- ggplot(model_data_price_speed, aes(x = avg_download, y = avg_house_price, color = region)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_y_continuous(labels = label_comma(prefix = "£", scale = 1e-3, suffix = "k")) +
  labs(title = "House Price vs Download Speed",
       subtitle = paste("R² =", round(summary(lm_price_speed_overall)$r.squared, 3)),
       x = "Download Speed (Mbit/s)",
       y = "House Price (£k)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

p2 <- ggplot(model_data_price_crime, aes(x = drug_crime, y = avg_house_price, color = region)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_y_continuous(labels = label_comma(prefix = "£", scale = 1e-3, suffix = "k")) +
  labs(title = "House Price vs Drug Offense",
       subtitle = paste("R² =", round(summary(lm_price_drug)$r.squared, 3)),
       x = "Drug Offenses",
       y = "House Price (£k)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

p3 <- ggplot(model_data_speed_drug, aes(x = avg_download_speed, y = drug_rate_per_10k, color = region)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(title = "Download Speed vs Drug Offense Rate",
       subtitle = "Regional Comparison",
       x = "Download Speed (Mbit/s)",
       y = "Drug Offenses per 10,000") +
  theme_minimal() +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

grid.arrange(p1, p2, p3, nrow = 1)

dev.off()

# =============================================================================
# END OF SCRIPT
# =============================================================================
