# =============================================================================
# Data Science Coursework - EDA, Visualization, Linear Modeling & Recommendation
# Counties: Cheshire and Cumberland (UK)

# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)

# Set working directory path
base_path <- "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota"

# =============================================================================
# SECTION 1: DATA LOADING AND PREPROCESSING
# =============================================================================

# --- 1.1 Load House Price Data ---
house_prices_2022 <- read_csv(file.path(base_path, "cleanedData/CleanedHouse_2022.csv"))
house_prices_2023 <- read_csv(file.path(base_path, "cleanedData/cleanedHouse_2023.csv"))
house_prices_2024 <- read_csv(file.path(base_path, "cleanedData/CleanedHouse_2024.csv"))


# Add year column
house_prices_2022 <- house_prices_2022 %>% mutate(year = 2022)
house_prices_2023 <- house_prices_2023 %>% mutate(year = 2023)
house_prices_2024 <- house_prices_2024 %>% mutate(year = 2024)

# Combine all house price data
all_house_prices <- bind_rows(house_prices_2022, house_prices_2023, house_prices_2024)

# Standardize county names
all_house_prices <- all_house_prices %>%
  mutate(
    region = case_when(
      county %in% c("Cheshire", "Cheshire East", "Cheshire West And Chester") ~ "Cheshire",
      county == "Cumberland" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>% filter(region != "Other")

# --- 1.2 Load Broadband Performance Data ---
broadband_performance <- read_csv(file.path(base_path, "cleanedData/PerformanceInternet.csv"))

# Add postcode area for classification
broadband_performance <- broadband_performance %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  )

# --- 1.3 Load Population Data ---
population_data <- read_csv(
  file.path(base_path, "ObtainData/population.csv"),
  col_types = cols(
    Postcode = col_character(),
    Population = col_character()
  )
)

# Clean population data
population_data <- population_data %>%
  mutate(
    Postcode = str_replace_all(Postcode, " ", ""),
    Population = as.numeric(str_replace_all(Population, ",", ""))
  ) %>% filter(!is.na(Population))

# Extract postcode area
population_data <- population_data %>%
  mutate(
    postcode_area = str_extract(Postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>% filter(region != "Other")

# --- 1.4 Load Crime Data ---
load_crime_data <- function(base_path) {
  crime_folder <- file.path(base_path, "ObtainData/crimerate")
  month_folders <- list.dirs(crime_folder, recursive = FALSE)
  all_crime_data <- list()

  for (folder in month_folders) {
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
    for (file in csv_files) {
      tryCatch({
        crime_df <- read_csv(file, show_col_types = FALSE)
        if (grepl("cheshire", file, ignore.case = TRUE)) crime_df$region <- "Cheshire"
        else if (grepl("cumbria", file, ignore.case = TRUE)) crime_df$region <- "Cumberland"
        all_crime_data[[length(all_crime_data) + 1]] <- crime_df
      }, error = function(e) {})
    }
  }
  bind_rows(all_crime_data)
}

crime_data <- load_crime_data(base_path)
crime_data <- crime_data %>%
  mutate(
    year = as.numeric(str_sub(Month, 1, 4)),
    month_num = as.numeric(str_sub(Month, 6, 7)),
    month_year = Month
  ) %>% filter(!is.na(`Crime type`))

cat("Data loaded successfully!\n")

# =============================================================================
# SECTION 2: HOUSE PRICES EDA AND VISUALIZATION
# =============================================================================

# Average House Prices by Town
avg_house_prices_by_town <- all_house_prices %>%
  group_by(town, region, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    num_sales = n(),
    .groups = "drop"
  ) %>% filter(num_sales >= 5)

# Boxplot 2023
house_prices_2023_summary <- avg_house_prices_by_town %>% filter(year == 2023)
top_towns_2023 <- house_prices_2023_summary %>% group_by(region) %>% top_n(8, num_sales) %>% ungroup()
boxplot_data_2023 <- all_house_prices %>% filter(year == 2023, town %in% top_towns_2023$town)

p1 <- ggplot(boxplot_data_2023, aes(x = reorder(town, price, FUN = median), y = price, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 1000000)) +
  labs(title = "House Prices Distribution by Town (2023)",
       subtitle = "Comparing Cheshire and Cumberland Counties",
       x = "Town", y = "House Price (£)", fill = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 10)) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/boxplot_house_prices_2023.png"), plot = p1, width = 12, height = 8, dpi = 120)
cat("Saved: boxplot_house_prices_2023.png\n")

# Bar Chart 2022
avg_prices_2022 <- avg_house_prices_by_town %>%
  filter(year == 2022) %>% group_by(region) %>% top_n(10, num_sales) %>% ungroup() %>%
  arrange(region, desc(avg_price))

p2 <- ggplot(avg_prices_2022, aes(x = reorder(town, avg_price), y = avg_price, fill = region)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() + facet_wrap(~region, scales = "free_y") +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  labs(title = "Average House Prices by Town (2022)",
       subtitle = "Top Towns in Cheshire and Cumberland",
       x = "Town", y = "Average House Price (£)", fill = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/barchart_avg_house_prices_2022.png"), plot = p2, width = 14, height = 8, dpi = 120)
cat("Saved: barchart_avg_house_prices_2022.png\n")

# Line Graph 2022-2024
yearly_avg_prices <- all_house_prices %>% group_by(region, year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE),
            median_price = median(price, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(yearly_avg_prices, aes(x = year, y = avg_price, color = region, group = region)) +
  geom_line(linewidth = 1.5) + geom_point(size = 4) +
  geom_text(aes(label = paste0("£", round(avg_price/1000, 0), "k")), vjust = -1, size = 4) +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, max(yearly_avg_prices$avg_price) * 1.2)) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(title = "Average House Prices Trend (2022-2024)",
       subtitle = "Comparing Cheshire and Cumberland Counties",
       x = "Year", y = "Average House Price (£)", color = "Region") +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"),
                          legend.position = "bottom") +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/linegraph_house_prices_2022_2024.png"), plot = p3, width = 10, height = 6, dpi = 120)
cat("Saved: linegraph_house_prices_2022_2024.png\n")

# =============================================================================
# SECTION 3: BROADBAND SPEED EDA
# =============================================================================

broadband_by_region <- broadband_performance %>% filter(region %in% c("Cheshire", "Cumberland"))

p4 <- ggplot(broadband_by_region, aes(x = region, y = mean_avg_download, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.5) +
  labs(title = "Distribution of Average Download Speeds",
       subtitle = "Comparing Cheshire and Cumberland Counties",
       x = "Region", y = "Average Download Speed (Mbit/s)", fill = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/boxplot_broadband_speeds.png"), plot = p4, width = 10, height = 6, dpi = 120)
cat("Saved: boxplot_broadband_speeds.png\n")

# Broadband by postcode district
broadband_by_district <- broadband_performance %>%
  mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>%
  group_by(postcode_district, region) %>%
  summarise(
    avg_download = mean(mean_avg_download, na.rm = TRUE),
    max_download = mean(mean_max_download, na.rm = TRUE),
    avg_upload = mean(mean_avg_upload, na.rm = TRUE),
    num_postcodes = n(),
    .groups = "drop"
  ) %>% filter(region %in% c("Cheshire", "Cumberland"))

# =============================================================================
# SECTION 4: CRIME RATE EDA AND VISUALIZATION
# =============================================================================

# --- Boxplot: Drug Offenses ---
drug_offenses <- crime_data %>% filter(`Crime type` == "Drugs") %>%
  group_by(region, `LSOA name`) %>% summarise(drug_count = n(), .groups = "drop")

p5 <- ggplot(drug_offenses, aes(x = region, y = drug_count, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.5) +
  labs(title = "Distribution of Drug Offenses by LSOA",
       subtitle = "Comparing Cheshire and Cumberland Counties",
       x = "Region", y = "Number of Drug Offenses", fill = "Region") +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/boxplot_drug_offenses.png"), plot = p5, width = 10, height = 6, dpi = 120)
cat("Saved: boxplot_drug_offenses.png\n")

# --- Additional crime visualizations (bars, pies, line graphs per 10k people) continue here ---
# =============================================================================
# SECTION 5: LINEAR MODELING VISUALIZATIONS
# =============================================================================

# Prepare modeling data
house_by_district <- all_house_prices %>%
  mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>%
  filter(year == 2023) %>% group_by(postcode_district, region) %>%
  summarise(avg_house_price = mean(price, na.rm = TRUE), .groups = "drop")

modeling_data <- house_by_district %>%
  left_join(broadband_by_district, by = c("postcode_district", "region")) %>%
  filter(!is.na(avg_download))

lm_price_speed <- lm(avg_house_price ~ avg_download, data = modeling_data)

p6 <- ggplot(modeling_data, aes(x = avg_download, y = avg_house_price, color = region)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "black", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  labs(title = "House Price vs Download Speed",
       subtitle = paste("R² =", round(summary(lm_price_speed)$r.squared, 3)),
       x = "Average Download Speed (Mbit/s)", y = "Average House Price (£)", color = "Region") +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/linear_model_price_vs_speed.png"), plot = p6, width = 10, height = 7, dpi = 120)
cat("Saved: linear_model_price_vs_speed.png\n")

# =============================================================================
# SECTION 6: RECOMMENDATION SYSTEM
# =============================================================================

# House Price Score
house_price_scores <- all_house_prices %>% filter(year == 2023) %>%
  group_by(town, region) %>% summarise(avg_price = mean(price, na.rm = TRUE), num_sales = n(), .groups = "drop") %>%
  filter(num_sales >= 5) %>%
  mutate(price_score = 10 * (1 - (avg_price - min(avg_price)) / (max(avg_price) - min(avg_price))))

# Broadband Score
town_postcodes <- all_house_prices %>% distinct(town, postcode)
broadband_by_town <- broadband_by_district %>%
  left_join(town_postcodes %>% mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>% distinct(postcode_district, town), by = "postcode_district") %>%
  filter(!is.na(town)) %>% group_by(town) %>%
  summarise(avg_broadband = mean(avg_download, na.rm = TRUE), .groups = "drop") %>%
  mutate(broadband_score = 10 * (avg_broadband - min(avg_broadband)) / (max(avg_broadband) - min(avg_broadband)))

# Crime Score
crime_by_town <- crime_data %>% filter(year == 2023) %>%
  mutate(town_extracted = str_extract(`LSOA name`, "^[^\\s]+")) %>%
  group_by(region, town_extracted) %>% summarise(total_crime = n(), .groups = "drop") %>%
  rename(town = town_extracted)

crime_scores <- crime_by_town %>%
  mutate(crime_score = 10 * (1 - (total_crime - min(total_crime)) / (max(total_crime) - min(total_crime))))

# Combine Scores
overall_scores <- house_price_scores %>% select(town, region, price_score, avg_price) %>%
  left_join(broadband_by_town %>% select(town, broadband_score, avg_broadband), by = "town") %>%
  left_join(crime_scores %>% select(town, crime_score, total_crime), by = "town") %>%
  group_by(region) %>% mutate(
    broadband_score = ifelse(is.na(broadband_score), mean(broadband_score, na.rm = TRUE), broadband_score),
    crime_score = ifelse(is.na(crime_score), mean(crime_score, na.rm = TRUE), crime_score)
  ) %>% ungroup() %>%
  mutate(broadband_score = ifelse(is.na(broadband_score), 5, broadband_score),
         crime_score = ifelse(is.na(crime_score), 5, crime_score),
         overall_score = 0.4 * price_score + 0.3 * broadband_score + 0.3 * crime_score) %>%
  arrange(desc(overall_score))

top_10_towns <- overall_scores %>% head(10)

print("=== TOP 10 RECOMMENDED TOWNS ===")
print(top_10_towns %>% select(town, region, price_score, broadband_score, crime_score, overall_score))

# Save recommendation results
write_csv(overall_scores, file.path(base_path, "cleanedData/recommendation_scores.csv"))
write_csv(top_10_towns, file.path(base_path, "cleanedData/top10_recommended_towns.csv"))

cat("Recommendation system executed and saved successfully.\n")

# =============================================================================
# SECTION 7: FINAL OUTPUTS
# =============================================================================

# Save preprocessed datasets
write_csv(all_house_prices, file.path(base_path, "cleanedData/all_house_prices_combined.csv"))
write_csv(broadband_performance, file.path(base_path, "cleanedData/broadband_performance_cleaned.csv"))
write_csv(population_data, file.path(base_path, "cleanedData/population_data_cleaned.csv"))
write_csv(crime_data, file.path(base_path, "cleanedData/crime_data_cleaned.csv"))

cat("All cleaned and processed datasets saved successfully.\n")

# Save all plots in one folder
plot_files <- list.files(file.path(base_path, "Graphs"), full.names = TRUE)
for (pf in plot_files) {
  cat("Saved plot:", pf, "\n")
}

cat("All analyses complete. Script executed successfully!\n")
