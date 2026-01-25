# =============================================================================
# RECOMMENDATION SYSTEM
# Property Investment Recommendation for Cheshire and Cumberland
# Scoring System: House Prices, Broadband Speed, Crime Rate, Population
# =============================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(knitr)

# Set working directory path
base_path <- "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota"

# =============================================================================
# SECTION 1: LOAD ALL REQUIRED DATA
# =============================================================================

cat("Loading data...\n")

# --- 1.1 Load House Price Data ---
house_prices_2022 <- read_csv(file.path(base_path, "cleanedData/CleanedHouse_2022.csv"))
house_prices_2023 <- read_csv(file.path(base_path, "cleanedData/cleanedHouse_2023.csv"))
house_prices_2024 <- read_csv(file.path(base_path, "cleanedData/CleanedHouse_2024.csv"))

house_prices_2022 <- house_prices_2022 %>% mutate(year = 2022)
house_prices_2023 <- house_prices_2023 %>% mutate(year = 2023)
house_prices_2024 <- house_prices_2024 %>% mutate(year = 2024)

all_house_prices <- bind_rows(house_prices_2022, house_prices_2023, house_prices_2024)

all_house_prices <- all_house_prices %>%
  mutate(
    region = case_when(
      county %in% c("Cheshire", "Cheshire East", "Cheshire West And Chester") ~ "Cheshire",
      county == "Cumberland" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>%
  filter(region != "Other")

# --- 1.2 Load Broadband Data ---
broadband_performance <- read_csv(file.path(base_path, "cleanedData/PerformanceInternet.csv"), show_col_types = FALSE)

broadband_performance <- broadband_performance %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(region %in% c("Cheshire", "Cumberland"))

# --- 1.3 Load Population Data ---
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

# --- 1.4 Load Crime Data ---
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

cat("Data loading complete!\n")

# =============================================================================
# SECTION 2: DEFINE MAIN TOWNS FOR ANALYSIS
# =============================================================================

main_towns <- all_house_prices %>%
  filter(year == 2023) %>%
  group_by(town, region) %>%
  summarise(num_sales = n(), .groups = "drop") %>%
  filter(num_sales >= 20) %>%
  pull(town) %>%
  unique()

cat("Main towns identified:", length(main_towns), "\n")

# =============================================================================
# SECTION 3: CALCULATE INDIVIDUAL SCORES (0-10 Scale)
# =============================================================================

# --- 3.1 House Price Score ---
house_price_score <- all_house_prices %>%
  filter(year == 2023, town %in% main_towns) %>%
  group_by(town, region) %>%
  summarise(avg_price = mean(price, na.rm = TRUE),
            median_price = median(price, na.rm = TRUE),
            num_sales = n(),
            .groups = "drop")

max_price <- max(house_price_score$avg_price)
min_price <- min(house_price_score$avg_price)

house_price_score <- house_price_score %>%
  mutate(price_score = round(10 * (1 - (avg_price - min_price) / (max_price - min_price)), 2))

# --- 3.2 Broadband Score ---
town_postcodes <- all_house_prices %>% distinct(town, postcode)

broadband_with_town <- broadband_performance %>%
  inner_join(town_postcodes, by = "postcode")

broadband_score <- broadband_with_town %>%
  filter(town %in% main_towns) %>%
  group_by(town) %>%
  summarise(avg_download = mean(mean_avg_download, na.rm = TRUE),
            max_download = mean(mean_max_download, na.rm = TRUE),
            avg_upload = mean(mean_avg_upload, na.rm = TRUE),
            num_postcodes = n(),
            .groups = "drop")

max_speed <- max(broadband_score$avg_download, na.rm = TRUE)
min_speed <- min(broadband_score$avg_download, na.rm = TRUE)

broadband_score <- broadband_score %>%
  mutate(broadband_score = round(10 * (avg_download - min_speed) / (max_speed - min_speed), 2))

# --- 3.3 Crime Score ---
crime_by_town <- crime_data %>%
  filter(year == 2023) %>%
  group_by(lsoa_district, region) %>%
  summarise(total_crime = n(),
            violent_crime = sum(`Crime type` == "Violence and sexual offences"),
            drug_crime = sum(`Crime type` == "Drugs"),
            theft_crime = sum(`Crime type` %in% c("Other theft","Burglary","Vehicle crime")),
            .groups = "drop") %>%
  rename(town = lsoa_district)

crime_score <- crime_by_town %>% filter(town %in% main_towns)

if(nrow(crime_score) > 0){
  max_crime <- max(crime_score$total_crime)
  min_crime <- min(crime_score$total_crime)
  crime_score <- crime_score %>%
    mutate(crime_score = round(10 * (1 - (total_crime - min_crime)/(max_crime - min_crime)),2))
}

# =============================================================================
# SECTION 4: COMBINE SCORES INTO OVERALL RECOMMENDATION
# =============================================================================

recommendation_data <- house_price_score %>%
  select(town, region, avg_price, price_score) %>%
  left_join(broadband_score %>% select(town, avg_download, broadband_score), by = "town") %>%
  left_join(crime_score %>% select(town, total_crime, crime_score), by = "town") %>%
  group_by(region) %>%
  mutate(
    broadband_score = ifelse(is.na(broadband_score), mean(broadband_score, na.rm=TRUE), broadband_score),
    crime_score = ifelse(is.na(crime_score), mean(crime_score, na.rm=TRUE), crime_score),
    avg_download = ifelse(is.na(avg_download), mean(avg_download, na.rm=TRUE), avg_download),
    total_crime = ifelse(is.na(total_crime), mean(total_crime, na.rm=TRUE), total_crime)
  ) %>%
  ungroup() %>%
  mutate(
    broadband_score = ifelse(is.na(broadband_score),5,broadband_score),
    crime_score = ifelse(is.na(crime_score),5,crime_score),
    overall_score = round(0.4*price_score + 0.3*broadband_score + 0.3*crime_score,2)
  ) %>%
  arrange(desc(overall_score))

# =============================================================================
# SECTION 5: DISPLAY TOP 10 RECOMMENDATIONS
# =============================================================================

cat("\n========================================\n")
cat("TOP 10 RECOMMENDED TOWNS FOR PROPERTY INVESTMENT\n")
cat("========================================\n\n")

top_10 <- recommendation_data %>%
  head(10) %>%
  select(Town = town,
         Region = region,
         `Avg House Price (£)` = avg_price,
         `Price Score` = price_score,
         `Avg Download (Mbit/s)` = avg_download,
         `Broadband Score` = broadband_score,
         `Total Crime (2023)` = total_crime,
         `Crime Score` = crime_score,
         `Overall Score` = overall_score)

print(top_10, n=10, width=Inf)

# =============================================================================
# SECTION 6: TOP 3 RECOMMENDED TOWNS
# =============================================================================

cat("\n========================================\n")
cat("TOP 3 RECOMMENDED TOWNS\n")
cat("========================================\n\n")

top_3 <- recommendation_data %>% head(3)

for(i in 1:nrow(top_3)){
  town_data <- top_3[i,]
  cat(paste0("Rank ", i, ": ", town_data$town, " (", town_data$region,")\n"))
  cat(paste0("   Overall Score: ", town_data$overall_score, "/10\n"))
  cat(paste0("   - House Price Score: ", town_data$price_score, "/10 (Avg: £", format(round(town_data$avg_price), big.mark=","), ")\n"))
  cat(paste0("   - Broadband Score: ", round(town_data$broadband_score,2), "/10 (Avg: ", round(town_data$avg_download,1), " Mbit/s)\n"))
  cat(paste0("   - Crime Score: ", round(town_data$crime_score,2), "/10 (Total: ", round(town_data$total_crime), " incidents)\n"))
  cat("\n")
}

# =============================================================================
# SECTION 7: VISUALIZATIONS
# =============================================================================

# --- 7.1 Top 10 Overall Score ---
png(file.path(base_path,"Graphs/recommendation_top10_overall_scores.png"), width=1200,height=700,res=120)
ggplot(head(recommendation_data,10), aes(x=reorder(town, overall_score), y=overall_score, fill=region)) +
  geom_bar(stat="identity", width=0.7) +
  geom_text(aes(label=overall_score), hjust=-0.2, size=4,fontface="bold") +
  coord_flip() +
  labs(title="Top 10 Recommended Towns for Property Investment",
       subtitle="Scoring: House Price (40%) + Broadband (30%) + Crime Rate (30%)",
       x="Town",y="Overall Score (0-10)",fill="Region") +
  theme_minimal() +
  theme(plot.title=element_text(size=18,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text.y=element_text(size=11)) +
  scale_fill_manual(values=c("Cheshire"="#3498db","Cumberland"="#e74c3c")) +
  scale_y_continuous(limits=c(0,12))
dev.off()

# --- 7.2 Score Breakdown ---
top_10_long <- head(recommendation_data,10) %>%
  select(town, region, price_score, broadband_score, crime_score) %>%
  pivot_longer(cols=c(price_score,broadband_score,crime_score), names_to="score_type", values_to="score") %>%
  mutate(score_type=factor(score_type, levels=c("price_score","broadband_score","crime_score"),
                           labels=c("House Price (40%)","Broadband (30%)","Crime (30%)")))

png(file.path(base_path,"Graphs/recommendation_score_breakdown_stacked.png"), width=1400,height=800,res=120)
ggplot(top_10_long, aes(x=reorder(town,-score), y=score, fill=score_type)) +
  geom_bar(stat="identity", position="stack") +
  coord_flip() +
  labs(title="Score Breakdown for Top 10 Towns",
       subtitle="Individual Component Scores",
       x="Town", y="Score", fill="Score Component") +
  theme_minimal() +
  theme(plot.title=element_text(size=18,face="bold"), legend.position="bottom") +
  scale_fill_manual(values=c("House Price (40%)"="#27ae60","Broadband (30%)"="#3498db","Crime (30%)"="#9b59b6"))
dev.off()

# --- 7.3 Regional Comparison ---
regional_comparison <- recommendation_data %>%
  group_by(region) %>%
  summarise(avg_price_score=round(mean(price_score,na.rm=TRUE),2),
            avg_broadband_score=round(mean(broadband_score,na.rm=TRUE),2),
            avg_crime_score=round(mean(crime_score,na.rm=TRUE),2),
            avg_overall_score=round(mean(overall_score,na.rm=TRUE),2),
            num_towns=n(),
            .groups="drop")

cat("\n========================================\n")
cat("REGIONAL COMPARISON\n")
cat("========================================\n")
print(regional_comparison)

regional_long <- regional_comparison %>%
  select(region, avg_price_score, avg_broadband_score, avg_crime_score) %>%
  pivot_longer(cols=-region, names_to="score_type", values_to="score") %>%
  mutate(score_type=factor(score_type, levels=c("avg_price_score","avg_broadband_score","avg_crime_score"),
                           labels=c("House Price","Broadband","Crime")))

png(file.path(base_path,"Graphs/recommendation_regional_comparison.png"), width=1000,height=600,res=120)
ggplot(regional_long,aes(x=score_type,y=score,fill=region)) +
  geom_bar(stat="identity",position="dodge",width=0.6) +
  geom_text(aes(label=score), position=position_dodge(width=0.6), vjust=-0.5,size=4) +
  labs(title="Regional Score Comparison: Cheshire vs Cumberland",
       subtitle="Average Scores Across All Analyzed Towns",
       x="Score Category",y="Average Score (0-10)",fill="Region") +
  theme_minimal() +
  theme(plot.title=element_text(size=18,face="bold")) +
  scale_fill_manual(values=c("Cheshire"="#3498db","Cumberland"="#e74c3c")) +
  scale_y_continuous(limits=c(0,12))
dev.off()

# --- 7.4 Overall Scores Table ---
png(file.path(base_path,"Graphs/overall_scores_table.png"), width=1400,height=800,res=120)
table_data <- head(recommendation_data,10) %>% mutate(rank=row_number()) %>%
  select(rank, town, region, price_score, broadband_score, crime_score, overall_score)

ggplot(table_data, aes(x=factor(rank))) +
  geom_bar(aes(y=price_score, fill="Price"), stat="identity", position="dodge", width=0.2) +
  geom_bar(aes(y=broadband_score+0.1, fill="Broadband"), stat="identity", position="dodge", width=0.2) +
  geom_bar(aes(y=crime_score+0.2, fill="Crime"), stat="identity", position="dodge", width=0.2) +
  geom_point(aes(y=overall_score, color=region), size=6) +
  geom_text(aes(y=overall_score,label=overall_score), vjust=-1,size=3) +
  geom_text(aes(y=-0.5,label=town), angle=45,hjust=1,size=3) +
  labs(title="Top 10 Towns - Score Components and Overall Ranking",
       x="Rank",y="Score (0-10)",fill="Score Type",color="Region") +
  theme_minimal() +
  theme(plot.title=element_text(size=16,face="bold"), axis.text.x=element_blank()) +
  scale_fill_manual(values=c("Price"="#27ae60","Broadband"="#3498db","Crime"="#9b59b6")) +
  scale_color_manual(values=c("Cheshire"="#2c3e50","Cumberland"="#c0392b"))
dev.off()

# =============================================================================
# SECTION 8: SAVE RESULTS
# =============================================================================

write_csv(recommendation_data, file.path(base_path,"cleanedData/recommendation_full_scores.csv"))
write_csv(head(recommendation_data,10), file.path(base_path,"cleanedData/recommendation_top10.csv"))
write_csv(head(recommendation_data,3), file.path(base_path,"cleanedData/recommendation_top3.csv"))
write_csv(regional_comparison, file.path(base_path,"cleanedData/regional_comparison.csv"))

cat("\n========================================\n")
cat("RECOMMENDATION SYSTEM COMPLETE\n")
cat("========================================\n")
cat("Files saved:\n")
cat("  - recommendation_full_scores.csv\n")
cat("  - recommendation_top10.csv\n")
cat("  - recommendation_top3.csv\n")
cat("  - regional_comparison.csv\n")
cat("Graphs saved in /Graphs folder.\n")
