
library(tidyverse)
library(lubridate)

# Set working directory
setwd("C:/Users/user/Desktop/DS")

# Define the base path for crime data
crime_base_path <- "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota/ObtainData/crimerate"
# Get all month folders
month_folders <- list.dirs(crime_base_path, recursive = FALSE, full.names = TRUE)

# Function to read all crime files from a folder
read_crime_folder <- function(folder_path) {
  files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  # Read all files in the folder and combine
  crime_data <- map_dfr(files, function(file) {
    df <- read_csv(file, show_col_types = FALSE)
    # Add source file info
    df$source_file <- basename(file)
    return(df)
  })
  return(crime_data)
}

# Read all crime data from all folders
all_crime_data <- map_dfr(month_folders, read_crime_folder)

# Display initial summary
cat("Initial crime data records:", nrow(all_crime_data), "\n")
cat("Columns:", paste(names(all_crime_data), collapse = ", "), "\n\n")

# Clean and standardize the crime data
crime_cleaned <- all_crime_data %>%
  # Rename columns for consistency
  rename(
    crime_id = `Crime ID`,
    month = Month,
    reported_by = `Reported by`,
    falls_within = `Falls within`,
    longitude = Longitude,
    latitude = Latitude,
    location = Location,
    lsoa_code = `LSOA code`,
    lsoa_name = `LSOA name`,
    crime_type = `Crime type`,
    outcome = `Last outcome category`
  ) %>%
  # Remove Context column as it's mostly empty
  select(-Context, -source_file) %>%
  # Parse the month column
  mutate(
    month = ymd(paste0(month, "-01"))
  )

# Extract county information from LSOA name
crime_cleaned <- crime_cleaned %>%
  mutate(
    county = case_when(
      str_detect(lsoa_name, "^Cheshire") ~ "Cheshire",
      str_detect(lsoa_name, "^Allerdale|^Barrow|^Carlisle|^Copeland|^Eden|^South Lakeland") ~ "Cumberland",
      str_detect(falls_within, "Cumbria") ~ "Cumberland",
      str_detect(falls_within, "Cheshire") ~ "Cheshire",
      TRUE ~ "Unknown"
    )
  )

# Extract town/district from LSOA name
crime_cleaned <- crime_cleaned %>%
  mutate(
    # Extract the district name from LSOA (e.g., "Cheshire East 001A" -> "Cheshire East")
    district = str_extract(lsoa_name, "^[A-Za-z ]+") %>% str_trim()
  )

# Remove rows with missing essential data
crime_cleaned <- crime_cleaned %>%
  filter(
    !is.na(crime_type),
    !is.na(lsoa_code),
    !is.na(month)
  )

# Display cleaned data summary
cat("\nCleaned crime data records:", nrow(crime_cleaned), "\n")
cat("Date range:", as.character(min(crime_cleaned$month)), "to",
    as.character(max(crime_cleaned$month)), "\n")
cat("Counties:", paste(unique(crime_cleaned$county), collapse = ", "), "\n")
cat("Crime types:", paste(unique(crime_cleaned$crime_type), collapse = ", "), "\n\n")

# Summary statistics by county and crime type
crime_summary <- crime_cleaned %>%
  group_by(county, crime_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(county, desc(count))

print("Crime summary by county and type:")
print(crime_summary)

# Save cleaned crime data
write_csv(
  crime_cleaned,
  "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota/CleanedData/cleaned_crime_data.csv"
)

cat("\nCleaned crime data saved to cleanedData/cleaned_crime_data.csv\n")

# Create aggregated crime data by LSOA for analysis
crime_by_lsoa <- crime_cleaned %>%
  group_by(lsoa_code, lsoa_name, county, district) %>%
  summarise(
    total_crimes = n(),
    violent_crimes = sum(crime_type == "Violence and sexual offences"),
    burglary = sum(crime_type == "Burglary"),
    robbery = sum(crime_type == "Robbery"),
    vehicle_crime = sum(crime_type == "Vehicle crime"),
    theft = sum(crime_type == "Other theft"),
    drugs = sum(crime_type == "Drugs"),
    anti_social = sum(crime_type == "Anti-social behaviour"),
    criminal_damage = sum(crime_type == "Criminal damage and arson"),
    public_order = sum(crime_type == "Public order"),
    .groups = "drop"
  )

write_csv(
  crime_by_lsoa,
  "C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota/CleanedData/crime_by_lsoa.csv"
)

cat("Aggregated crime data by LSOA saved to cleanedData/crime_by_lsoa.csv\n")
