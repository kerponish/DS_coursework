# =============================================================================
# MASTER ANALYSIS SCRIPT
# Data Science Coursework - Cheshire and Cumberland Property Investment Analysis
# =============================================================================
#
# This script coordinates the execution of all analysis components:
# 1. Data Cleaning (House Prices, Crime, Broadband, Population)
# 2. Exploratory Data Analysis (EDA) with Visualizations
# 3. Linear Modeling Analysis
# 4. Recommendation System
#

# =============================================================================

# working directory
setwd("C:/Users/user/Desktop/DS/Coursework_DS_KripanSapkota")

cat("========================================\n")
cat("COURSEWORK ANALYSIS - MASTER SCRIPT\n")
cat("========================================\n\n")

# --- Step 1: Data Cleaning ---
cat("STEP 1: Running Data Cleaning Scripts...\n")
cat("----------------------------------------\n")



cat("Data cleaning scripts referenced (run individually if needed)\n\n")

# --- Step 2: Exploratory Data Analysis ---
cat("STEP 2: Running EDA and Visualization...\n")
cat("----------------------------------------\n")
source("R_file/EDA_Visualization_Analysis.R")
cat("EDA complete!\n\n")

# --- Step 3: Linear Modeling ---
cat("STEP 3: Running Linear Modeling Analysis...\n")
cat("------------------------------------------\n")
source("R_file/LinearModeling.R")
cat("Linear Modeling complete!\n\n")

# --- Step 4: Recommendation System ---
cat("STEP 4: Running Recommendation System...\n")
cat("----------------------------------------\n")
source("R_file/RecommendationSystem.R")
cat("Recommendation System complete!\n\n")

cat("========================================\n")
cat("ALL ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("\nOutput files saved to:\n")
cat("  - cleanedData/ (processed datasets)\n")
cat("  - Graphs/ (all visualizations)\n")
cat("\nVisualization Summary:\n")
cat("  House Prices:\n")
cat("    - boxplot_house_prices_2023.png\n")
cat("    - barchart_avg_house_prices_2022.png\n")
cat("    - linegraph_house_prices_2022_2024.png\n")
cat("  Broadband:\n")
cat("    - boxplot_broadband_speeds.png\n")
cat("    - stacked_barchart_cheshire_broadband.png\n")
cat("    - stacked_barchart_cumberland_broadband.png\n")
cat("  Crime:\n")
cat("    - boxplot_drug_offenses.png\n")
cat("    - radarchart_vehicle_crime_2023.png\n")
cat("    - piechart_robbery_2023.png\n")
cat("    - linegraph_drug_offenses_per_10k.png\n")
cat("  Linear Models:\n")
cat("    - linear_model_house_price_vs_download_speed.png\n")
cat("    - linear_model_house_price_vs_drug_offense_2022.png\n")
cat("    - linear_model_download_speed_vs_drug_offense.png\n")
cat("    - linear_models_combined_summary.png\n")
cat("  Recommendation:\n")
cat("    - recommendation_top10_overall_scores.png\n")
cat("    - recommendation_score_breakdown_stacked.png\n")
cat("    - recommendation_regional_comparison.png\n")
cat("    - overall_scores_table.png\n")
cat("  Population:\n")
cat("    - population_by_region.png\n")
cat("    - population_by_postcode_area.png\n")
cat("    - population_top15_districts.png\n")
