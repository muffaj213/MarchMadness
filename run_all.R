# =============================================================================
# run_all.R - Master script to run the full NCAA bracket prediction pipeline
# =============================================================================
# Usage:
#   source("run_all.R")
#
# Or from command line:
#   Rscript run_all.R
#
# Steps: 1) Download data, 2) Process, 3) Train, 4) Predict
# =============================================================================

library(here)

message("========== NCAA Bracket Prediction Pipeline ==========")

# Step 0 (optional): Create sample data if no Kaggle data exists
raw_dir <- here::here("data", "raw")
raw_extended_dir <- here::here("data", "raw_extended")
required <- c("MTeams.csv", "MNCAATourneyCompactResults.csv", "MNCAATourneySeeds.csv",
              "MNCAATourneySlots.csv")
required_opt <- "MRegularSeasonCompactResults.csv"  # optional; seed-only when missing

paths_raw <- file.path(raw_dir, c(required, required_opt))
paths_ext <- file.path(raw_extended_dir, required)
has_raw <- all(file.exists(paths_raw[1:length(required)]))
has_extended <- dir.exists(raw_extended_dir) && all(file.exists(paths_ext))

if (!has_raw && !has_extended) {
  message("\n--- Step 0: Creating sample data (no Kaggle data found) ---")
  source(here::here("src", "00_create_sample_data.R"))
}
if (!has_raw && !has_extended) {
  message("\n--- Step 1: Download data ---")
  source(here::here("src", "01_download_data.R"))
}
# Step 1b (optional): Build extended historical data from nishaanamin
if (file.exists(file.path(here::here("data", "raw_nishaa"), "Tournament Matchups.csv")) && !has_extended) {
  message("\n--- Step 1b: Build historical data from Tournament Matchups ---")
  source(here::here("src", "01b_build_historical_from_nishaa.R"))
}
has_extended <- dir.exists(raw_extended_dir) && all(file.exists(paths_ext))
if (!has_raw && !has_extended) {
  stop("Required data files missing. Run 00_create_sample_data.R, 01_download_data.R, or 01b (with raw_nishaa) into data/raw/ or data/raw_extended/")
}

# Step 2: Process data
message("\n--- Step 2: Process data ---")
source(here::here("src", "02_process_data.R"))

# Step 3: Train model
message("\n--- Step 3: Train model ---")
source(here::here("src", "03_train_model.R"))

# Step 4: Predict bracket
message("\n--- Step 4: Predict bracket ---")
source(here::here("src", "04_predict_bracket.R"))

message("\n========== Pipeline complete ==========")
