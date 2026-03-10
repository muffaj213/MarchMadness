# =============================================================================
# run_all.R - Master script to run the full NCAA bracket prediction pipeline
# =============================================================================
# Usage:
#   source("run_all.R")
#
# Or from command line:
#   Rscript run_all.R
#
# Skip prediction: RUN_PREDICTION=FALSE Rscript run_all.R
#
# Steps: 1) Download data, 2) Process, 3) Train, 4) Predict (optional)
# =============================================================================

library(here)

source(here("src", "config.R"))

message("========== NCAA Bracket Prediction Pipeline ==========")

# Step 0 (optional): Create sample data if no Kaggle data exists
required <- c("MTeams.csv", "MNCAATourneyCompactResults.csv", "MNCAATourneySeeds.csv",
              "MNCAATourneySlots.csv")
required_opt <- "MRegularSeasonCompactResults.csv"  # optional; seed-only when missing

paths_raw <- file.path(RAW_DIR, c(required, required_opt))
paths_ext <- file.path(RAW_EXTENDED_DIR, required)
has_raw <- all(file.exists(paths_raw[1:length(required)]))
has_extended <- dir.exists(RAW_EXTENDED_DIR) && all(file.exists(paths_ext))

if (!has_raw && !has_extended) {
  message("\n--- Step 0: Creating sample data (no Kaggle data found) ---")
  source(here::here("src", "00_create_sample_data.R"))
}
if (!has_raw && !has_extended) {
  message("\n--- Step 1: Download data ---")
  source(here::here("src", "01_download_data.R"))
}
# Step 1b (optional): Build extended historical data from nishaanamin
if (file.exists(file.path(NISHAA_DIR, "Tournament Matchups.csv")) && !has_extended) {
  message("\n--- Step 1b: Build historical data from Tournament Matchups ---")
  source(here::here("src", "01b_build_historical_from_nishaa.R"))
}
has_extended <- dir.exists(RAW_EXTENDED_DIR) && all(file.exists(paths_ext))

# Step 1b2 (optional): Build regular-season data from raw_schedules when using Nishaa
# Required for win_pct, SOS, head-to-head, etc. Place schedule CSVs in data/raw_schedules/
schedule_files <- if (dir.exists(SCHEDULES_DIR)) {
  list.files(SCHEDULES_DIR, pattern = "[0-9]{4}-[0-9]{2}_schedule\\.csv$", full.names = TRUE)
} else character()
if (has_extended && length(schedule_files) > 0 && file.exists(here::here("src", "01c_convert_schedules_to_regular.R"))) {
  message("\n--- Step 1b2: Build regular season from raw_schedules ---")
  tryCatch(
    { source(here::here("src", "01c_convert_schedules_to_regular.R")) },
    error = function(e) message("  01c skipped: ", conditionMessage(e))
  )
}
if (!has_raw && !has_extended) {
  stop("Required data files missing. Run 00_create_sample_data.R, 01_download_data.R, or 01b (with raw_nishaa) into data/raw/ or data/raw_extended/")
}

# Step 1c (optional): Fetch KenPom gap (2018-2023) via toRvik
if (file.exists(here::here("scripts", "fetch_kenpom_gap.R"))) {
  message("\n--- Step 1c: Fetch KenPom gap (2018-2023) ---")
  tryCatch(
    { source(here::here("scripts", "fetch_kenpom_gap.R")) },
    error = function(e) message("  KenPom gap fetch skipped: ", conditionMessage(e))
  )
}

# Step 2: Process data
message("\n--- Step 2: Process data ---")
source(here::here("src", "02_process_data.R"))

# Step 3: Train model
message("\n--- Step 3: Train model ---")
source(here::here("src", "03_train_model.R"))

# Step 4: Predict bracket (optional; set RUN_PREDICTION=FALSE to skip)
RUN_PREDICTION <- as.logical(Sys.getenv("RUN_PREDICTION", "TRUE"))
if (RUN_PREDICTION) {
  message("\n--- Step 4: Predict bracket ---")
  source(here::here("src", "04_predict_bracket.R"))
} else {
  message("\n--- Step 4: Skipped (RUN_PREDICTION=FALSE) ---")
}

message("\n========== Pipeline complete ==========")
