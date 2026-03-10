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
# Steps: 1) Data prep, 2) Process, 3) Train, 4) Predict (optional)
# =============================================================================

library(here)

source(here("src", "config.R"))

message("========== NCAA Bracket Prediction Pipeline ==========")

# -----------------------------------------------------------------------------
# STEP 1: Data preparation
# -----------------------------------------------------------------------------
required <- c("MTeams.csv", "MNCAATourneyCompactResults.csv", "MNCAATourneySeeds.csv",
              "MNCAATourneySlots.csv")
paths_raw <- file.path(RAW_DIR, required)
paths_ext <- file.path(RAW_EXTENDED_DIR, required)
has_raw <- all(file.exists(paths_raw))
has_extended <- dir.exists(RAW_EXTENDED_DIR) && all(file.exists(paths_ext))

if (!has_raw && !has_extended) {
  message("\n--- Step 1a: Create sample data ---")
  source(here("src", "00_create_sample_data.R"))
  message("\n--- Step 1b: Download data ---")
  source(here("src", "01_download_data.R"))
}
if (file.exists(file.path(NISHAA_DIR, "Tournament Matchups.csv")) && !has_extended) {
  message("\n--- Step 1c: Build historical data from Tournament Matchups ---")
  source(here("src", "01b_build_historical_from_nishaa.R"))
}
# Step 1c2: Fix seed region assignment using danvk/march-madness-data (correct W/X/Y/Z)
if (dir.exists(RAW_EXTENDED_DIR) && file.exists(here("scripts", "fix_seeds_from_danvk.R"))) {
  message("\n--- Step 1c2: Fix seed regions from danvk bracket data ---")
  tryCatch(
    { source(here("scripts", "fix_seeds_from_danvk.R")) },
    error = function(e) message("  fix_seeds skipped: ", conditionMessage(e))
  )
}
has_extended <- dir.exists(RAW_EXTENDED_DIR) && all(file.exists(paths_ext))

schedule_files <- if (dir.exists(SCHEDULES_DIR)) {
  list.files(SCHEDULES_DIR, pattern = "[0-9]{4}-[0-9]{2}_schedule\\.csv$", full.names = TRUE)
} else character()
if (has_extended && length(schedule_files) > 0 && file.exists(here("src", "01c_convert_schedules_to_regular.R"))) {
  message("\n--- Step 1d: Build regular season from raw_schedules ---")
  tryCatch(
    { source(here("src", "01c_convert_schedules_to_regular.R")) },
    error = function(e) message("  01c skipped: ", conditionMessage(e))
  )
}

has_raw <- all(file.exists(paths_raw))
has_extended <- dir.exists(RAW_EXTENDED_DIR) && all(file.exists(paths_ext))
if (!has_raw && !has_extended) {
  stop("Required data missing. Need: ", paste(required, collapse = ", "),
       "\nRun 01_download_data.R or 01b (with raw_nishaa Tournament Matchups.csv)")
}

# Step 1e: Fetch KenPom gap (2018-2023) via toRvik
if (file.exists(here("scripts", "fetch_kenpom_gap.R"))) {
  message("\n--- Step 1e: Fetch KenPom gap (2018-2023) ---")
  tryCatch(
    { source(here("scripts", "fetch_kenpom_gap.R")) },
    error = function(e) message("  KenPom gap fetch skipped: ", conditionMessage(e))
  )
}

# Step 1f: Build team_id_master when raw_historical exists (enables 68-team seeds for 2011-2016)
hist_teams_path <- file.path(RAW_HIST_DIR, "Teams.csv")
has_mteams <- file.exists(file.path(RAW_EXTENDED_DIR, "MTeams.csv")) || file.exists(file.path(RAW_DIR, "MTeams.csv"))
if (file.exists(hist_teams_path) && has_mteams && !file.exists(TEAM_ID_MASTER_PATH)) {
  message("\n--- Step 1f: Build team_id_master (for 68-team seeds 2011-2016) ---")
  if (!dir.exists(PROC_DIR)) dir.create(PROC_DIR, recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    { source(here("scripts", "build_canonical_team_master.R")) },
    error = function(e) message("  team_id_master build skipped: ", conditionMessage(e))
  )
}

# -----------------------------------------------------------------------------
# STEP 2: Process data
# -----------------------------------------------------------------------------
message("\n--- Step 2: Process data ---")
source(here("src", "02_process_data.R"))

# -----------------------------------------------------------------------------
# STEP 3: Train model
# -----------------------------------------------------------------------------
message("\n--- Step 3: Train model ---")
source(here("src", "03_train_model.R"))

# -----------------------------------------------------------------------------
# STEP 4: Predict bracket (optional; RUN_PREDICTION=FALSE to skip)
# -----------------------------------------------------------------------------
RUN_PREDICTION <- as.logical(Sys.getenv("RUN_PREDICTION", "TRUE"))
if (RUN_PREDICTION) {
  message("\n--- Step 4: Predict bracket ---")
  source(here("src", "04_predict_bracket.R"))
} else {
  message("\n--- Step 4: Skipped (RUN_PREDICTION=FALSE) ---")
}

message("\n========== Pipeline complete ==========")
