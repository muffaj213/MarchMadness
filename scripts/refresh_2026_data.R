# =============================================================================
# refresh_2026_data.R - Fetch and prepare data for 2026 prediction
# =============================================================================
# 1. Fetch regular-season schedule via cbbdata (if available)
# 2. Convert to MRegularSeasonCompactResults (01c)
# 3. Run data quality check
# Usage: Rscript scripts/refresh_2026_data.R
# =============================================================================

library(here)

source(here("src", "config.R"))

main <- function() {
  year <- 2026L
  message("=== Refreshing data for ", year, " prediction ===\n")

  # Step 1: Fetch schedule via cbbdata
  if (requireNamespace("cbbdata", quietly = TRUE)) {
    message("1. Fetching schedule via cbbdata...")
    exit <- system2("Rscript", c(
      here::here("scripts", "fetch_schedule_cbbdata.R"),
      as.character(year)
    ))
    if (exit != 0) {
      message("  cbbdata fetch failed. Add data/raw_schedules/2025-26_schedule.csv manually.")
    }
  } else {
    message("1. cbbdata not installed. Add data/raw_schedules/2025-26_schedule.csv manually.")
    message("   Or: devtools::install_github('andreweatherman/cbbdata')")
  }

  # Step 2: Convert schedules to MRegularSeasonCompactResults
  message("\n2. Converting schedules to MRegularSeasonCompactResults...")
  schedule_path <- file.path(SCHEDULES_DIR, "2025-26_schedule.csv")
  if (file.exists(schedule_path)) {
    exit <- system2("Rscript", here::here("src", "01c_convert_schedules_to_regular.R"))
    if (exit == 0) message("  Done.") else message("  Conversion failed.")
  } else {
    message("  No 2025-26_schedule.csv. Skipping conversion.")
  }

  # Step 3: Data quality check
  message("\n3. Running data quality check...")
  system2("Rscript", c(
    here::here("scripts", "check_data_quality.R"),
    as.character(year)
  ))

  message("\n=== Done. Run 02_process_data.R then 03_train_model.R to retrain. ===")
}

main()
