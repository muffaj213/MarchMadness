# =============================================================================
# test_pipeline.R - Basic sanity checks for the NCAA prediction pipeline
# =============================================================================
# Run: source(here::here("tests", "test_pipeline.R"))
# =============================================================================

library(here)

message("Testing NCAA Bracket Prediction Pipeline...")

# Test 1: Required packages
required_pkgs <- c("dplyr", "readr", "tidymodels", "here")
for (pkg in required_pkgs) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    message("  OK: ", pkg)
  } else {
    stop("  MISSING: ", pkg, " - run install.R")
  }
}

# Test 2: Source utilities (no execution, just load)
source(here("src", "utils", "feature_engineering.R"))
source(here("src", "utils", "bracket_logic.R"))
message("  OK: Utility scripts load")

# Test 3: parse_seed_number
stopifnot(parse_seed_number("W01") == 1)
stopifnot(parse_seed_number("X16") == 16)
message("  OK: parse_seed_number works")

# Test 4: Check data exists (if pipeline has been run)
proc_dir <- here("data", "processed")
if (file.exists(file.path(proc_dir, "matchup_data.csv"))) {
  data <- readr::read_csv(file.path(proc_dir, "matchup_data.csv"), show_col_types = FALSE)
  message("  OK: Processed data has ", nrow(data), " games")
} else {
  message("  SKIP: Run 02_process_data.R first to test with real data")
}

message("All tests passed.")
