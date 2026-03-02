# =============================================================================
# 01_download_data.R - Download NCAA Basketball data from Kaggle
# =============================================================================
# Run this script to download the Kaggle NCAA dataset into data/raw/
#
# Prerequisites:
#   1. Kaggle account: https://www.kaggle.com/
#   2. Accept terms for the dataset (visit dataset page once)
#   3. Kaggle API credentials: kaggle.json in ~/.kaggle/ (or %USERPROFILE%\.kaggle\ on Windows)
#
# Alternative: Manual download from
#   https://www.kaggle.com/datasets/ncaa/ncaa-basketball
#   or https://www.kaggle.com/c/march-madness-analytics-2020/data (Stage 2)
#   Extract CSVs into data/raw/
# =============================================================================

library(here)

# Configuration
RAW_DIR <- here("data", "raw")
REQUIRED_FILES <- c(
  "MTeams.csv",
  "MSeasons.csv",
  "MNCAATourneyCompactResults.csv",
  "MNCAATourneySeeds.csv",
  "MNCAATourneySlots.csv",
  "MRegularSeasonCompactResults.csv"
)

#' Download NCAA data using Kaggle API (requires kaggle CLI)
download_via_kaggle_cli <- function() {
  # Common NCAA dataset identifiers on Kaggle
  datasets <- c(
    "ncaa/ncaa-basketball",
    "ncaa/march-madness-analytics-2020"
  )

  for (ds in datasets) {
    message("Attempting to download: ", ds)
    result <- tryCatch(
      {
        # Kaggle CLI: kaggle datasets download -d <dataset> -p <path>
        system2("kaggle", args = c(
          "datasets", "download", "-d", ds,
          "-p", RAW_DIR,
          "--unzip"
        ), stdout = TRUE, stderr = TRUE)
      },
      error = function(e) {
        message("Error: ", e$message)
        NULL
      }
    )
    if (!is.null(result) && length(result) > 0) {
      # Check if we got the files
      present <- file.exists(file.path(RAW_DIR, REQUIRED_FILES))
      if (all(present)) {
        message("Download successful.")
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Check if required files exist in data/raw
check_data_exists <- function() {
  paths <- file.path(RAW_DIR, REQUIRED_FILES)
  present <- file.exists(paths)
  if (all(present)) {
    message("All required files already present in ", RAW_DIR)
    return(TRUE)
  }
  missing <- REQUIRED_FILES[!present]
  message("Missing files: ", paste(missing, collapse = ", "))
  FALSE
}

# Main execution
main <- function() {
  if (!dir.exists(RAW_DIR)) {
    dir.create(RAW_DIR, recursive = TRUE)
  }

  if (check_data_exists()) {
    return(invisible(TRUE))
  }

  message("Attempting download via Kaggle CLI...")
  if (download_via_kaggle_cli()) {
    return(invisible(TRUE))
  }

  message("\n",
    "========================================\n",
    "Manual download required.\n",
    "========================================\n",
    "1. Visit: https://www.kaggle.com/datasets/ncaa/ncaa-basketball\n",
    "   (or search 'NCAA basketball' / 'March Madness' on Kaggle)\n",
    "2. Click 'Download' after accepting terms\n",
    "3. Extract all CSV files into: ", normalizePath(RAW_DIR, mustWork = FALSE), "\n",
    "4. Re-run this script to verify\n",
    "========================================\n"
  )
  invisible(FALSE)
}

main()
