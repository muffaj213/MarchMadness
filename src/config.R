# =============================================================================
# config.R - Central path configuration for NCAA bracket prediction pipeline
# =============================================================================
# Source this at the start of scripts that need path constants.
# Usage: source(here::here("src", "config.R"))
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Install the 'here' package: install.packages('here')")
}

# Core data directories (relative to project root)
RAW_DIR <- here::here("data", "raw")
RAW_EXTENDED_DIR <- here::here("data", "raw_extended")
PROC_DIR <- here::here("data", "processed")
MODELS_DIR <- here::here("models")
OUTPUT_DIR <- here::here("output")
BRACKET_DIR <- here::here("data", "bracket")

# Secondary data sources
RAW_HIST_DIR <- here::here("data", "raw_historical")
NISHAA_DIR <- here::here("data", "raw_nishaa")
KENPOM_DIR <- here::here("data", "raw_kenpom")
SCHEDULES_DIR <- here::here("data", "raw_schedules")
RAW_ATOZIYE_DIR <- here::here("data", "raw_atoziye")

# File paths (for scripts that need specific files)
TEAM_ID_MASTER_PATH <- file.path(PROC_DIR, "team_id_master.csv")
