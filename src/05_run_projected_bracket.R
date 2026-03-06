# =============================================================================
# 05_run_projected_bracket.R - Convert Excel projection to seeds, run model
# =============================================================================
# Reads bracket_projection_YYYY.xlsx, maps TeamName -> TeamID, writes
# seeds_projected_YYYY.csv, then runs prediction with that seeds file.
# Run: source("src/05_run_projected_bracket.R") or Rscript src/05_run_projected_bracket.R
# =============================================================================

library(here)
library(readxl)
library(readr)
library(dplyr)

PROC_DIR <- here("data", "processed")
BRACKET_DIR <- here("data", "bracket")

# Configurable year for projected bracket
PROJECTED_YEAR <- 2025L

main <- function(year = PROJECTED_YEAR) {
  excel_path <- file.path(BRACKET_DIR, paste0("bracket_projection_", year, ".xlsx"))
  if (!file.exists(excel_path)) {
    stop("Projection file not found: ", excel_path,
         "\nCopy bracket_projection_TEMPLATE.xlsx to bracket_projection_", year, ".xlsx and fill in teams.")
  }

  message("Reading projection from ", excel_path)
  proj <- read_excel(excel_path, sheet = "Seeds")
  if (!"Seed" %in% names(proj) || !"TeamName" %in% names(proj)) {
    stop("Excel must have 'Seeds' sheet with columns Seed and TeamName.")
  }

  teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)
  source(here("src", "utils", "bracket_utils.R"), local = TRUE)

  message("Resolving team names to TeamIDs...")
  proj$TeamID <- resolve_team_names_to_ids(proj$TeamName, teams)
  missing <- which(is.na(proj$TeamID) & trimws(as.character(proj$TeamName)) != "")
  if (length(missing) > 0) {
    stop("Could not resolve team(s): ", paste(proj$TeamName[missing], collapse = ", "),
         "\nUse exact names from data/processed/teams.csv or see docs/PROJECTED_BRACKET_GUIDE.md")
  }

  seeds_df <- proj %>%
    filter(!is.na(TeamID), trimws(as.character(TeamName)) != "") %>%
    mutate(Season = year) %>%
    select(Season, Seed, TeamID)

  if (nrow(seeds_df) == 0) {
    stop("No teams resolved. Fill in TeamName for all 64 seeds in the Excel template.")
  }

  seeds_path <- file.path(BRACKET_DIR, paste0("seeds_projected_", year, ".csv"))
  write_csv(seeds_df, seeds_path)
  message("Wrote ", seeds_path, " (", nrow(seeds_df), " teams)")

  options(bracket.skip_main = TRUE)
  source(here("src", "04_predict_bracket.R"), local = FALSE)
  options(bracket.skip_main = NULL)

  main(season = year, seeds_file = seeds_path, use_projected_output = TRUE)
}

main(year = PROJECTED_YEAR)
