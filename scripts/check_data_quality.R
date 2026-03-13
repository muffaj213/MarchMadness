# =============================================================================
# check_data_quality.R - Verify data completeness for 2026 prediction
# =============================================================================
# Run before training to ensure KenPom, regular-season, and tournament data
# are complete. Reports missing features and coverage gaps.
# Usage: Rscript scripts/check_data_quality.R [SEASON]
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

main <- function(season = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  if (is.null(season) && length(args) >= 1 && nzchar(args[1])) {
    season <- suppressWarnings(as.integer(args[1]))
  }
  if (is.null(season) || is.na(season)) season <- 2026L

  message("=== Data Quality Check for Season ", season, " ===\n")

  issues <- character()
  warnings <- character()

  # 1. KenPom/Barttorvik coverage
  kp_path <- file.path(NISHAA_DIR, "KenPom Barttorvik.csv")
  if (!file.exists(kp_path)) {
    kp_path <- file.path(RAW_DIR, "kenpom.csv")
  }
  if (file.exists(kp_path)) {
    kp <- read_csv(kp_path, show_col_types = FALSE)
    season_col <- intersect(names(kp), c("Season", "Year", "YEAR"))[1]
    kp_seasons <- if (!is.na(season_col)) unique(kp[[season_col]]) else integer()
    if (length(kp_seasons) == 0 || !season %in% kp_seasons) {
      issues <- c(issues, paste0("KenPom/Barttorvik: No data for ", season, ". Update raw_nishaa/KenPom Barttorvik.csv"))
    } else {
      message("[OK] KenPom/Barttorvik has data for ", season)
    }
  } else {
    issues <- c(issues, "KenPom: No KenPom Barttorvik.csv or kenpom.csv found")
  }

  # 2. Regular-season games (MRegularSeasonCompactResults)
  reg_path <- file.path(RAW_EXTENDED_DIR, "MRegularSeasonCompactResults.csv")
  if (!file.exists(reg_path)) reg_path <- file.path(RAW_DIR, "MRegularSeasonCompactResults.csv")
  if (file.exists(reg_path)) {
    reg <- read_csv(reg_path, show_col_types = FALSE)
    reg_seasons <- unique(reg$Season)
    if (!season %in% reg_seasons) {
      warnings <- c(warnings, paste0("Regular season: No games for ", season, ". Run fetch_schedule_cbbdata.R ", season, " or add schedule CSV"))
    } else {
      n_games <- sum(reg$Season == season)
      message("[OK] Regular season: ", n_games, " games for ", season)
    }
  } else {
    issues <- c(issues, "Regular season: MRegularSeasonCompactResults.csv not found")
  }

  # 3. Tournament results (for training; prediction uses seeds only)
  tourney_path <- file.path(RAW_EXTENDED_DIR, "MNCAATourneyCompactResults.csv")
  if (!file.exists(tourney_path)) tourney_path <- file.path(RAW_DIR, "MNCAATourneyCompactResults.csv")
  if (file.exists(tourney_path)) {
    tourney <- read_csv(tourney_path, show_col_types = FALSE)
    tourney_seasons <- unique(tourney$Season)
    if (!(season - 1L) %in% tourney_seasons) {
      warnings <- c(warnings, paste0("Tournament: No results for ", season - 1L, " (needed for historical features). Update Tournament Matchups."))
    }
    message("[OK] Tournament results: seasons ", min(tourney_seasons), "-", max(tourney_seasons))
  } else {
    issues <- c(issues, "Tournament: MNCAATourneyCompactResults.csv not found")
  }

  # 4. Seeds for prediction season
  seeds_path <- file.path(PROC_DIR, "tourney_seeds.csv")
  if (!file.exists(seeds_path)) {
    seeds_path <- file.path(RAW_EXTENDED_DIR, "MNCAATourneySeeds.csv")
  }
  if (!file.exists(seeds_path)) seeds_path <- file.path(RAW_DIR, "MNCAATourneySeeds.csv")
  if (file.exists(seeds_path)) {
    seeds <- read_csv(seeds_path, show_col_types = FALSE)
    seed_seasons <- unique(seeds$Season)
    if (!season %in% seed_seasons) {
      issues <- c(issues, paste0("Seeds: No seeds for ", season, ". Create seeds_68team_", season, ".csv from Selection Sunday bracket"))
    } else {
      n_teams <- sum(seeds$Season == season)
      message("[OK] Seeds: ", n_teams, " teams for ", season)
    }
  } else {
    issues <- c(issues, "Seeds: No tourney_seeds.csv or MNCAATourneySeeds.csv found")
  }

  # 5. Processed matchup_data: missing features
  matchup_path <- file.path(PROC_DIR, "matchup_data.csv")
  if (file.exists(matchup_path)) {
    md <- read_csv(matchup_path, show_col_types = FALSE)
    key_cols <- c("adjem_diff", "tourney_winpct_diff", "winpct_diff")
    missing <- key_cols[!key_cols %in% names(md)]
    if (length(missing) > 0) {
      warnings <- c(warnings, paste0("Matchup data: missing columns ", paste(missing, collapse = ", ")))
    }
    # Check KenPom coverage in latest season
    latest <- max(md$Season, na.rm = TRUE)
    md_latest <- md %>% filter(Season == latest)
    na_adjem <- sum(is.na(md_latest$adjem_diff) | md_latest$adjem_diff == 0)
    if (na_adjem > nrow(md_latest) * 0.5) {
      warnings <- c(warnings, paste0("Matchup data: ", na_adjem, "/", nrow(md_latest), " rows have no KenPom (adjem_diff=0) for ", latest))
    }
    message("[OK] Matchup data: ", nrow(md), " rows, seasons ", min(md$Season), "-", max(md$Season))
  } else {
    warnings <- c(warnings, "Matchup data: Run 02_process_data.R first")
  }

  # Summary
  message("\n--- Summary ---")
  if (length(issues) > 0) {
    message("CRITICAL (", length(issues), "):")
    for (i in issues) message("  - ", i)
  }
  if (length(warnings) > 0) {
    message("WARNINGS (", length(warnings), "):")
    for (w in warnings) message("  - ", w)
  }
  if (length(issues) == 0 && length(warnings) == 0) {
    message("All checks passed. Ready for 2026 prediction.")
  } else if (length(issues) == 0) {
    message("No critical issues. Review warnings before predicting.")
  } else {
    message("Fix critical issues before training/predicting.")
  }

  invisible(list(issues = issues, warnings = warnings))
}

main()
