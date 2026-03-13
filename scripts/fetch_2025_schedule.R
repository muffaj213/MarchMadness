# =============================================================================
# fetch_2025_schedule.R - Fetch 2025 regular-season games via toRvik
# =============================================================================
# Uses bart_game_box(year=2025) to get all games with scores in one call.
# Writes data/raw_schedules/2024-25_schedule.csv for 01c_convert_schedules_to_regular.R.
# Run: Rscript scripts/fetch_2025_schedule.R
# Requires: toRvik (install.packages("toRvik") or devtools::install_github("andreweatherman/toRvik"))
# =============================================================================

library(here)
library(readr)
library(dplyr)

if (!requireNamespace("toRvik", quietly = TRUE)) {
  stop("Install toRvik: install.packages('toRvik') or devtools::install_github('andreweatherman/toRvik')")
}

source(here("src", "config.R"))
OUT_PATH <- file.path(SCHEDULES_DIR, "2024-25_schedule.csv")

main <- function() {
  dir.create(SCHEDULES_DIR, showWarnings = FALSE, recursive = TRUE)

  message("Fetching 2025 game box scores via bart_game_box...")
  x <- tryCatch(
    toRvik::bart_game_box(year = 2025),
    error = function(e) {
      message("  Error: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(x) || nrow(x) == 0) {
    if (file.exists(OUT_PATH)) {
      message("Using existing ", OUT_PATH, " (API fetch failed)")
      return(invisible(OUT_PATH))
    }
    stop(
      "Could not fetch bart_game_box(2025). ",
      "The toRvik/cbbstat API may not have 2025 data yet. ",
      "See data/raw_schedules/README.md for manual steps."
    )
  }

  # bart_game_box: date, team1, team2, team1_pts, team2_pts
  # Exclude NCAA tournament (starts ~March 18-20). Keep regular season + conf tournaments.
  NCAA_START <- as.Date("2025-03-18")
  games <- x %>%
    filter(
      !is.na(team1_pts), !is.na(team2_pts),
      as.Date(date) < NCAA_START
    ) %>%
    mutate(
      Date = as.character(date),
      Team = team1,
      Opp = team2,
      Points_For = as.integer(team1_pts),
      Points_Against = as.integer(team2_pts),
      Win = as.integer(team1_pts > team2_pts),
      Type = "N"  # generic; 01c doesn't require type for conversion
    ) %>%
    select(Date, Type, Team, Opp, Points_For, Points_Against, Win)

  write_csv(games, OUT_PATH)
  message("Wrote ", OUT_PATH, " (", nrow(games), " games)")
}

main()
