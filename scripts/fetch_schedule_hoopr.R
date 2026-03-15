# =============================================================================
# fetch_schedule_hoopr.R - Fetch regular-season games via hoopR
# =============================================================================
# Uses load_mbb_schedule(seasons = YEAR) from hoopR as a fallback source.
# Run: Rscript scripts/fetch_schedule_hoopr.R [YEAR]
# Output: data/raw_schedules/YYYY-(YY+1)_schedule.csv
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

if (!requireNamespace("hoopR", quietly = TRUE)) {
  stop("Install hoopR: install.packages('hoopR')")
}

main <- function(year = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  if (is.null(year) && length(args) >= 1 && nzchar(args[1])) {
    year <- suppressWarnings(as.integer(args[1]))
  }
  if (is.null(year) && nzchar(Sys.getenv("HOOPR_FETCH_YEAR"))) {
    year <- as.integer(Sys.getenv("HOOPR_FETCH_YEAR"))
  }
  if (is.null(year) || is.na(year)) year <- 2026L

  dir.create(SCHEDULES_DIR, showWarnings = FALSE, recursive = TRUE)
  schedule_year <- paste0(year - 1L, "-", sprintf("%02d", year %% 100))
  out_path <- file.path(SCHEDULES_DIR, paste0(schedule_year, "_schedule.csv"))

  message("Fetching hoopR schedule for season ", year, "...")
  x <- tryCatch(
    hoopR::load_mbb_schedule(seasons = year),
    error = function(e) {
      message("  Error: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(x) || nrow(x) == 0) {
    message("  No data returned.")
    return(invisible(NULL))
  }

  ncaa_start <- as.Date(paste0(year, "-03-18"))

  games <- x %>%
    mutate(
      game_date = as.Date(game_date),
      home_score_num = suppressWarnings(as.integer(home_score)),
      away_score_num = suppressWarnings(as.integer(away_score)),
      is_completed = as.logical(status_type_completed)
    ) %>%
    filter(
      !is.na(game_date),
      game_date < ncaa_start,
      is_completed,
      !is.na(home_score_num),
      !is.na(away_score_num),
      home_location != "TBD",
      away_location != "TBD"
    ) %>%
    transmute(
      Date = as.character(game_date),
      Type = "N",
      Team = home_location,
      Opp = away_location,
      Points_For = home_score_num,
      Points_Against = away_score_num,
      Win = as.integer(home_score_num > away_score_num)
    ) %>%
    distinct()

  if (nrow(games) == 0) {
    message("  No completed regular-season rows after filtering.")
    return(invisible(NULL))
  }

  write_csv(games, out_path)
  message("Wrote ", out_path, " (", nrow(games), " games)")
  invisible(out_path)
}

main()
