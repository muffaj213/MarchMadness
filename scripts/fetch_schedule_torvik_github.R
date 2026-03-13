# =============================================================================
# fetch_schedule_torvik_github.R - Fetch regular-season games from toRvik-data GitHub
# =============================================================================
# Downloads game_box_YEAR.csv from andreweatherman/toRvik-data (no API, no toRvik pkg).
# Works for years 2008-2023 (as of 2025, repo has not been updated with 2024+).
# Run: Rscript scripts/fetch_schedule_torvik_github.R [YEAR]
# Example: Rscript scripts/fetch_schedule_torvik_github.R 2023
# Output: data/raw_schedules/YYYY-(YY+1)_schedule.csv (e.g. 2022-23 for year=2023)
# See docs/DATA_SOURCES_2025_2026.md for 2024+ alternatives.
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
BASE_URL <- "https://raw.githubusercontent.com/andreweatherman/toRvik-data/main/game_box"

# game_box format: date, type, did_win, game_id, team, conf, opp, opp_conf, ..., pts
# Each game appears twice (one row per team). type: nc, conf, conf_t, nond1, post

main <- function(year = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  if (is.null(year) && length(args) >= 1 && nzchar(args[1])) {
    year <- suppressWarnings(as.integer(args[1]))
  }
  if (is.null(year) || is.na(year)) year <- 2025L
  dir.create(SCHEDULES_DIR, showWarnings = FALSE, recursive = TRUE)

  schedule_year <- paste0(year - 1L, "-", sprintf("%02d", year %% 100))
  out_path <- file.path(SCHEDULES_DIR, paste0(schedule_year, "_schedule.csv"))

  url <- paste0(BASE_URL, "/game_box_", year, ".csv")
  message("Trying ", url, " ...")

  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch(
    download.file(url, tmp, quiet = TRUE),
    error = function(e) {
      message("  Download failed: ", conditionMessage(e))
      FALSE
    }
  )
  if (ok != 0 || !file.exists(tmp) || file.info(tmp)$size == 0) {
    message("  File not found or empty. toRvik-data has game_box through 2023.")
    message("  See docs/DATA_SOURCES_2025_2026.md for 2024+ options.")
    if (file.exists(tmp)) unlink(tmp)
    return(invisible(NULL))
  }

  x <- read_csv(tmp, show_col_types = FALSE)
  unlink(tmp)

  # Exclude postseason
  x <- x %>% filter(type %in% c("nc", "conf", "conf_t", "nond1"))

  # Each game_id has 2 rows. Create one row per game with Team, Opp, Points_For, Points_Against, Win
  games <- x %>%
    group_by(game_id) %>%
    filter(n() == 2L) %>%
    summarise(
      Date = as.character(first(date)),
      Type = first(type),
      Team = team[1],
      Opp = opp[1],
      Points_For = as.integer(pts[1]),
      Points_Against = as.integer(pts[2]),
      Win = as.integer(did_win[1])
    ) %>%
    ungroup() %>%
    select(Date, Type, Team, Opp, Points_For, Points_Against, Win)

  write_csv(games, out_path)
  message("Wrote ", out_path, " (", nrow(games), " games)")
  invisible(out_path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
main()
