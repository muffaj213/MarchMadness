# =============================================================================
# fetch_schedule_cbbdata.R - Fetch regular-season games via cbbdata API
# =============================================================================
# Uses cbd_torvik_game_box(year) for 2025/2026 when toRvik-data GitHub lacks data.
# Requires: cbbdata + account (run scripts/setup_cbbdata.R once).
# Run: Rscript scripts/fetch_schedule_cbbdata.R [YEAR]
# Output: data/raw_schedules/YYYY-(YY+1)_schedule.csv
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

if (!requireNamespace("cbbdata", quietly = TRUE)) {
  stop("Install cbbdata: devtools::install_github('andreweatherman/cbbdata')")
}

main <- function(year = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  if (is.null(year) && length(args) >= 1 && nzchar(args[1])) {
    year <- suppressWarnings(as.integer(args[1]))
  }
  if (is.null(year) && nzchar(Sys.getenv("CBD_FETCH_YEAR"))) {
    year <- as.integer(Sys.getenv("CBD_FETCH_YEAR"))
  }
  if (is.null(year) || is.na(year)) year <- 2025L

  dir.create(SCHEDULES_DIR, showWarnings = FALSE, recursive = TRUE)
  schedule_year <- paste0(year - 1L, "-", sprintf("%02d", year %% 100))
  out_path <- file.path(SCHEDULES_DIR, paste0(schedule_year, "_schedule.csv"))

  message("Logging in to cbbdata...")
  cbbdata::cbd_login()

  message("Fetching cbd_torvik_game_box(", year, ")...")
  x <- tryCatch(
    cbbdata::cbd_torvik_game_box(year = year),
    error = function(e) {
      message("  Error: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(x) || nrow(x) == 0) {
    message("  No data returned.")
    return(invisible(NULL))
  }

  # cbd_torvik_game_box: returns team1, team2, team1_pts, team2_pts (one row per game)
  nm <- tolower(names(x))
  date_col <- names(x)[which(nm %in% c("date", "game_date"))[1]]
  if (length(date_col) == 0 || is.na(date_col)) date_col <- names(x)[1]

  has_team1 <- "team1" %in% nm || "team_1" %in% nm
  has_team <- "team" %in% nm || "team_name" %in% nm

  if (has_team1) {
    t1 <- names(x)[which(nm %in% c("team1", "team_1"))[1]]
    t2 <- names(x)[which(nm %in% c("team2", "team_2"))[1]]
    p1 <- names(x)[which(nm %in% c("team1_pts", "team_1_pts", "pts", "score"))[1]]
    p2 <- names(x)[which(nm %in% c("team2_pts", "team_2_pts", "opp_pts"))[1]]
    if (length(t1) && !is.na(t1) && length(t2) && !is.na(t2) && length(p1) && !is.na(p1)) {
      games <- x %>%
        filter(
          !is.na(.data[[p1]]),
          as.Date(.data[[date_col]]) < as.Date(paste0(year, "-03-18"))
        ) %>%
        mutate(
          Date = as.character(.data[[date_col]]),
          Team = .data[[t1]],
          Opp = .data[[t2]],
          Points_For = as.integer(.data[[p1]]),
          Points_Against = as.integer(if (length(p2) && !is.na(p2)) .data[[p2]] else NA_integer_),
          Win = as.integer(.data[[p1]] > if (length(p2) && !is.na(p2)) .data[[p2]] else 0),
          Type = "N"
        ) %>%
        select(Date, Type, Team, Opp, Points_For, Points_Against, Win)
    }
  }

  if (has_team && !exists("games")) {
    team_col <- names(x)[which(nm %in% c("team", "team_name"))[1]]
    opp_col <- names(x)[which(nm %in% c("opp", "opponent", "opp_name"))[1]]
    pts_col <- names(x)[which(nm %in% c("pts", "score", "points"))[1]]
    game_id <- names(x)[which(nm %in% c("game_id", "gameid"))[1]]
    did_win <- names(x)[which(nm %in% c("result", "did_win", "win", "w"))[1]]
    opp_pts_col <- names(x)[which(nm %in% c("opp_pts", "opp_score"))[1]]
    type_col <- names(x)[which(nm %in% c("type", "game_type"))[1]]

    # Exclude postseason (type "post"); filter to regular season
    type_ok <- if (!is.na(type_col)) !(x[[type_col]] %in% c("post", "postseason")) else TRUE
    x <- x %>% filter(
      as.Date(.data[[date_col]]) < as.Date(paste0(year, "-03-18")),
      type_ok
    )

    if (!is.na(game_id) && !is.na(team_col) && !is.na(opp_col) && !is.na(pts_col)) {
      # cbbdata: one row per team, two per game. Row 1 has team, opp, pts, opp_pts
      games <- x %>%
        group_by(!!sym(game_id)) %>%
        filter(n() == 2L) %>%
        summarise(
          Date = as.character(first(.data[[date_col]])),
          Type = if (!is.na(type_col) && nzchar(type_col)) as.character(first(.data[[type_col]])) else "N",
          Team = first(.data[[team_col]]),
          Opp = first(.data[[opp_col]]),
          Points_For = as.integer(first(.data[[pts_col]])),
          Points_Against = as.integer(if (!is.na(opp_pts_col)) first(.data[[opp_pts_col]]) else last(.data[[pts_col]])),
          Win = as.integer(if (!is.na(did_win) && nzchar(did_win)) (first(.data[[did_win]]) %in% c("W", "w", "1")) else (first(.data[[pts_col]]) > last(.data[[pts_col]])))
        ) %>%
        ungroup() %>%
        select(Date, Type, Team, Opp, Points_For, Points_Against, Win)
    } else {
      games <- x %>%
        mutate(
          Date = as.character(!!sym(date_col)),
          Type = if (!is.na(type_col)) !!sym(type_col) else "N",
          Team = !!sym(team_col),
          Opp = !!sym(opp_col),
          Points_For = as.integer(!!sym(pts_col)),
          Points_Against = NA_integer_,
          Win = if (!is.na(did_win)) as.integer(!!sym(did_win)) else NA_integer_
        ) %>%
        select(Date, Type, Team, Opp, Points_For, Points_Against, Win)
    }
  }

  if (!exists("games")) {
    message("  Could not map cbd_torvik_game_box columns. Columns: ", paste(names(x), collapse = ", "))
    return(invisible(NULL))
  }

  write_csv(games, out_path)
  message("Wrote ", out_path, " (", nrow(games), " games)")
  invisible(out_path)
}

main()
