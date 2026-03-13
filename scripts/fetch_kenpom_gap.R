# =============================================================================
# fetch_kenpom_gap.R - Fetch KenPom/Barttorvik data for 2018-2023 gap
# =============================================================================
# Uses toRvik::bart_ratings() to pull T-Rank ratings (adj_o, adj_d, adj_t).
# Fallback: download from toRvik-data GitHub when API fails.
# Saves to data/raw_kenpom/kenpom_gap_2018_2023.csv for load_kenpom_stats().
# For 2024+: use raw_nishaa/KenPom Barttorvik.csv. See docs/DATA_SOURCES_2025_2026.md
# Run: Rscript scripts/fetch_kenpom_gap.R
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
OUT_DIR <- KENPOM_DIR
GAP_YEARS <- 2018:2023
RATINGS_URL <- "https://raw.githubusercontent.com/andreweatherman/toRvik-data/main/ratings"

#' Fetch ratings for one year: try toRvik API, then GitHub
fetch_ratings_year <- function(yr) {
  x <- NULL
  if (requireNamespace("toRvik", quietly = TRUE)) {
    x <- tryCatch(
      toRvik::bart_ratings(year = yr),
      error = function(e) NULL
    )
  }
  if (is.null(x) || nrow(x) == 0) {
    tmp <- tempfile(fileext = ".csv")
    url <- paste0(RATINGS_URL, "/ratings_", yr, ".csv")
    if (tryCatch(download.file(url, tmp, quiet = TRUE), error = function(e) TRUE) == 0 &&
        file.exists(tmp) && file.info(tmp)$size > 1000) {
      x <- read_csv(tmp, show_col_types = FALSE)
      unlink(tmp)
    }
  }
  x
}

main <- function() {
  dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

  all_data <- list()
  for (yr in GAP_YEARS) {
    message("Fetching ", yr, "...")
    x <- fetch_ratings_year(yr)
    if (is.null(x) || nrow(x) == 0) next

    # Barttorvik/T-Rank: adj_em = adj_o - adj_d (efficiency margin)
    # barthag = win prob vs avg D1 team; use as win_pct proxy when no W-L
    # API returns year, team; GitHub may use different column names
    year_col <- intersect(names(x), c("year", "Year", "YEAR"))[1]
    team_col <- intersect(names(x), c("team", "Team", "TEAM"))[1]
    if (is.na(year_col)) year_col <- names(x)[1]
    if (is.na(team_col)) team_col <- names(x)[2]
    x <- x %>% rename(year = !!sym(year_col), team = !!sym(team_col))

    out <- x %>%
      mutate(
        Season = as.integer(year),
        Team = team,
        adj_em = adj_o - adj_d,
        adj_o = adj_o,
        adj_d = adj_d,
        adj_t = adj_t,
        win_pct = barthag,
        Wins = round(barthag * 32),
        Losses = round((1 - barthag) * 32),
        Games = 32L
      ) %>%
      select(Season, Team, adj_em, adj_o, adj_d, adj_t, win_pct, Wins, Losses, Games)

    all_data[[as.character(yr)]] <- out
  }

  if (length(all_data) == 0) {
    stop("No data fetched. Check toRvik and network.")
  }

  gap_df <- bind_rows(all_data) %>%
    arrange(Season, Team)

  out_path <- file.path(OUT_DIR, "kenpom_gap_2018_2023.csv")
  write_csv(gap_df, out_path)

  message("Wrote ", out_path, " (", nrow(gap_df), " rows, seasons ", 
          paste(range(gap_df$Season), collapse = "-"), ")")
}

main()
