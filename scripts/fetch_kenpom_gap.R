# =============================================================================
# fetch_kenpom_gap.R - Fetch KenPom/Barttorvik data for 2018-2023 gap
# =============================================================================
# Uses toRvik::bart_ratings() to pull T-Rank ratings (adj_o, adj_d, adj_t).
# Saves to data/raw_kenpom/kenpom_gap_2018_2023.csv for load_kenpom_stats().
# Run: Rscript scripts/fetch_kenpom_gap.R
# =============================================================================

library(here)
library(readr)
library(dplyr)

if (!requireNamespace("toRvik", quietly = TRUE)) {
  stop("Install toRvik: devtools::install_github('andreweatherman/toRvik')")
}

OUT_DIR <- here("data", "raw_kenpom")
GAP_YEARS <- 2018:2023

main <- function() {
  dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

  all_data <- list()
  for (yr in GAP_YEARS) {
    message("Fetching ", yr, "...")
    x <- tryCatch(
      toRvik::bart_ratings(year = yr),
      error = function(e) {
        message("  Error for ", yr, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(x) || nrow(x) == 0) next

    # Barttorvik/T-Rank: adj_em = adj_o - adj_d (efficiency margin)
    # barthag = win prob vs avg D1 team; use as win_pct proxy when no W-L
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
