# =============================================================================
# create_68team_seeds.R - Build 68-team seeds from 64-team + play-in matchups
# =============================================================================
# Use after Selection Sunday when the bracket is announced. Takes 64-team seeds
# and adds the 4 First Four play-in pairs. Template uses W16, W11, Y11, Z16.
#
# Usage:
#   Rscript scripts/create_68team_seeds.R
#   # Then edit data/bracket/seeds_68team_YYYY.csv to fill play-in TeamIDs
#
# Or with play-in teams as arguments (TeamIDs for the 8 play-in teams in order):
#   W16a, W16b, W11a, W11b, Y11a, Y11b, Z16a, Z16b
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
PLAYIN_SEEDS <- c("W16", "W11", "Y11", "Z16")  # Template structure
PLAYIN_SPLIT <- list(
  W16 = c("W16a", "W16b"),
  W11 = c("W11a", "W11b"),
  Y11 = c("Y11a", "Y11b"),
  Z16 = c("Z16a", "Z16b")
)

#' Create 68-team seeds from 64-team seeds + play-in TeamID pairs
#' @param seeds_64 Data frame with Season, Seed, TeamID (64-team)
#' @param season Season year
#' @param playin_teams Named list: W16=c(id1,id2), W11=c(...), Y11=c(...), Z16=c(...)
#'   Or integer vector of length 8: c(W16a,W16b, W11a,W11b, Y11a,Y11b, Z16a,Z16b)
#' @return 68-team seeds
create_68team_seeds <- function(seeds_64, season, playin_teams = NULL) {
  seeds_64 <- seeds_64 %>% filter(Season == season)
  if (nrow(seeds_64) == 0) stop("No seeds for season ", season)
  # Remove play-in parent seeds (W16, W11, Y11, Z16)
  base <- seeds_64 %>% filter(!Seed %in% PLAYIN_SEEDS)
  playin_rows <- tibble()
  if (!is.null(playin_teams)) {
    if (is.numeric(playin_teams) && length(playin_teams) == 8) {
      playin_teams <- list(
        W16 = playin_teams[1:2], W11 = playin_teams[3:4],
        Y11 = playin_teams[5:6], Z16 = playin_teams[7:8]
      )
    }
    for (parent in PLAYIN_SEEDS) {
      kids <- PLAYIN_SPLIT[[parent]]
      ids <- playin_teams[[parent]]
      if (length(ids) >= 2) {
        playin_rows <- bind_rows(playin_rows, tibble(
          Season = season, Seed = kids[1], TeamID = as.integer(ids[1])
        ), tibble(Season = season, Seed = kids[2], TeamID = as.integer(ids[2]))
        )
      }
    }
  }
  if (nrow(playin_rows) == 0) {
    # Placeholder: add play-in seeds with NA for user to fill
    for (parent in PLAYIN_SEEDS) {
      kids <- PLAYIN_SPLIT[[parent]]
      playin_rows <- bind_rows(playin_rows, tibble(
        Season = season, Seed = kids, TeamID = NA_integer_
      ))
    }
    message("Added play-in placeholder rows. Edit the CSV to fill TeamIDs for W16a/b, W11a/b, Y11a/b, Z16a/b.")
  }
  result <- bind_rows(base, playin_rows) %>% arrange(Season, Seed)

  # Sanity check: X16 and Y16 (direct 16-seeds) must not be in play-in teams.
  # W16 and Z16 are play-ins; their teams must not also appear as X16/Y16.
  if (nrow(playin_rows) > 0) {
    playin_ids <- unique(as.integer(playin_rows$TeamID))
    direct_16 <- result %>% filter(Seed %in% c("X16", "Y16"))
    bad <- direct_16 %>% filter(TeamID %in% playin_ids)
    if (nrow(bad) > 0) {
      warning("Teams in play-ins (W16/Z16) also appear as X16/Y16 - they cannot play two games. ",
              "Fix seeds so X16 and Y16 use the direct 16-seeds from tourney_seeds, not play-in teams.")
    }
  }
  result
}

#' Main: create seeds_68team_YYYY.csv
main <- function(season = 2025L, playin_teams = NULL) {
  if (!dir.exists(BRACKET_DIR)) dir.create(BRACKET_DIR, recursive = TRUE)
  seeds_path <- file.path(PROC_DIR, "tourney_seeds.csv")
  if (!file.exists(seeds_path)) {
    stop("Run 02_process_data.R first to create tourney_seeds.csv")
  }
  seeds_64 <- read_csv(seeds_path, show_col_types = FALSE)
  seeds_68 <- create_68team_seeds(seeds_64, season, playin_teams)
  out_path <- file.path(BRACKET_DIR, paste0("seeds_68team_", season, ".csv"))
  write_csv(seeds_68, out_path)
  message("Wrote ", out_path, " (", nrow(seeds_68), " rows)")
  message("To predict with First Four: main(season = ", season,
          ', seeds_file = "', out_path, '") in 04_predict_bracket.R')
  invisible(seeds_68)
}

# Run: Rscript scripts/create_68team_seeds.R [season]
# With play-in TeamIDs: Rscript scripts/create_68team_seeds.R 2025 W16a W16b W11a W11b Y11a Y11b Z16a Z16b
args <- commandArgs(trailingOnly = TRUE)
season <- if (length(args) >= 1) as.integer(args[1]) else 2025L
playin <- if (length(args) >= 9) as.integer(args[2:9]) else NULL  # W16a,W16b, W11a,W11b, Y11a,Y11b, Z16a,Z16b
main(season = season, playin_teams = playin)
