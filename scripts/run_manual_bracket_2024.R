# =============================================================================
# run_manual_bracket_2024.R - Convert bracket_manual_2024.csv to seeds, run prediction
# =============================================================================
# Reads the manual 2024 bracket (Region, Seed, Team), maps to W/X/Y/Z format,
# resolves team names to TeamIDs, adds First Four play-in seeds, writes
# seeds_manual_2024.csv, then runs 04_predict_bracket with that seeds file.
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
source(here("src", "utils", "bracket_utils.R"))

# Region mapping: create_bracket_template uses W=East, X=South, Y=Midwest, Z=West
REGION_MAP <- c(
  East = "W",
  South = "X",
  Midwest = "Y",
  West = "Z"
)

# First Four play-in seeds per bracket_slots.R template: W16, W11, Y11, Z16
# Each needs a/b entries; we assign the post-play-in winner to BOTH (winner advances)
PLAYIN_SEEDS <- list(
  W16 = "W16",   # East 16 -> both W16a and W16b
  W11 = "W11",   # East 11
  Y11 = "Y11",   # Midwest 11
  Z16 = "Z16"    # West 16
)

main <- function() {
  manual_path <- file.path(BRACKET_DIR, "bracket_manual_2024.csv")
  if (!file.exists(manual_path)) {
    stop("Manual bracket not found: ", manual_path)
  }

  message("Reading manual bracket from ", manual_path)
  manual <- read_csv(manual_path, show_col_types = FALSE)

  if (!all(c("Region", "Seed", "Team") %in% names(manual))) {
    stop("Manual bracket must have columns Region, Seed, Team")
  }

  teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)

  message("Resolving team names to TeamIDs (season-aware for 2024)...")
  manual$TeamID <- resolve_team_names_to_ids(manual$Team, teams, season = 2024L)
  missing <- manual %>% filter(is.na(TeamID), trimws(as.character(Team)) != "")
  if (nrow(missing) > 0) {
    stop("Could not resolve team(s): ", paste(missing$Team, collapse = ", "),
         "\nAdd aliases to data/processed/team_name_master.csv or use names from teams.csv")
  }

  # Map Region -> W/X/Y/Z and build Seed string (e.g. W01, X16)
  manual <- manual %>%
    filter(!is.na(TeamID)) %>%
    mutate(
      SeedNum = as.integer(Seed),
      region_code = REGION_MAP[Region],
      Seed = paste0(region_code, sprintf("%02d", SeedNum)),
      Season = 2024L
    ) %>%
    select(Season, Seed, TeamID)

  # Add play-in seeds: slots expect W16a/W16b, W11a/W11b, Y11a/Y11b, Z16a/Z16b.
  # Assign the post-play-in winner to BOTH a and b so the play-in "winner" advances.
  playin_teams <- manual %>%
    filter(Seed %in% c("W16", "W11", "Y11", "Z16")) %>%
    select(Seed, TeamID)

  playin_rows <- bind_rows(
    playin_teams %>% mutate(Seed = paste0(Seed, "a"), Season = 2024L),
    playin_teams %>% mutate(Seed = paste0(Seed, "b"), Season = 2024L)
  ) %>%
    select(Season, Seed, TeamID)

  # Remove W16, W11, Y11, Z16 from manual (replaced by a/b entries)
  seeds_df <- manual %>%
    filter(!Seed %in% c("W16", "W11", "Y11", "Z16")) %>%
    bind_rows(playin_rows)

  seeds_path <- file.path(BRACKET_DIR, "seeds_manual_2024.csv")
  write_csv(seeds_df, seeds_path)
  message("Wrote ", seeds_path, " (", nrow(seeds_df), " seed entries)")

  message("\nRunning bracket prediction with manual 2024 seeds...")
  options(bracket.skip_main = TRUE)
  source(here("src", "04_predict_bracket.R"), local = FALSE)
  options(bracket.skip_main = NULL)

  out <- main(season = 2024L, seeds_file = seeds_path, deterministic = TRUE)

  message("\n--- Predicted Winners (each game) ---")
  print(out$game_results %>% select(team_a_name, team_b_name, winner_name))

  invisible(out)
}

main()
