# =============================================================================
# merge_68team_seeds.R - Merge raw_historical 68-team seeds for 2011-2016
# =============================================================================
# When team_id_master exists (from build_canonical_team_master.R), merges
# raw_historical TourneySeeds (canonical Team_Id) into tourney_seeds with
# Kaggle TeamIDs. Uses hybrid merge: for play-in pairs where both teams map,
# use 68-team; otherwise keep base 64-team parent seed so bracket still runs.
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

FIRST_FOUR_YEAR <- 2011L
LAST_RAW_HIST_68 <- 2016L  # raw_historical has 68-team data through 2016

#' Merge 68-team seeds from raw_historical for 2011-2016 when team_id_master exists
#' Hybrid: use 68-team for play-in pairs where both map; else keep 64-team parent.
#' @param base_seeds tourney_seeds from Kaggle (Season, Seed, TeamID)
#' @return Merged seeds
merge_68team_seeds_if_available <- function(base_seeds) {
  master_path <- TEAM_ID_MASTER_PATH
  hist_seeds_path <- file.path(RAW_HIST_DIR, "TourneySeeds.csv")
  if (!file.exists(master_path) || !file.exists(hist_seeds_path)) {
    return(base_seeds)
  }
  source(here("src", "utils", "team_id_remap.R"), local = TRUE)
  hist <- read_csv(hist_seeds_path, show_col_types = FALSE)
  if (!"Team" %in% names(hist) && !"Team_Id" %in% names(hist)) {
    return(base_seeds)
  }
  team_col <- if ("Team" %in% names(hist)) "Team" else "Team_Id"
  hist_all <- hist %>%
    filter(Season >= FIRST_FOUR_YEAR, Season <= LAST_RAW_HIST_68) %>%
    mutate(TeamID = remap_canonical_to_kaggle(as.integer(.data[[team_col]])))

  # Play-in seeds end in 'a' or 'b'; parent is e.g. W16 from W16a/W16b
  hist_playin <- hist_all %>% filter(grepl("[ab]$", Seed))
  # For each play-in pair (W16a+W16b), if BOTH map, we can use 68-team for that slot
  playin_parents <- unique(gsub("[ab]$", "", hist_playin$Seed))
  base_68 <- base_seeds %>% filter(Season >= FIRST_FOUR_YEAR, Season <= LAST_RAW_HIST_68)
  base_other <- base_seeds %>% filter(Season < FIRST_FOUR_YEAR | Season > LAST_RAW_HIST_68)

  out_rows <- list()
  for (yr in unique(base_68$Season)) {
    base_yr <- base_68 %>% filter(Season == yr)
    hist_yr <- hist_all %>% filter(Season == yr)
    # Start with base (64-team)
    # For each play-in parent, check if both a and b map
    to_remove <- character()
    to_add <- tibble(Season = integer(), Seed = character(), TeamID = integer())
    for (parent in playin_parents) {
      kids <- paste0(parent, c("a", "b"))
      hist_kids <- hist_yr %>% filter(Seed %in% kids)
      if (nrow(hist_kids) == 2 && all(!is.na(hist_kids$TeamID))) {
        to_remove <- c(to_remove, parent)
        to_add <- bind_rows(to_add, hist_kids %>% select(Season, Seed, TeamID))
      }
    }
    season_df <- base_yr %>% filter(!Seed %in% to_remove)
    if (nrow(to_add) > 0) {
      season_df <- bind_rows(season_df, to_add)
    }
    out_rows[[length(out_rows) + 1]] <- season_df
  }
  merged <- bind_rows(base_other, bind_rows(out_rows)) %>% arrange(Season, Seed)
  merged
}
