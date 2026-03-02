# =============================================================================
# 00_create_sample_data.R - Create minimal sample data for testing pipeline
# =============================================================================
# Run this ONLY if you don't have Kaggle data. Creates minimal CSVs so the
# pipeline can run. For real predictions, use 01_download_data.R with Kaggle.
# =============================================================================

library(here)
library(readr)
library(dplyr)

RAW_DIR <- here("data", "raw")
if (!dir.exists(RAW_DIR)) dir.create(RAW_DIR, recursive = TRUE)

# Minimal teams (64 teams x 5 years = 320 teams)
team_ids <- 1101:(1101 + 64 * 5 - 1)
teams <- data.frame(
  TeamID = team_ids,
  TeamName = paste0("Team_", team_ids)
)
write_csv(teams, file.path(RAW_DIR, "MTeams.csv"))

# Seasons
seasons <- data.frame(Season = 2020:2024, DayZero = "2020-11-25")
write_csv(seasons, file.path(RAW_DIR, "MSeasons.csv"))

# Tournament seeds (4 regions x 16 seeds = 64 teams per year)
regions <- c("W", "X", "Y", "Z")
seeds_list <- list()
for (yr in 2020:2024) {
  tid <- 1101 + (yr - 2020) * 64
  for (r in regions) {
    for (s in 1:16) {
      seeds_list[[length(seeds_list) + 1]] <- data.frame(
        Season = yr,
        Seed = sprintf("%s%02d", r, s),
        TeamID = tid
      )
      tid <- tid + 1
    }
  }
}
tourney_seeds <- do.call(rbind, seeds_list)
write_csv(tourney_seeds, file.path(RAW_DIR, "MNCAATourneySeeds.csv"))

# Tournament results (simulated historical games)
# Build ~200 games across 2020-2023
set.seed(42)
results_list <- list()
for (yr in 2020:2023) {
  seed_df <- tourney_seeds %>% filter(Season == yr)
  n_games <- 60 + sample(10, 1)
  for (g in 1:n_games) {
    pair <- seed_df[sample(nrow(seed_df), 2), ]
    # Higher seed (lower number) wins ~65% of the time
    s1 <- as.integer(gsub("[A-Z]", "", pair$Seed[1]))
    s2 <- as.integer(gsub("[A-Z]", "", pair$Seed[2]))
    winner <- if (s1 <= s2 && runif(1) < 0.65) 1 else 2
    results_list[[length(results_list) + 1]] <- data.frame(
      Season = yr,
      DayNum = 134 + g %% 10,
      WTeamID = pair$TeamID[winner],
      LTeamID = pair$TeamID[3 - winner],
      WScore = 70 + sample(20, 1),
      LScore = 60 + sample(20, 1)
    )
  }
}
tourney_results <- do.call(rbind, results_list)
write_csv(tourney_results, file.path(RAW_DIR, "MNCAATourneyCompactResults.csv"))

# Regular season results (for win pct, points)
reg_list <- list()
for (yr in 2020:2024) {
  tid <- 1101 + (yr - 2020) * 64
  team_ids_yr <- tid:(tid + 63)
  for (i in 1:min(200, length(team_ids_yr)^2 %/% 4)) {
    t1 <- sample(team_ids_yr, 1)
    t2 <- sample(team_ids_yr[team_ids_yr != t1], 1)
    reg_list[[length(reg_list) + 1]] <- data.frame(
      Season = yr,
      DayNum = sample(1:120, 1),
      WTeamID = t1,
      LTeamID = t2,
      WScore = 70 + sample(15, 1),
      LScore = 65 + sample(15, 1)
    )
  }
}
regular_results <- do.call(rbind, reg_list)
write_csv(regular_results, file.path(RAW_DIR, "MRegularSeasonCompactResults.csv"))

# Bracket slots (simplified 64-team structure)
# Round 1: 32 games, seeds like W01 vs W16, etc.
slots <- data.frame(
  Slot = character(),
  Strong = character(),
  Weak = character(),
  stringsAsFactors = FALSE
)
r1_slots <- c()
for (r in regions) {
  for (i in 1:8) {
    slot <- sprintf("R1%s%d", r, i)
    strong <- sprintf("%s%02d", r, i)
    weak <- sprintf("%s%02d", r, 17 - i)
    r1_slots <- c(r1_slots, slot)
    slots <- rbind(slots, data.frame(Slot = slot, Strong = strong, Weak = weak))
  }
}
# Round 2: 16 games
r2_slots <- c()
for (i in 1:16) {
  s1 <- r1_slots[2 * i - 1]
  s2 <- r1_slots[2 * i]
  slot <- sprintf("R2G%d", i)
  r2_slots <- c(r2_slots, slot)
  slots <- rbind(slots, data.frame(Slot = slot, Strong = s1, Weak = s2))
}
# Round 3: 8 games
r3_slots <- c()
for (i in 1:8) {
  s1 <- r2_slots[2 * i - 1]
  s2 <- r2_slots[2 * i]
  slot <- sprintf("R3G%d", i)
  r3_slots <- c(r3_slots, slot)
  slots <- rbind(slots, data.frame(Slot = slot, Strong = s1, Weak = s2))
}
# Round 4: 4 games
r4_slots <- c()
for (i in 1:4) {
  s1 <- r3_slots[2 * i - 1]
  s2 <- r3_slots[2 * i]
  slot <- sprintf("R4G%d", i)
  r4_slots <- c(r4_slots, slot)
  slots <- rbind(slots, data.frame(Slot = slot, Strong = s1, Weak = s2))
}
# Round 5: 2 games (Final Four)
r5_slots <- c()
for (i in 1:2) {
  s1 <- r4_slots[2 * i - 1]
  s2 <- r4_slots[2 * i]
  slot <- sprintf("R5G%d", i)
  r5_slots <- c(r5_slots, slot)
  slots <- rbind(slots, data.frame(Slot = slot, Strong = s1, Weak = s2))
}
# Round 6: Championship
slots <- rbind(slots, data.frame(Slot = "R6", Strong = r5_slots[1], Weak = r5_slots[2]))

names(slots) <- c("Slot", "StrongSeed", "WeakSeed")
write_csv(slots, file.path(RAW_DIR, "MNCAATourneySlots.csv"))

message("Sample data created in ", RAW_DIR)
message("WARNING: This is synthetic data for testing. Use 01_download_data.R for real Kaggle data.")
