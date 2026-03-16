# =============================================================================
# 04_predict_bracket.R - Load model and bracket, simulate, output predictions
# =============================================================================
# Run after training to predict the full bracket for a given season.
# Set PREDICT_SEASON to the year you want to predict (e.g., 2024).
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(tidymodels)  # for predict.workflow on saved model

source(here("src", "config.R"))
source(here("src", "utils", "feature_engineering.R"))  # compute_matchup_features for simulate_bracket

# Which season to predict (use seeds from this year)
PREDICT_SEASON <- 2026L

#' Read CSV if file exists, else NULL
read_optional_csv <- function(path) {
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else NULL
}

#' Build audit of which features used defaults (missing data) per game
#' @return Tibble with slot, round, team_a, team_b, team_a_missing, team_b_missing, prob_a
build_prediction_defaults_audit <- function(game_results, season,
                                            win_pct, points_stats, kenpom_stats,
                                            late_win_pct, recent_win_pct, recent_mov,
                                            sos_stats, rest_stats, conference_stats,
                                            quadrant_stats, resume_stats, home_away_stats,
                                            first_four_stats) {
  has_data <- function(df, sid, tid) {
    if (is.null(df) || nrow(df) == 0) return(FALSE)
    if (!"TeamID" %in% names(df)) return(FALSE)
    nrow(df %>% filter(Season == sid, TeamID == tid)) > 0
  }
  game_results %>%
    rowwise() %>%
    mutate(
      team_a_missing = paste(c(
        if (!has_data(win_pct, season, team_a)) "win_pct",
        if (!has_data(points_stats, season, team_a)) "points_stats",
        if (!has_data(kenpom_stats, season, team_a)) "kenpom",
        if (!has_data(late_win_pct, season, team_a)) "late_win_pct",
        if (!has_data(recent_win_pct, season, team_a)) "recent_win_pct",
        if (!has_data(recent_mov, season, team_a)) "recent_mov",
        if (!has_data(sos_stats, season, team_a)) "sos",
        if (!has_data(rest_stats, season, team_a)) "rest",
        if (!has_data(conference_stats, season, team_a)) "conference",
        if (!has_data(quadrant_stats, season, team_a)) "quadrant",
        if (!has_data(resume_stats, season, team_a)) "resume",
        if (!has_data(home_away_stats, season, team_a)) "home_away",
        if (!has_data(first_four_stats, season, team_a)) "first_four"
      ), collapse = ","),
      team_b_missing = paste(c(
        if (!has_data(win_pct, season, team_b)) "win_pct",
        if (!has_data(points_stats, season, team_b)) "points_stats",
        if (!has_data(kenpom_stats, season, team_b)) "kenpom",
        if (!has_data(late_win_pct, season, team_b)) "late_win_pct",
        if (!has_data(recent_win_pct, season, team_b)) "recent_win_pct",
        if (!has_data(recent_mov, season, team_b)) "recent_mov",
        if (!has_data(sos_stats, season, team_b)) "sos",
        if (!has_data(rest_stats, season, team_b)) "rest",
        if (!has_data(conference_stats, season, team_b)) "conference",
        if (!has_data(quadrant_stats, season, team_b)) "quadrant",
        if (!has_data(resume_stats, season, team_b)) "resume",
        if (!has_data(home_away_stats, season, team_b)) "home_away",
        if (!has_data(first_four_stats, season, team_b)) "first_four"
      ), collapse = ",")
    ) %>%
    ungroup() %>%
    select(slot, round, team_a, team_b, team_a_missing, team_b_missing, prob_a)
}

#' Load model and processed data
#' @param seeds_file Optional path to seeds CSV (Season, Seed, TeamID). If NULL, use tourney_seeds.csv.
load_for_prediction <- function(seeds_file = NULL) {
  model_path <- file.path(MODELS_DIR, "bracket_model.rds")
  if (!file.exists(model_path)) {
    stop("Model not found. Run 03_train_model.R first.")
  }

  model <- readRDS(model_path)
  source(here("src", "utils", "kenpom_utils.R"), local = TRUE)

  win_pct <- read_csv(file.path(PROC_DIR, "win_pct.csv"), show_col_types = FALSE)
  points_stats <- read_csv(file.path(PROC_DIR, "points_stats.csv"), show_col_types = FALSE)
  late_win_pct <- read_optional_csv(file.path(PROC_DIR, "late_win_pct.csv"))
  recent_win_pct <- read_optional_csv(file.path(PROC_DIR, "recent_win_pct.csv"))
  recent_mov <- read_optional_csv(file.path(PROC_DIR, "recent_mov.csv"))
  seeds <- if (is.null(seeds_file)) {
    read_csv(file.path(PROC_DIR, "tourney_seeds.csv"), show_col_types = FALSE)
  } else {
    if (!file.exists(seeds_file)) stop("Seeds file not found: ", seeds_file)
    read_csv(seeds_file, show_col_types = FALSE)
  }
  slots <- read_csv(file.path(PROC_DIR, "tourney_slots.csv"), show_col_types = FALSE)
  teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)
  kenpom_stats <- read_optional_csv(file.path(PROC_DIR, "kenpom_stats.csv"))

  # Augment KenPom for prediction seasons not in processed (e.g. 2025)
  # Processed data is built from historical seeds; future seasons need raw KenPom/Barttorvik
  seeds_seasons <- unique(seeds$Season)
  kp_seasons <- if (!is.null(kenpom_stats) && nrow(kenpom_stats) > 0) unique(kenpom_stats$Season) else integer()
  missing_kp_seasons <- setdiff(seeds_seasons, kp_seasons)
  if (length(missing_kp_seasons) > 0) {
    source(here("src", "utils", "kenpom_utils.R"), local = TRUE)
    kp_raw <- load_kenpom_stats(seeds, teams)
    if (nrow(kp_raw) > 0) {
      kp_augment <- kp_raw %>% filter(Season %in% missing_kp_seasons)
      if (nrow(kp_augment) > 0) {
        kenpom_stats <- if (is.null(kenpom_stats) || nrow(kenpom_stats) == 0) {
          kp_augment
        } else {
          bind_rows(kenpom_stats, kp_augment) %>% distinct(Season, TeamID, .keep_all = TRUE)
        }
        message("Augmented KenPom for season(s) ", paste(missing_kp_seasons, collapse = ", "), " from raw data")
      }
    }
  }
  home_away_stats <- read_optional_csv(file.path(PROC_DIR, "home_away_stats.csv"))
  resume_stats <- read_optional_csv(file.path(PROC_DIR, "resume_stats.csv"))
  head_to_head <- read_optional_csv(file.path(PROC_DIR, "head_to_head.csv"))
  sos_stats <- read_optional_csv(file.path(PROC_DIR, "sos_stats.csv"))
  rest_stats <- read_optional_csv(file.path(PROC_DIR, "rest_stats.csv"))
  conference_stats <- read_optional_csv(file.path(PROC_DIR, "conference_stats.csv"))
  quadrant_stats <- read_optional_csv(file.path(PROC_DIR, "quadrant_stats.csv"))
  first_four_stats <- read_optional_csv(file.path(PROC_DIR, "first_four_stats.csv"))
  tourney_history_stats <- read_optional_csv(file.path(PROC_DIR, "tourney_history_stats.csv"))
  tourney_h2h <- read_optional_csv(file.path(PROC_DIR, "tourney_h2h.csv"))
  upset_history <- read_optional_csv(file.path(PROC_DIR, "upset_history.csv"))

  # For future/manual seed seasons (e.g. 2026), re-load auxiliary team-level features
  # directly from raw sources when processed files do not include those seasons.
  augment_team_feature <- function(existing_df, fresh_df, label) {
    if (is.null(fresh_df) || nrow(fresh_df) == 0) return(existing_df)
    if (!all(c("Season", "TeamID") %in% names(fresh_df))) return(existing_df)
    have <- if (!is.null(existing_df) && nrow(existing_df) > 0 &&
                  all(c("Season", "TeamID") %in% names(existing_df))) {
      unique(existing_df$Season)
    } else integer()
    need <- setdiff(seeds_seasons, have)
    if (length(need) == 0) return(existing_df)
    to_add <- fresh_df %>% filter(Season %in% need)
    if (nrow(to_add) == 0) return(existing_df)
    out <- if (is.null(existing_df) || nrow(existing_df) == 0) {
      to_add
    } else {
      bind_rows(existing_df, to_add) %>% distinct(Season, TeamID, .keep_all = TRUE)
    }
    message("Augmented ", label, " for season(s) ", paste(sort(unique(to_add$Season)), collapse = ", "),
            " (", nrow(to_add), " rows)")
    out
  }

  ensure_seed_defaults <- function(df, defaults, label) {
    keys <- seeds %>% distinct(Season, TeamID)
    if (is.null(df) || nrow(df) == 0) {
      df <- keys
      added <- nrow(keys)
    } else {
      if (!all(c("Season", "TeamID") %in% names(df))) return(df)
      missing_keys <- keys %>% anti_join(df %>% select(Season, TeamID) %>% distinct(),
                                         by = c("Season", "TeamID"))
      added <- nrow(missing_keys)
      if (added > 0) df <- bind_rows(df, missing_keys)
    }
    for (nm in names(defaults)) {
      def <- defaults[[nm]]
      if (!nm %in% names(df)) {
        df[[nm]] <- def
      } else {
        df[[nm]] <- ifelse(is.na(df[[nm]]), def, df[[nm]])
      }
    }
    if (added > 0) {
      message("Filled ", label, " defaults for ", added, " seed-team rows")
    }
    df
  }

  lookup <- build_season_team_lookup(seeds, teams)
  home_away_raw <- load_home_away_win_rates(lookup = lookup)
  resume_raw <- load_resume_stats(lookup = lookup)
  bt_resume_raw <- load_barttorvik_resume_metrics(lookup = lookup)
  if (nrow(bt_resume_raw) > 0) {
    resume_raw <- if (nrow(resume_raw) > 0) {
      resume_raw %>% full_join(bt_resume_raw, by = c("Season", "TeamID"))
    } else {
      bt_resume_raw
    }
  }
  conference_raw <- load_conference_strength(lookup = lookup)
  quadrant_raw <- load_quadrant_stats(lookup = lookup)

  home_away_stats <- augment_team_feature(home_away_stats, home_away_raw, "home/away stats")
  resume_stats <- augment_team_feature(resume_stats, resume_raw, "resume stats")
  conference_stats <- augment_team_feature(conference_stats, conference_raw, "conference stats")
  quadrant_stats <- augment_team_feature(quadrant_stats, quadrant_raw, "quadrant stats")

  # First Four status can be inferred directly from a/b split seeds for prediction seasons.
  have_ff <- if (!is.null(first_four_stats) && nrow(first_four_stats) > 0 &&
                   "Season" %in% names(first_four_stats)) unique(first_four_stats$Season) else integer()
  need_ff <- setdiff(seeds_seasons, have_ff)
  if (length(need_ff) > 0) {
    ff_from_seeds <- seeds %>%
      filter(Season %in% need_ff) %>%
      distinct(Season, TeamID, Seed) %>%
      mutate(played_first_four = as.integer(grepl("[ab]$", Seed))) %>%
      select(Season, TeamID, played_first_four)
    first_four_stats <- if (is.null(first_four_stats) || nrow(first_four_stats) == 0) {
      ff_from_seeds
    } else {
      bind_rows(first_four_stats, ff_from_seeds) %>% distinct(Season, TeamID, .keep_all = TRUE)
    }
    message("Augmented first_four stats from seeds for season(s) ", paste(sort(need_ff), collapse = ", "),
            " (", nrow(ff_from_seeds), " rows)")
  }

  # Ensure every seeded team has explicit rows in team-level feature tables.
  # This does not change model behavior vs current fallback defaults; it prevents
  # silent sparsity and keeps defaults-audit focused on true data gaps.
  win_pct <- ensure_seed_defaults(win_pct, list(WinPct = 0.5, Wins = 0L, Losses = 0L, Games = 0L), "win_pct")
  points_stats <- ensure_seed_defaults(points_stats, list(PF_per_game = 70, PA_per_game = 70), "points_stats")
  kenpom_stats <- ensure_seed_defaults(kenpom_stats, list(
    adj_em = 0, adj_o = 100, adj_d = 100, adj_t = 68, luck = 0, win_pct = 0.5, Wins = 0L, Losses = 0L, Games = 0L
  ), "kenpom")
  late_win_pct <- ensure_seed_defaults(late_win_pct, list(LateWinPct = 0.5, LateWins = 0L, LateLosses = 0L, LateGames = 0L), "late_win_pct")
  recent_win_pct <- ensure_seed_defaults(recent_win_pct, list(RecentWinPct = 0.5, RecentWins = 0L, RecentLosses = 0L, RecentGames = 0L), "recent_win_pct")
  recent_mov <- ensure_seed_defaults(recent_mov, list(RecentMOV = 0), "recent_mov")
  sos_stats <- ensure_seed_defaults(sos_stats, list(sos = 0.5), "sos")
  rest_stats <- ensure_seed_defaults(rest_stats, list(days_rest = 0L), "rest")
  conference_stats <- ensure_seed_defaults(conference_stats, list(conf_em = 0), "conference")
  quadrant_stats <- ensure_seed_defaults(quadrant_stats, list(quad1_winpct = 0.5, quad12_winpct = 0.5), "quadrant")
  home_away_stats <- ensure_seed_defaults(home_away_stats, list(home_win_rate = 0.5, away_win_rate = 0.5), "home_away")
  resume_stats <- ensure_seed_defaults(resume_stats, list(elo = 0, net = 200, wab = 200, barthag = 0.5, elite_sos = 0), "resume")
  first_four_stats <- ensure_seed_defaults(first_four_stats, list(played_first_four = 0L), "first_four")

  # Compute historical tournament features for prediction seasons not in saved data (e.g. 2026)
  tourney_results_path <- file.path(RAW_EXTENDED_DIR, "MNCAATourneyCompactResults.csv")
  if (!file.exists(tourney_results_path)) tourney_results_path <- file.path(RAW_DIR, "MNCAATourneyCompactResults.csv")
  pred_seasons <- setdiff(seeds_seasons, if (!is.null(tourney_history_stats) && nrow(tourney_history_stats) > 0) unique(tourney_history_stats$Season) else integer())
  if (length(pred_seasons) > 0 && file.exists(tourney_results_path)) {
    tourney_results <- read_csv(tourney_results_path, show_col_types = FALSE)
    if (!"WTeamID" %in% names(tourney_results)) {
      idx <- grep("W.*Team|Winner|Wteam", names(tourney_results), ignore.case = TRUE)
      if (length(idx) >= 1) names(tourney_results)[idx[1]] <- "WTeamID"
    }
    if (!"LTeamID" %in% names(tourney_results)) {
      idx <- grep("L.*Team|Loser|Lteam", names(tourney_results), ignore.case = TRUE)
      if (length(idx) >= 1) names(tourney_results)[idx[1]] <- "LTeamID"
    }
    predict_seeds <- seeds %>% filter(Season %in% pred_seasons) %>% select(Season, TeamID)
    th_pred <- compute_tourney_history_stats(tourney_results, seeds, n_years = 5L, predict_seeds = predict_seeds)
    uh_pred <- compute_upset_history(tourney_results, seeds, n_years = 5L, predict_seeds = predict_seeds)
    tourney_history_stats <- if (is.null(tourney_history_stats) || nrow(tourney_history_stats) == 0) {
      th_pred
    } else {
      bind_rows(tourney_history_stats, th_pred %>% filter(Season %in% pred_seasons)) %>%
        distinct(Season, TeamID, .keep_all = TRUE)
    }
    upset_history <- if (is.null(upset_history) || nrow(upset_history) == 0) {
      uh_pred
    } else {
      bind_rows(upset_history, uh_pred %>% filter(Season %in% pred_seasons)) %>%
        distinct(Season, TeamID, .keep_all = TRUE)
    }
    if (nrow(th_pred) > 0) message("Computed tourney history for prediction season(s) ", paste(pred_seasons, collapse = ", "))
  }

  # Fill missing win_pct from KenPom for seasons not in regular-season data (e.g. 2025)
  # Uses kenpom_stats (already augmented above if prediction season was missing)
  seeds_needed <- seeds %>% distinct(Season, TeamID)
  win_pct_have <- win_pct %>% distinct(Season, TeamID)
  missing <- seeds_needed %>% anti_join(win_pct_have, by = c("Season", "TeamID"))
  if (nrow(missing) > 0 && !is.null(kenpom_stats) && nrow(kenpom_stats) > 0 && "win_pct" %in% names(kenpom_stats)) {
    kp_win <- kenpom_stats %>%
      filter(!is.na(win_pct), (Season %in% missing$Season)) %>%
      mutate(
        Wins = if ("Wins" %in% names(.)) Wins else round(win_pct * 32),
        Losses = if ("Losses" %in% names(.)) Losses else round((1 - win_pct) * 32),
        Games = if ("Games" %in% names(.)) Games else pmax(1L, as.integer(Wins + Losses))
      ) %>%
      select(Season, TeamID, WinPct = win_pct, Wins, Losses, Games)
    to_add <- kp_win %>% inner_join(missing, by = c("Season", "TeamID"))
    if (nrow(to_add) > 0) {
      win_pct <- bind_rows(win_pct, to_add)
      message("Filled ", nrow(to_add), " win_pct rows from KenPom for missing season(s)")
    }
  }

  list(
    model = model,
    win_pct = win_pct,
    points_stats = points_stats,
    late_win_pct = late_win_pct,
    recent_win_pct = recent_win_pct,
    recent_mov = recent_mov,
    seeds = seeds,
    slots = slots,
    teams = teams,
    kenpom_stats = kenpom_stats,
    home_away_stats = home_away_stats,
    resume_stats = resume_stats,
    head_to_head = head_to_head,
    sos_stats = sos_stats,
    rest_stats = rest_stats,
    conference_stats = conference_stats,
    quadrant_stats = quadrant_stats,
    first_four_stats = first_four_stats,
    tourney_history_stats = tourney_history_stats,
    tourney_h2h = tourney_h2h,
    upset_history = upset_history
  )
}

#' Run bracket simulation
#' @param season Season year
#' @param seeds_file Optional path to seeds CSV for projected bracket. If NULL, use tourney_seeds.csv.
#' @param use_projected_output If TRUE, write to bracket_prediction_projected_YEAR.csv
#' @param deterministic If TRUE, pick higher-probability team; if FALSE, sample
# Manual seeds override: use this file for 2025 predictions (correct bracket from Selection Sunday)
SEEDS_2025_PATH <- file.path(BRACKET_DIR, "seeds_68team_2025.csv")

main <- function(season = PREDICT_SEASON, seeds_file = NULL, use_projected_output = FALSE, deterministic = TRUE) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  # Use manual 2025 seeds when predicting 2025 and no seeds_file specified
  if (season == 2025L && is.null(seeds_file) && file.exists(SEEDS_2025_PATH)) {
    seeds_file <- SEEDS_2025_PATH
    message("Using manual seeds: ", SEEDS_2025_PATH)
  }

  message("Loading model and data...")
  data <- load_for_prediction(seeds_file = seeds_file)

  # Filter seeds and slots for the season
  # Some datasets have slots per season; if not, use all slots
  seeds_season <- data$seeds %>% filter(Season == season)
  if (nrow(seeds_season) == 0) {
    message("No seeds for season ", season, ". Using latest available season.")
    available <- unique(data$seeds$Season)
    season <- max(available)
    seeds_season <- data$seeds %>% filter(Season == season)
  }

  # Build season-specific bracket slots (supports 68-team First Four templates)
  source(here('src', 'utils', 'bracket_slots.R'), local = TRUE)
  slots_season <- get_slots_for_season(season, data$slots)

  message("Simulating bracket for season ", season, "...")
  source(here("src", "utils", "bracket_logic.R"), local = TRUE)

  result <- simulate_bracket(
    season = season,
    slots_df = slots_season,
    seeds_df = seeds_season,
    model = data$model,
    win_pct = data$win_pct,
    points_stats = data$points_stats,
    kenpom_stats = data$kenpom_stats,
    late_win_pct = data$late_win_pct,
    recent_win_pct = data$recent_win_pct,
    recent_mov = data$recent_mov,
    home_away_stats = data$home_away_stats,
    resume_stats = data$resume_stats,
    head_to_head = data$head_to_head,
    sos_stats = data$sos_stats,
    rest_stats = data$rest_stats,
    conference_stats = data$conference_stats,
    quadrant_stats = data$quadrant_stats,
    first_four_stats = data$first_four_stats,
    tourney_history_stats = data$tourney_history_stats,
    tourney_h2h = data$tourney_h2h,
    upset_history = data$upset_history,
    deterministic = deterministic
  )

  # Add team names to results (handle TeamName or Name column)
  team_name_col <- intersect(names(data$teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_name_col)) team_name_col <- names(data$teams)[2]

  lookup <- setNames(data$teams[[team_name_col]], data$teams$TeamID)
  # Fallback names from bracket reference (useful when manual seed TeamIDs are not
  # present in processed teams.csv but are valid for a prediction season).
  ref_path <- file.path(BRACKET_DIR, paste0("bracket_reference_", season, ".csv"))
  if (file.exists(ref_path) && all(c("Seed", "TeamName") %in% names(read_csv(ref_path, n_max = 1, show_col_types = FALSE)))) {
    ref <- read_csv(ref_path, show_col_types = FALSE) %>%
      select(Seed, TeamName) %>%
      distinct(Seed, .keep_all = TRUE)
    ref_lookup <- seeds_season %>%
      select(TeamID, Seed) %>%
      left_join(ref, by = "Seed") %>%
      filter(!is.na(TeamName)) %>%
      mutate(TeamID_chr = as.character(TeamID))
    if (nrow(ref_lookup) > 0) {
      needs <- is.na(lookup[ref_lookup$TeamID_chr])
      if (any(needs)) {
        fill_ids <- ref_lookup$TeamID_chr[needs]
        fill_names <- ref_lookup$TeamName[needs]
        lookup[fill_ids] <- fill_names
        message("Filled ", sum(needs), " team names from ", basename(ref_path))
      }
    }
  }

  game_results <- result$game_results %>%
    mutate(
      team_a_name = lookup[as.character(team_a)],
      team_b_name = lookup[as.character(team_b)],
      winner_name = lookup[as.character(winner)]
    )

  champ_name <- lookup[as.character(result$champion)]

  message("Predicted champion: ", champ_name, " (TeamID ", result$champion, ")")

  # Audit: which teams used default values (missing data) per game
  defaults_audit <- build_prediction_defaults_audit(
    game_results = result$game_results,
    season = season,
    win_pct = data$win_pct,
    points_stats = data$points_stats,
    kenpom_stats = data$kenpom_stats,
    late_win_pct = data$late_win_pct,
    recent_win_pct = data$recent_win_pct,
    recent_mov = data$recent_mov,
    sos_stats = data$sos_stats,
    rest_stats = data$rest_stats,
    conference_stats = data$conference_stats,
    quadrant_stats = data$quadrant_stats,
    resume_stats = data$resume_stats,
    home_away_stats = data$home_away_stats,
    first_four_stats = data$first_four_stats
  )
  defaults_audit <- defaults_audit %>%
    mutate(
      team_a_name = lookup[as.character(team_a)],
      team_b_name = lookup[as.character(team_b)]
    )
  defaults_base <- if (use_projected_output) paste0("prediction_defaults_projected_", season) else paste0("prediction_defaults_", season)
  defaults_file <- file.path(OUTPUT_DIR, paste0(defaults_base, ".csv"))
  write_csv(defaults_audit, defaults_file)
  message("Prediction defaults audit saved to ", defaults_file)

  # Save outputs
  out_base <- if (use_projected_output) paste0("bracket_prediction_projected_", season) else paste0("bracket_prediction_", season)
  out_file <- file.path(OUTPUT_DIR, paste0(out_base, ".csv"))
  write_csv(game_results, out_file)
  message("Bracket predictions saved to ", out_file)

  # Save champion
  champ_base <- if (use_projected_output) paste0("champion_projected_", season) else paste0("champion_", season)
  champ_file <- file.path(OUTPUT_DIR, paste0(champ_base, ".txt"))
  writeLines(c(
    paste("Season:", season),
    paste("Predicted Champion:", champ_name),
    paste("TeamID:", result$champion)
  ), champ_file)

  invisible(list(
    champion = result$champion,
    champion_name = champ_name,
    game_results = game_results,
    season = season
  ))
}

if (!isTRUE(getOption("bracket.skip_main"))) {
  main()
}
