# =============================================================================
# analyze_h2h_dominance.R - Investigate H2H feature dominance in the model
# =============================================================================
# Run: Rscript scripts/analyze_h2h_dominance.R
# Output: Console summary + optional CSV
# =============================================================================

library(here)
library(readr)
library(dplyr)

PROC_DIR <- here("data", "processed")
md <- read_csv(file.path(PROC_DIR, "matchup_data.csv"), show_col_types = FALSE)

# Ensure outcome is binary (1 = TeamA won)
md <- md %>% mutate(outcome = as.integer(outcome))

# -----------------------------------------------------------------------------
# 1. H2H coverage: how many games have prior H2H?
# -----------------------------------------------------------------------------
h2h_coverage <- md %>%
  mutate(has_h2h = h2h_games > 0) %>%
  summarise(
    n_total = n(),
    n_with_h2h = sum(has_h2h),
    pct_with_h2h = 100 * mean(has_h2h),
    .groups = "drop"
  )

message("========== H2H COVERAGE ==========")
message(sprintf("Total tournament games: %d", h2h_coverage$n_total))
message(sprintf("Games with prior H2H (h2h_games > 0): %d (%.1f%%)", h2h_coverage$n_with_h2h, h2h_coverage$pct_with_h2h))
message(sprintf("Games with NO H2H (h2h_games = 0): %d (%.1f%%)", h2h_coverage$n_total - h2h_coverage$n_with_h2h, 100 - h2h_coverage$pct_with_h2h))

# -----------------------------------------------------------------------------
# 2. Distribution of h2h_games when h2h_games > 0
# -----------------------------------------------------------------------------
h2h_dist <- md %>%
  filter(h2h_games > 0) %>%
  count(h2h_games, name = "n_games") %>%
  arrange(h2h_games)

message("\n========== H2H GAMES DISTRIBUTION (when > 0) ==========")
for (i in seq_len(nrow(h2h_dist))) {
  message(sprintf("  h2h_games = %d: %d matchups", h2h_dist$h2h_games[i], h2h_dist$n_games[i]))
}

# -----------------------------------------------------------------------------
# 3. H2H as sole predictor: accuracy when H2H exists vs when it doesn't
# -----------------------------------------------------------------------------
# When h2h_games > 0: predict TeamA wins if h2h_team_a_winpct > 0.5
# When h2h_games = 0: h2h_team_a_winpct is always 0.5 (neutral) - no prediction from H2H
pred_h2h_only <- md %>%
  mutate(
    pred_team_a = case_when(
      h2h_games == 0 ~ NA_integer_,  # no H2H => no prediction
      h2h_team_a_winpct > 0.5 ~ 1L,
      h2h_team_a_winpct < 0.5 ~ 0L,
      TRUE ~ NA_integer_  # exactly 0.5 => tie
    )
  )

acc_with_h2h <- pred_h2h_only %>%
  filter(h2h_games > 0, !is.na(pred_team_a)) %>%
  summarise(
    n = n(),
    correct = sum(pred_team_a == outcome),
    acc = 100 * mean(pred_team_a == outcome),
    .groups = "drop"
  )

acc_no_h2h <- pred_h2h_only %>%
  filter(h2h_games == 0) %>%
  summarise(n = n(), .groups = "drop")

message("\n========== H2H AS SOLE PREDICTOR ==========")
message(sprintf("When H2H exists (h2h_games > 0): predict TeamA if h2h_team_a_winpct > 0.5"))
message(sprintf("  N games: %d", acc_with_h2h$n))
message(sprintf("  Accuracy: %.1f%% (%d / %d correct)", acc_with_h2h$acc, acc_with_h2h$correct, acc_with_h2h$n))
message(sprintf("When H2H does NOT exist: h2h_team_a_winpct = 0.5 (neutral) for all %d games", acc_no_h2h$n))

# -----------------------------------------------------------------------------
# 4. Per-year breakdown
# -----------------------------------------------------------------------------
per_year <- md %>%
  group_by(Season) %>%
  summarise(
    n = n(),
    n_with_h2h = sum(h2h_games > 0),
    pct_h2h = 100 * mean(h2h_games > 0),
    .groups = "drop"
  )

message("\n========== H2H COVERAGE BY SEASON ==========")
print(per_year, n = Inf)

# -----------------------------------------------------------------------------
# 5. Correlation: h2h_team_a_winpct vs outcome (only when h2h_games > 0)
# -----------------------------------------------------------------------------
md_h2h <- md %>% filter(h2h_games > 0)
if (nrow(md_h2h) > 0) {
  # Shift winpct to [-0.5, 0.5] so 0.5 = neutral
  cor_val <- cor(md_h2h$h2h_team_a_winpct - 0.5, md_h2h$outcome, use = "complete.obs")
  message("\n========== CORRELATION ==========")
  message(sprintf("Correlation (h2h_team_a_winpct - 0.5) vs outcome, when h2h_games > 0: %.3f", cor_val))
}

# -----------------------------------------------------------------------------
# 6. Potential overfitting: small-sample H2H (1-2 games) vs larger (3+)
# -----------------------------------------------------------------------------
small_h2h <- md %>% filter(h2h_games >= 1, h2h_games <= 2)
large_h2h <- md %>% filter(h2h_games >= 3)

acc_small <- small_h2h %>%
  filter(h2h_team_a_winpct != 0.5) %>%
  mutate(pred = as.integer(h2h_team_a_winpct > 0.5)) %>%
  summarise(n = n(), acc = 100 * mean(pred == outcome), .groups = "drop")

acc_large <- large_h2h %>%
  filter(h2h_team_a_winpct != 0.5) %>%
  mutate(pred = as.integer(h2h_team_a_winpct > 0.5)) %>%
  summarise(n = n(), acc = 100 * mean(pred == outcome), .groups = "drop")

message("\n========== H2H ACCURACY BY SAMPLE SIZE ==========")
message(sprintf("1-2 H2H games: N=%d, accuracy=%.1f%%", acc_small$n, acc_small$acc))
message(sprintf("3+ H2H games: N=%d, accuracy=%.1f%%", acc_large$n, acc_large$acc))

# -----------------------------------------------------------------------------
# 7. Feature importance context: H2H vs other top features
# -----------------------------------------------------------------------------
message("\n========== SUMMARY ==========")
message("H2H is computed from same-season regular results (no leakage).")
message("When h2h_games=0, h2h_team_a_winpct=0.5 (neutral).")
message("High RF importance (80) may reflect:")
message("  1. Strong signal when H2H exists (conference rematches)")
message("  2. Model over-relying on sparse H2H (1-2 games = noisy)")
message("  3. Tree splits: 'if h2h_games>0 then...' creates a sharp decision boundary")
message("Consider: cap H2H weight when h2h_games < 3, or use h2h_games as interaction.")
