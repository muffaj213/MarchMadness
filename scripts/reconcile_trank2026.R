library(readr)
library(dplyr)

in_path <- here::here("data", "raw", "trank2026_data.csv")
bt_path <- here::here("data", "raw_nishaa", "KenPom Barttorvik.csv")

if (!file.exists(in_path)) stop("Missing input: ", in_path)
if (!file.exists(bt_path)) stop("Missing target: ", bt_path)

raw <- read_csv(in_path, col_names = FALSE, show_col_types = FALSE) %>%
  filter(!is.na(X1), trimws(as.character(X1)) != "")

bt <- read_csv(bt_path, show_col_types = FALSE)

mk <- raw %>%
  transmute(
    YEAR = suppressWarnings(as.integer(X31)),
    TEAM = trimws(as.character(X1)),
    `KADJ O` = suppressWarnings(as.numeric(X2)),
    `KADJ D` = suppressWarnings(as.numeric(X3)),
    BARTHAG = suppressWarnings(as.numeric(X4)),
    W = suppressWarnings(as.numeric(X6)),
    GAMES = suppressWarnings(as.numeric(X7)),
    `EFG%` = suppressWarnings(as.numeric(X8)),
    `EFG%D` = suppressWarnings(as.numeric(X9)),
    FTR = suppressWarnings(as.numeric(X10)),
    FTRD = suppressWarnings(as.numeric(X11)),
    `TOV%` = suppressWarnings(as.numeric(X12)),
    `TOV%D` = suppressWarnings(as.numeric(X13)),
    `OREB%` = suppressWarnings(as.numeric(X14)),
    `DREB%` = suppressWarnings(as.numeric(X15)),
    `KADJ T` = suppressWarnings(as.numeric(X16)),
    `2PT%` = suppressWarnings(as.numeric(X17)),
    `2PT%D` = suppressWarnings(as.numeric(X18)),
    `3PT%` = suppressWarnings(as.numeric(X19)),
    `3PT%D` = suppressWarnings(as.numeric(X20)),
    `BLK%` = suppressWarnings(as.numeric(X21)),
    `BLKED%` = suppressWarnings(as.numeric(X22)),
    `AST%` = suppressWarnings(as.numeric(X23)),
    `OP AST%` = suppressWarnings(as.numeric(X24)),
    `2PTR` = suppressWarnings(as.numeric(X25)),
    `3PTR` = suppressWarnings(as.numeric(X26)),
    `RAW T` = suppressWarnings(as.numeric(X27)),
    `ELITE SOS` = suppressWarnings(as.numeric(X35)),
    `FT%` = suppressWarnings(as.numeric(X36))
  ) %>%
  mutate(
    L = if_else(!is.na(GAMES) & !is.na(W), pmax(0, GAMES - W), NA_real_),
    `WIN%` = if_else(!is.na(GAMES) & GAMES > 0 & !is.na(W), 100 * W / GAMES, NA_real_),
    `KADJ EM` = `KADJ O` - `KADJ D`,
    `BADJ O` = `KADJ O`,
    `BADJ D` = `KADJ D`,
    `BADJ EM` = `KADJ EM`,
    `BADJ T` = `KADJ T`,
    `OP OREB%` = if_else(!is.na(`DREB%`), 100 - `DREB%`, NA_real_),
    `OP DREB%` = if_else(!is.na(`OREB%`), 100 - `OREB%`, NA_real_)
  ) %>%
  filter(YEAR == 2026)

# Ensure all target columns exist and order matches existing file.
for (nm in names(bt)) {
  if (!nm %in% names(mk)) mk[[nm]] <- NA
}
mk <- mk %>% select(all_of(names(bt)))

out <- bt %>%
  filter(YEAR != 2026) %>%
  bind_rows(mk) %>%
  arrange(YEAR, TEAM)

write_csv(out, bt_path, na = "")

message("Mapped rows appended: ", nrow(mk))
message("Total 2026 rows now: ", sum(out$YEAR == 2026, na.rm = TRUE))
message("Total rows in file: ", nrow(out))
