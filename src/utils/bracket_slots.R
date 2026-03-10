# =============================================================================
# bracket_slots.R - Build 68-team bracket slots (64 + First Four play-ins)
# =============================================================================
# Tournament expanded from 64 to 68 teams in 2011. For 2011+, prepends 4 First
# Four play-in slots before R1. Uses raw_historical when available (2011-2016),
# else a standard template (W16, W11, Y11, Z16) for 2017+.
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

FIRST_FOUR_YEAR <- 2011L

# Fallback when first arg is empty/NA
`%||%` <- function(x, y) if (length(x) == 0 || is.na(x[1])) y else x[1]

#' Standard 68-team First Four play-in slots (template when raw_historical unavailable)
#' NCAA format: 2 games for 16-seeds, 2 for 11-seeds. Regions vary by year.
#' Using 2016 structure: W16, W11, Y11, Z16.
FIRST_FOUR_TEMPLATE <- tibble(
  Slot = c("W16", "W11", "Y11", "Z16"),
  Strong = c("W16a", "W11a", "Y11a", "Z16a"),
  Weak = c("W16b", "W11b", "Y11b", "Z16b")
)

#' Build slots for a single season (for use in 02_process_data)
#' @param season Season year
#' @param base_slots 64-team base slots (no Season)
#' @return Slots for that season with First Four if 2011+
get_slots_for_season <- function(season, base_slots) {
  base <- as.data.frame(base_slots)
  nm <- names(base)
  slot_col <- intersect(nm, c("Slot", "slot"))[1] %||% nm[1]
  strong_col <- intersect(nm, c("Strong", "StrongSeed", "Strongseed"))[1] %||% nm[2]
  weak_col <- intersect(nm, c("Weak", "WeakSeed", "Weakseed"))[1] %||% nm[3]
  base <- base %>%
    rename(Slot = !!sym(slot_col), Strong = !!sym(strong_col), Weak = !!sym(weak_col)) %>%
    select(Slot, Strong, Weak)

  if (season < FIRST_FOUR_YEAR) {
    return(base %>% rename(StrongSeed = Strong, WeakSeed = Weak))
  }

  # 68-team: get play-in slots
  hist_path <- file.path(RAW_HIST_DIR, "TourneySlots.csv")
  playin <- NULL
  if (file.exists(hist_path)) {
    hist <- read_csv(hist_path, show_col_types = FALSE)
    hn <- names(hist)
    h_season <- hn[1]
    h_slot <- intersect(hn, c("Slot", "slot"))[1] %||% hn[2]
    h_strong <- intersect(hn, c("Strong", "StrongSeed", "Strongseed"))[1] %||% hn[3]
    h_weak <- intersect(hn, c("Weak", "WeakSeed", "Weakseed"))[1] %||% hn[4]
    playin_rows <- hist %>%
      filter(!!sym(h_season) == season, !grepl("^R[0-9]", !!sym(h_slot)))
    if (nrow(playin_rows) > 0) {
      playin <- playin_rows %>%
        transmute(Slot = !!sym(h_slot), Strong = !!sym(h_strong), Weak = !!sym(h_weak))
    }
  }
  if (is.null(playin) || nrow(playin) == 0) {
    playin <- FIRST_FOUR_TEMPLATE
  }

  bind_rows(playin, base) %>%
    rename(StrongSeed = Strong, WeakSeed = Weak)
}
