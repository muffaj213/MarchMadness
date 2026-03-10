# =============================================================================
# bracket_slots.R - Build 68-team bracket slots (64 + First Four play-ins)
# =============================================================================
# Uses raw_historical/TourneySlots.csv when the season exists there - this has
# the correct NCAA R2-R6 pairings (1v8, 2v7, 3v6, 4v5 per region). MNCAATourneySlots
# has wrong R2 pairings (1v2, 3v4, etc.) and is used only as fallback for seasons
# not in raw_historical (with a correct bracket template).
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

FIRST_FOUR_YEAR <- 2011L

# Fallback when first arg is empty/NA
`%||%` <- function(x, y) if (length(x) == 0 || is.na(x[1])) y else x[1]

#' Standard 68-team First Four play-in slots (template when raw_historical unavailable)
FIRST_FOUR_TEMPLATE <- tibble(
  Slot = c("W16", "W11", "Y11", "Z16"),
  Strong = c("W16a", "W11a", "Y11a", "Z16a"),
  Weak = c("W16b", "W11b", "Y11b", "Z16b")
)

#' Correct 64-team bracket (R1-R6) with NCAA pairings: R2 = 1v8, 2v7, 3v6, 4v5 per region.
#' Used when raw_historical lacks the season (e.g. 2017+).
CORRECT_BRACKET_BASE <- tibble(
  Slot = c(
    "R1W1", "R1W2", "R1W3", "R1W4", "R1W5", "R1W6", "R1W7", "R1W8",
    "R1X1", "R1X2", "R1X3", "R1X4", "R1X5", "R1X6", "R1X7", "R1X8",
    "R1Y1", "R1Y2", "R1Y3", "R1Y4", "R1Y5", "R1Y6", "R1Y7", "R1Y8",
    "R1Z1", "R1Z2", "R1Z3", "R1Z4", "R1Z5", "R1Z6", "R1Z7", "R1Z8",
    "R2W1", "R2W2", "R2W3", "R2W4", "R2X1", "R2X2", "R2X3", "R2X4",
    "R2Y1", "R2Y2", "R2Y3", "R2Y4", "R2Z1", "R2Z2", "R2Z3", "R2Z4",
    "R3W1", "R3W2", "R3X1", "R3X2", "R3Y1", "R3Y2", "R3Z1", "R3Z2",
    "R4W1", "R4X1", "R4Y1", "R4Z1", "R5WX", "R5YZ", "R6CH"
  ),
  Strong = c(
    "W01", "W02", "W03", "W04", "W05", "W06", "W07", "W08",
    "X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08",
    "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08",
    "Z01", "Z02", "Z03", "Z04", "Z05", "Z06", "Z07", "Z08",
    "R1W1", "R1W2", "R1W3", "R1W4", "R1X1", "R1X2", "R1X3", "R1X4",
    "R1Y1", "R1Y2", "R1Y3", "R1Y4", "R1Z1", "R1Z2", "R1Z3", "R1Z4",
    "R2W1", "R2W2", "R2X1", "R2X2", "R2Y1", "R2Y2", "R2Z1", "R2Z2",
    "R3W1", "R3X1", "R3Y1", "R3Z1", "R4W1", "R4Y1", "R5WX"
  ),
  Weak = c(
    "W16", "W15", "W14", "W13", "W12", "W11", "W10", "W09",
    "X16", "X15", "X14", "X13", "X12", "X11", "X10", "X09",
    "Y16", "Y15", "Y14", "Y13", "Y12", "Y11", "Y10", "Y09",
    "Z16", "Z15", "Z14", "Z13", "Z12", "Z11", "Z10", "Z09",
    "R1W8", "R1W7", "R1W6", "R1W5", "R1X8", "R1X7", "R1X6", "R1X5",
    "R1Y8", "R1Y7", "R1Y6", "R1Y5", "R1Z8", "R1Z7", "R1Z6", "R1Z5",
    "R2W4", "R2W3", "R2X4", "R2X3", "R2Y4", "R2Y3", "R2Z4", "R2Z3",
    "R3W2", "R3X2", "R3Y2", "R3Z2", "R4X1", "R4Z1", "R5YZ"
  )
)

#' Build slots for a single season (for use in 02_process_data)
#' @param season Season year
#' @param base_slots 64-team base slots from MNCAATourneySlots (used only when raw_historical lacks this season)
#' @return Slots for that season with correct R2-R6 NCAA pairings
get_slots_for_season <- function(season, base_slots) {
  hist_path <- file.path(RAW_HIST_DIR, "TourneySlots.csv")
  if (file.exists(hist_path)) {
    hist <- read_csv(hist_path, show_col_types = FALSE)
    hn <- names(hist)
    h_season <- hn[1]
    h_slot <- intersect(hn, c("Slot", "slot"))[1] %||% hn[2]
    h_strong <- intersect(hn, c("Strong", "StrongSeed", "Strongseed"))[1] %||% hn[3]
    h_weak <- intersect(hn, c("Weak", "WeakSeed", "Weakseed"))[1] %||% hn[4]
    hist_rows <- hist %>% filter(!!sym(h_season) == season)
    if (nrow(hist_rows) > 0) {
      return(hist_rows %>%
        transmute(Slot = !!sym(h_slot), StrongSeed = !!sym(h_strong), WeakSeed = !!sym(h_weak)))
    }
  }

  # Fallback: use correct bracket (raw_historical doesn't have this season)
  base <- if (season < FIRST_FOUR_YEAR) {
    CORRECT_BRACKET_BASE
  } else {
    bind_rows(FIRST_FOUR_TEMPLATE, CORRECT_BRACKET_BASE)
  }
  base %>%
    rename(StrongSeed = Strong, WeakSeed = Weak) %>%
    select(Slot, StrongSeed, WeakSeed)
}
