library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
source(here("src", "utils", "bracket_utils.R"))

ref <- read_csv(here("data", "bracket", "bracket_reference_2026.csv"), show_col_types = FALSE)
teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)

manual_overrides <- c(
  "UConn" = 1135L,
  "NC State" = 1037L,
  "Miami (FL)" = 977L,
  "McNeese" = 1116L,
  "Hofstra" = 1220L,
  "Idaho" = 1225L,
  "Santa Clara" = 1365L,
  "Miami (Ohio)" = 1275L,
  "Tennessee St." = 1398L,
  "Prairie View A&M" = 1341L,
  "Long Island" = 1254L,
  # Not present in current team masters; assign new IDs for 2026 field
  "Cal Baptist" = 1148L,
  "Queens (N.C.)" = 1149L
)

ids <- resolve_team_names_to_ids(ref$TeamName, teams, season = 2026L)
for (nm in names(manual_overrides)) {
  idx <- which(ref$TeamName == nm)
  if (length(idx) > 0) {
    ids[idx] <- as.integer(manual_overrides[[nm]])
  }
}

out <- ref %>%
  transmute(
    Season = 2026L,
    Seed = Seed,
    TeamID = as.integer(ids)
  )

if (any(is.na(out$TeamID))) {
  unresolved <- ref$TeamName[is.na(out$TeamID)]
  stop(
    "Unresolved team names in bracket_reference_2026.csv: ",
    paste(unique(unresolved), collapse = ", ")
  )
}

write_csv(out, here("data", "bracket", "seeds_68team_2026.csv"))
message("Wrote data/bracket/seeds_68team_2026.csv with ", nrow(out), " rows.")
