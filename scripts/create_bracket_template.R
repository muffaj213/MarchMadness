# =============================================================================
# create_bracket_template.R - Generate bracket_projection_TEMPLATE.xlsx
# =============================================================================
# Run once to create the Excel template. Uses writexl.
# =============================================================================

library(writexl)
library(here)

source(here("src", "config.R"))
dir.create(BRACKET_DIR, showWarnings = FALSE, recursive = TRUE)

# Seeds: W01-W16, X01-X16, Y01-Y16, Z01-Z16 (64 rows)
regions <- c("W", "X", "Y", "Z")
seeds_df <- do.call(rbind, lapply(regions, function(r) {
  data.frame(
    Seed = paste0(r, sprintf("%02d", 1:16)),
    TeamName = "",
    Notes = "",
    stringsAsFactors = FALSE
  )
}))

# Instructions sheet
instructions_df <- data.frame(
  Section = c(
    "Where to get projections",
    "",
    "Region / seed notation",
    "",
    "How to use",
    "",
    ""
  ),
  Content = c(
    "ESPN: espn.com -> Search 'Bracketology' -> view full bracket",
    "KenPom: kenpom.com -> ratings; Substack/KenPom for bracket projections",
    "W = East, X = South, Y = Midwest, Z = West (match ESPN regions)",
    "W01 = 1-seed East, W16 = 16-seed East, etc.",
    "1. Copy this file to bracket_projection_YYYY.xlsx",
    "2. Fill Seed and TeamName for each slot (use exact names from teams.csv)",
    "3. Run: source('src/05_run_projected_bracket.R')"
  ),
  stringsAsFactors = FALSE
)

sheets <- list(Seeds = seeds_df, Instructions = instructions_df)
out_path <- file.path(BRACKET_DIR, "bracket_projection_TEMPLATE.xlsx")
write_xlsx(sheets, out_path)
message("Created: ", out_path)
