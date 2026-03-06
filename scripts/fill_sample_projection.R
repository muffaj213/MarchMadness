# Fill bracket_projection_2025.xlsx with 2024 seeds (for testing 05_run_projected_bracket.R)
library(here)
library(readr)
library(dplyr)
library(writexl)
library(readxl)

seeds <- read_csv(here("data", "processed", "tourney_seeds.csv"), show_col_types = FALSE)
teams <- read_csv(here("data", "processed", "teams.csv"), show_col_types = FALSE)
s24 <- seeds %>% filter(Season == 2024) %>% left_join(teams, by = "TeamID")
proj_template <- read_excel(here("data", "bracket", "bracket_projection_TEMPLATE.xlsx"), sheet = "Seeds")
proj_filled <- proj_template %>%
  left_join(s24 %>% select(Seed, TeamName), by = "Seed") %>%
  mutate(TeamName = if_else(is.na(TeamName.y), TeamName.x, TeamName.y)) %>%
  select(Seed, TeamName, Notes)
inst <- read_excel(here("data", "bracket", "bracket_projection_TEMPLATE.xlsx"), sheet = "Instructions")
write_xlsx(list(Seeds = proj_filled, Instructions = inst), here("data", "bracket", "bracket_projection_2025.xlsx"))
message("Filled bracket_projection_2025.xlsx with 2024 seeds for testing")
