library(readr)
library(dplyr)

#' Compute DayZero for one season from schedule file.
#' Returns Date or NA when no valid schedule date is available.
compute_dayzero_from_schedule <- function(season, schedules_dir) {
  season_start <- as.integer(season) - 1L
  season_label <- paste0(season_start, "-", sprintf("%02d", as.integer(season) %% 100))
  schedule_path <- file.path(schedules_dir, paste0(season_label, "_schedule.csv"))
  if (!file.exists(schedule_path)) return(as.Date(NA))

  df <- read_csv(schedule_path, show_col_types = FALSE)
  date_col <- names(df)[tolower(names(df)) == "date"][1]
  if (is.na(date_col)) return(as.Date(NA))

  dates <- suppressWarnings(as.Date(trimws(as.character(df[[date_col]]))))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) return(as.Date(NA))
  min(dates) - 1
}

#' Build MSeasons-style Season/DayZero table.
#'
#' Rules:
#' 1) Prefer schedule-derived DayZero when available.
#' 2) Otherwise keep existing DayZero from current MSeasons.csv (if provided).
#' 3) Otherwise fallback to Season-1-11-01.
build_mseasons_with_dayzero <- function(
    seasons,
    schedules_dir,
    existing_mseasons_path = NULL,
    include_existing_seasons = TRUE) {
  seasons_vec <- as.integer(seasons)

  existing <- tibble(Season = integer(), DayZero = character())
  if (!is.null(existing_mseasons_path) && file.exists(existing_mseasons_path)) {
    existing <- read_csv(existing_mseasons_path, show_col_types = FALSE) %>%
      transmute(
        Season = as.integer(Season),
        DayZero = as.character(DayZero)
      ) %>%
      filter(!is.na(Season), !is.na(DayZero), DayZero != "")
  }

  if (include_existing_seasons && nrow(existing) > 0) {
    seasons_vec <- sort(unique(c(seasons_vec, existing$Season)))
  } else {
    seasons_vec <- sort(unique(seasons_vec))
  }

  rows <- lapply(seasons_vec, function(season) {
    schedule_dayzero <- compute_dayzero_from_schedule(season, schedules_dir)
    if (!is.na(schedule_dayzero)) {
      dayzero <- schedule_dayzero
    } else {
      existing_match <- existing %>% filter(Season == season) %>% slice(1)
      if (nrow(existing_match) == 1) {
        existing_date <- suppressWarnings(as.Date(existing_match$DayZero[[1]]))
      } else {
        existing_date <- as.Date(NA)
      }
      if (!is.na(existing_date)) {
        dayzero <- existing_date
      } else {
        dayzero <- as.Date(sprintf("%d-11-01", season - 1L))
      }
    }
    tibble(Season = as.integer(season), DayZero = format(dayzero, "%Y-%m-%d"))
  })

  bind_rows(rows) %>% arrange(Season)
}
