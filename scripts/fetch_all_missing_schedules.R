# =============================================================================
# fetch_all_missing_schedules.R - Fetch all missing schedule years via cbbdata
# =============================================================================
# Fills gaps: 2008-2013 (0% H2H) and refreshes 2025, 2026 for complete H2H.
# Requires: cbbdata + account (run scripts/setup_cbbdata.R once).
# Run: Rscript scripts/fetch_all_missing_schedules.R
# =============================================================================

library(here)

source(here("src", "config.R"))

# Years to fetch: 2008-2013 (gap), 2025-2026 (refresh/ensure)
YEARS_TO_FETCH <- c(2008L, 2009L, 2010L, 2011L, 2012L, 2013L, 2025L, 2026L)

main <- function() {
  if (!requireNamespace("cbbdata", quietly = TRUE)) {
    stop("Install cbbdata: devtools::install_github('andreweatherman/cbbdata')")
  }

  dir.create(SCHEDULES_DIR, showWarnings = FALSE, recursive = TRUE)
  message("Fetching missing schedules via cbbdata (", length(YEARS_TO_FETCH), " years)...")
  message("Years: ", paste(YEARS_TO_FETCH, collapse = ", "))

  for (year in YEARS_TO_FETCH) {
    sched_label <- paste0(year - 1L, "-", sprintf("%02d", year %% 100))
    out_path <- file.path(SCHEDULES_DIR, paste0(sched_label, "_schedule.csv"))
    message("\n--- ", year, " (", sched_label, ") ---")
    tryCatch(
      {
        Sys.setenv(CBD_FETCH_YEAR = as.character(year))
        source(here("scripts", "fetch_schedule_cbbdata.R"), local = new.env())
        if (file.exists(out_path)) {
          message("  OK: ", out_path)
        } else {
          message("  WARNING: File not created")
        }
      },
      error = function(e) {
        message("  FAILED: ", conditionMessage(e))
      }
    )
  }

  message("\n=== Done. Run 01c_convert_schedules_to_regular.R, then 02_process_data.R, then analyze_h2h_dominance.R ===")
}

main()
