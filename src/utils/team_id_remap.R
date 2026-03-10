# =============================================================================
# team_id_remap.R - Map between canonical (raw_historical) and Kaggle TeamIDs
# =============================================================================
# Uses data/processed/team_id_master.csv. Run build_canonical_team_master.R first.
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

#' Load team ID master (canonical <-> Kaggle)
load_team_id_master <- function(path = TEAM_ID_MASTER_PATH) {
  if (!file.exists(path)) {
    stop("team_id_master.csv not found. Run: Rscript scripts/build_canonical_team_master.R")
  }
  read_csv(path, show_col_types = FALSE)
}

#' Remap Kaggle TeamID to canonical Team_Id
#' @param ids Integer vector of Kaggle TeamIDs
#' @return Integer vector of canonical Team_Ids; NA where no mapping
remap_kaggle_to_canonical <- function(ids) {
  master <- load_team_id_master()
  lookup <- setNames(master$Canonical_Team_Id, master$Kaggle_TeamID)
  as.integer(lookup[as.character(ids)])
}

#' Remap canonical Team_Id to Kaggle TeamID
#' @param ids Integer vector of canonical Team_Ids
#' @return Integer vector of Kaggle TeamIDs; NA where no mapping
remap_canonical_to_kaggle <- function(ids) {
  master <- load_team_id_master()
  lookup <- setNames(master$Kaggle_TeamID, master$Canonical_Team_Id)
  as.integer(lookup[as.character(ids)])
}
