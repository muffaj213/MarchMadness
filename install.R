# =============================================================================
# install.R - Install required R packages for NCAA Bracket Prediction
# =============================================================================
# Run: source("install.R") or Rscript install.R
# =============================================================================

packages <- c(
  "tidyverse",   # dplyr, tidyr, readr, purrr
  "tidymodels",  # modeling workflow
  "glmnet",      # regularized regression (optional alternative)
  "here",        # project paths
  "readxl",      # read Excel for projected bracket template
  "xgboost",     # gradient boosting for xgboost model
  "ranger",      # random forest for rand_forest model
  "toRvik"       # Barttorvik ratings for KenPom gap (2018-2023)
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (pkg == "toRvik") {
      # toRvik may not be on CRAN for all R versions; try GitHub
      if (requireNamespace("devtools", quietly = TRUE)) {
        tryCatch(
          devtools::install_github("andreweatherman/toRvik"),
          error = function(e) message("toRvik: install with devtools::install_github('andreweatherman/toRvik')")
        )
      } else {
        message("toRvik: install devtools, then devtools::install_github('andreweatherman/toRvik')")
      }
    } else {
      install.packages(pkg, repos = "https://cloud.r-project.org/")
    }
  }
}

message("Required packages installed. Load with library(tidyverse); library(tidymodels); library(here)")
