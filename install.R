# =============================================================================
# install.R - Install required R packages for NCAA Bracket Prediction
# =============================================================================
# Run: source("install.R") or Rscript install.R
# =============================================================================

packages <- c(
  "tidyverse",   # dplyr, tidyr, readr, purrr
  "tidymodels",  # modeling workflow
  "glmnet",      # regularized regression (optional alternative)
  "here"         # project paths
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}

message("Required packages installed. Load with library(tidyverse); library(tidymodels); library(here)")
