# MarchMadness

NCAA Men's Basketball Tournament bracket prediction model in R.

## Setup

1. Install R packages: `source("install.R")`
2. Download data from [Kaggle NCAA Basketball](https://www.kaggle.com/datasets/ncaa/ncaa-basketball) into `data/raw/`
3. Run full pipeline: `source("run_all.R")`

## Pipeline

- `src/01_download_data.R` - Download from Kaggle
- `src/02_process_data.R` - Process and engineer features
- `src/03_train_model.R` - Train logistic regression, validate
- `src/04_predict_bracket.R` - Simulate bracket, output predictions
