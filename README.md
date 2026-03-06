# MarchMadness

NCAA Men's Basketball Tournament bracket prediction model in R.

## Setup

1. Install R packages: `source("install.R")`
2. Download data from [Kaggle NCAA Basketball](https://www.kaggle.com/datasets/ncaa/ncaa-basketball) into `data/raw/`
3. KenPom data is auto-downloaded to `data/raw_kenpom/` on first run (or place `kenpom.csv` there manually)
4. Run full pipeline: `source("run_all.R")`

## Pipeline

- `src/01_download_data.R` - Download from Kaggle
- `src/02_process_data.R` - Process data, load KenPom, engineer features
- `src/03_train_model.R` - Train logistic regression (with KenPom features), validate
- `src/04_predict_bracket.R` - Simulate bracket, output predictions

## Features

Model uses: seed differential, win percentage, points margin, and **KenPom** metrics (AdjEM, AdjO, AdjD, tempo). KenPom data from:
- [jfinocchiaro/marchmadness](https://github.com/jfinocchiaro/marchmadness) (2002-2017)
- toRvik/Barttorvik (2018-2023) — run `Rscript scripts/fetch_kenpom_gap.R` to fill the gap
- Barttorvik (2024-2025 when available)
