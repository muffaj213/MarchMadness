# MarchMadness

NCAA Men's Basketball Tournament bracket prediction model in R.

## Setup

1. Install R packages: `source("install.R")`
2. Download data from [Kaggle NCAA Basketball](https://www.kaggle.com/datasets/ncaa/ncaa-basketball) into `data/raw/`
3. KenPom data is auto-downloaded to `data/raw_kenpom/` on first run (or place `kenpom.csv` there manually)
4. Run full pipeline: `source("run_all.R")`
5. Skip prediction (process + train only): `RUN_PREDICTION=FALSE Rscript run_all.R`

## Pipeline

- `src/01_download_data.R` - Download from Kaggle
- `src/02_process_data.R` - Process data, load KenPom, engineer features
- `src/03_train_model.R` - Train logistic regression (with KenPom features), validate
- `src/04_predict_bracket.R` - Simulate bracket, output predictions

## 68-Team Bracket (First Four)

The tournament expanded from 64 to 68 teams in **2011**. The simulation supports First Four play-in games:

- **Slots**: For 2011+, 4 First Four slots (e.g. W16a vs W16b) are prepended before R1. Uses `data/raw_historical/TourneySlots.csv` when available (2011–2016); otherwise a standard template (W16, W11, Y11, Z16).
- **Seeds**: With **68-team seeds**, First Four games are simulated and appear in `game_results` with `round=0` for bracket pool points.
- **2011–2016**: Run `Rscript scripts/build_canonical_team_master.R` first, then `02_process_data.R`. When `team_id_master.csv` exists, 68-team seeds from `raw_historical` are merged (with hybrid fallback for unmapped teams).
- **2025+**: Run `Rscript scripts/create_68team_seeds.R 2025` after Selection Sunday, edit the CSV to fill play-in TeamIDs, then:
  ```r
  source("src/04_predict_bracket.R")
  main(season = 2025, seeds_file = "data/bracket/seeds_68team_2025.csv")
  ```
- See `data/bracket/README.md` for full instructions.

## Features

Model uses: seed differential, win percentage, points margin, and **KenPom** metrics (AdjEM, AdjO, AdjD, tempo). KenPom data from:
- [jfinocchiaro/marchmadness](https://github.com/jfinocchiaro/marchmadness) (2002-2017)
- toRvik/Barttorvik (2018-2023) — run `Rscript scripts/fetch_kenpom_gap.R` to fill the gap
- Barttorvik (2024-2025 when available)
