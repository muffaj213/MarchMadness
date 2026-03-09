# Bracket Data (68-team seeds, projections)

## 68-Team Seeds (First Four)

After **Selection Sunday**, create 68-team seeds to simulate the First Four play-in games:

### Option 1: Use the template script

```bash
Rscript scripts/create_68team_seeds.R 2025
```

This creates `seeds_68team_2025.csv` with placeholder rows for W16a/b, W11a/b, Y11a/b, Z16a/b. Edit the CSV and fill in the 8 TeamIDs for the play-in teams (use `data/processed/teams.csv` for TeamID lookup).

### Option 2: Provide play-in TeamIDs directly

```bash
Rscript scripts/create_68team_seeds.R 2025 1106 1427 1102 1037 1106 1052 1058 1049
# Order: W16a, W16b, W11a, W11b, Y11a, Y11b, Z16a, Z16b
```

### Run prediction with 68-team seeds

From R:

```r
source("src/04_predict_bracket.R")
main(season = 2025, seeds_file = "data/bracket/seeds_68team_2025.csv")
```

Or edit `src/04_predict_bracket.R` and set at the bottom:

```r
main(season = 2025L, seeds_file = here("data", "bracket", "seeds_68team_2025.csv"))
```

## Template structure

The 68-team format replaces 4 seeds with 8 play-in seeds:
- **W16** → W16a, W16b (16-seed play-in)
- **W11** → W11a, W11b (11-seed play-in)  
- **Y11** → Y11a, Y11b (11-seed play-in)
- **Z16** → Z16a, Z16b (16-seed play-in)

Regions may vary by year; the bracket slots use this template for 2017+.
