# Bracket Data (68-team seeds, projections)

## 68-Team Seeds (First Four)

After **Selection Sunday**, create 68-team seeds to simulate the First Four play-in games:

### Option 1: Use the template script

```bash
Rscript scripts/create_68team_seeds.R 2026
```

This creates `seeds_68team_2026.csv` with placeholder rows for W16a/b, W11a/b, X11a/b, Z16a/b. Edit the CSV and fill in the 8 TeamIDs for the play-in teams (use `data/processed/teams.csv` for TeamID lookup).

### Option 2: Provide play-in TeamIDs directly

```bash
Rscript scripts/create_68team_seeds.R 2026 <W16a> <W16b> <W11a> <W11b> <X11a> <X11b> <Z16a> <Z16b>
# Order: W16a, W16b, W11a, W11b, X11a, X11b, Z16a, Z16b
```

### Run prediction with 68-team seeds

From R:

```r
source("src/04_predict_bracket.R")
main(season = 2026, seeds_file = "data/bracket/seeds_68team_2026.csv")
```

Or edit `src/04_predict_bracket.R` and set at the bottom:

```r
main(season = 2026L, seeds_file = here("data", "bracket", "seeds_68team_2026.csv"))
```

## Template structure

The 68-team format replaces 4 seeds with 8 play-in seeds:
- **W16** → W16a, W16b (16-seed play-in, Midwest)
- **W11** → W11a, W11b (11-seed play-in, Midwest)
- **X11** → X11a, X11b (11-seed play-in, West)
- **Z16** → Z16a, Z16b (16-seed play-in, South)

Regions may vary by year; the bracket slots use this template for 2017+.
