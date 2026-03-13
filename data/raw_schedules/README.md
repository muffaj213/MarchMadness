# Schedule files for regular-season game results

Each file `YYYY-(YY+1)_schedule.csv` corresponds to season `YY+1` (e.g. `2024-25_schedule.csv` = 2025 season).

## Required columns

| Column | Description |
|--------|-------------|
| Date | Game date (YYYY-MM-DD) |
| Team | Team name (must resolve via team_aliases → MTeams) |
| Opp | Opponent name |
| Points_For | Points scored by Team |
| Points_Against | Points allowed |
| Win | 1 = win, 0 = loss |

Optional: `Type`, `Conf.`, `Venue` (used by 01c if present).

## Adding 2025 season data

**Automated (when available):**
```bash
Rscript scripts/fetch_2025_schedule.R
```

**Manual:** If the toRvik API doesn't have 2025 data yet, add `2024-25_schedule.csv` manually:

1. **Barttorvik T-Rank:** Visit [barttorvik.com](https://barttorvik.com), use team schedule exports or game finder.
2. **Sports Reference:** [College Basketball Scoreboard](https://www.sports-reference.com/cbb/boxscores/) — export game results.
3. Ensure team names match `data/processed/team_aliases.csv` conventions (e.g. "College of Charleston" not "Charleston").

Then run:
```bash
Rscript src/01c_convert_schedules_to_regular.R
Rscript src/02_process_data.R
```
