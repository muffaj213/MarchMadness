# March Madness Model Performance

## Baseline Reference (Original Feature Set)

**This section is fixed and should never change.** It preserves the original baseline metrics from the initial model configuration (seed, winpct, KenPom features only—before H2H, SOS, round, rest).

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm         | baseline | 74.6%    | 0.5425   |
| xgboost     | baseline | 68.2%    | 0.6609   |
| rand_forest | baseline | 68.2%    | 0.5499   |

*2024 holdout, 63 games*

---

## Best Model Performance

*Updated when training produces a model with lower log loss than the current best.*

| Metric         | Model       | Config | Accuracy | Log Loss | Updated   |
|----------------|-------------|--------|----------|----------|-----------|
| Best (log loss)| rand_forest | tuned  | 71.43%   | 0.559    | 2025-03-02 |
