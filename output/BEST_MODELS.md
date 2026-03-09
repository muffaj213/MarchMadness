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
| Best (log loss)| rand_forest | baseline | 74.6%    | 0.5004   | 2025-03-09 |

---

## Ensemble Results

*Blended predictions from tuned GLM, XGBoost, and Random Forest. Weights optimized to minimize log loss on holdout.*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|---------|
| Ensemble | 73.02%   | 0.5102   | 63      |

**Ensemble Weights**

| Model      | Weight |
|------------|--------|
| glm        | 0.000  |
| xgboost    | 0.000  |
| rand_forest| 1.000  |

*Weights updated 2025-03-09*
