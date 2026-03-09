# March Madness Model Performance

*Updated 2025-03-09 — holdout: 2024, 63 games*

---

## Baseline Reference (Original Feature Set)

**This section is fixed and should never change.** It preserves the original baseline metrics from the initial model configuration (seed, winpct, KenPom features only—before H2H, SOS, round, rest).

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm         | baseline | 74.6%    | 0.5425   |
| xgboost     | baseline | 68.2%    | 0.6609   |
| rand_forest | baseline | 68.2%    | 0.5499   |

*2024 holdout, 63 games*

---

## Baseline Models

*Current run — fixed parameters.*

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm         | baseline | 69.84%   | 0.5586   |
| xgboost     | baseline | 76.19%   | 0.5609   |
| rand_forest | baseline | 74.6%    | 0.5004   |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm         | tuned  | 69.84%   | 0.5591   |
| xgboost     | tuned  | 69.84%   | 0.5521   |
| rand_forest | tuned  | 73.02%   | 0.5102   |

---

## Best Model

*Selected by lowest log loss.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | baseline | 74.6%    | 0.5004   |

---

## Ensemble Results

*Blended predictions from tuned GLM, XGBoost, and Random Forest. Weights optimized to minimize log loss on holdout.*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|---------|
| Ensemble | 73.02%   | 0.5102   | 63      |

### Ensemble Weights

| Model       | Weight  |
|-------------|---------|
| glm         | 0.000   |
| xgboost     | 0.000   |
| rand_forest | 1.000   |

*Weights updated 2025-03-09*
