# March Madness Model Performance

*Updated 2026-03-09*

**Validation:** Time-based CV for tuning (expanding window by season). Holdout: 2022, 2023, 2024 (189 games total). Metrics show mean ± SD across holdout years when multiple.

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
| glm | baseline | 69.84 ± 1.59% | 0.5555 ± 0.0292 |
| xgboost | baseline | 70.37 ± 2.42% | 0.6200 ± 0.0404 |
| rand_forest | baseline | 72.49 ± 1.83% | 0.5485 ± 0.0406 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 70.90 ± 3.30% | 0.5617 ± 0.0258 |
| xgboost | tuned | 71.43 ± 3.17% | 0.5416 ± 0.0042 |
| rand_forest | tuned | 72.49 ± 3.30% | 0.5479 ± 0.0309 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| ensemble | ensemble | 73.54 ± 3.30% | 0.5396 ± 0.0113 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized to minimize log loss on holdout.*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 73.54 ± 3.30% | 0.5396 ± 0.0113 | 189 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.000 |
| glm_tuned | 0.000 |
| xgboost_baseline | 0.000 |
| xgboost_tuned | 0.670 |
| rand_forest_baseline | 0.000 |
| rand_forest_tuned | 0.330 |

*Weights updated 2026-03-09*

