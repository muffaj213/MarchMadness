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
| rand_forest | baseline | 73.02 ± 1.59% | 0.5435 ± 0.0346 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 68.78 ± 1.83% | 0.5854 ± 0.0174 |
| xgboost | tuned | 68.78 ± 4.85% | 0.5518 ± 0.0149 |
| rand_forest | tuned | 73.02 ± 3.17% | 0.5460 ± 0.0370 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | baseline | 73.02 ± 1.59% | 0.5435 ± 0.0346 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on last CV fold (2021); probabilities calibrated when beneficial (Platt).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 70.37 ± 2.42% | 0.6200 ± 0.0404 | 189 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.000 |
| glm_tuned | 0.000 |
| xgboost_baseline | 1.000 |
| xgboost_tuned | 0.000 |
| rand_forest_baseline | 0.000 |
| rand_forest_tuned | 0.000 |

*Weights updated 2026-03-09*

