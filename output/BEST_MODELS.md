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
| glm | baseline | 71.96 ± 5.10% | 0.5471 ± 0.0431 |
| xgboost | baseline | 68.25 ± 6.92% | 0.6431 ± 0.0848 |
| rand_forest | baseline | 73.02 ± 2.75% | 0.5478 ± 0.0322 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 68.78 ± 1.83% | 0.5848 ± 0.0165 |
| xgboost | tuned | 70.37 ± 2.42% | 0.5614 ± 0.0338 |
| rand_forest | tuned | 72.49 ± 2.42% | 0.5454 ± 0.0390 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | tuned | 72.49 ± 2.42% | 0.5454 ± 0.0390 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on last CV fold (2021); probabilities calibrated when beneficial (Platt).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 68.25 ± 6.92% | 0.6431 ± 0.0848 | 189 |

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

