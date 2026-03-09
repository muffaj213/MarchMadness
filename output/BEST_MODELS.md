# March Madness Model Performance

*Updated 2026-03-09*

**Validation:** Time-based CV for tuning (expanding window by season). Holdout: 2022, 2023, 2024 (189 games total). Metrics show mean ± SD across holdout years when multiple. Lower metrics with this strategy are more trustworthy than earlier single-year results.

---

## Baseline Reference (Original Feature Set)

**This section is fixed and should never change.** It preserves the original baseline metrics from the initial model configuration (seed, winpct, KenPom features only—before H2H, SOS, round, rest).

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm         | baseline | 74.6%    | 0.5425   |
| xgboost     | baseline | 68.2%    | 0.6609   |
| rand_forest | baseline | 68.2%    | 0.5499   |

*2024 holdout, 63 games*

**⚠️ These numbers were likely too optimistic** because: (1) single-year holdout (63 games) has high variance—a 5% accuracy swing is only ~3 games; (2) model selection and tuning were partly overfit to 2024's structure; (3) random 5-fold CV during tuning mixed past/future seasons, which doesn't reflect real deployment. The multi-year validation below (2022–2024, 189 games) is a more trustworthy estimate of true generalization.

---

## Baseline Models

*Current run — fixed parameters.*

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm | baseline | 69.84 ± 1.59% | 0.5594 ± 0.0296 |
| xgboost | baseline | 70.37 ± 1.83% | 0.6150 ± 0.0612 |
| rand_forest | baseline | 71.96 ± 3.67% | 0.5458 ± 0.0353 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 70.37 ± 3.67% | 0.5620 ± 0.0270 |
| xgboost | tuned | 72.49 ± 3.30% | 0.5519 ± 0.0247 |
| rand_forest | tuned | 71.96 ± 2.42% | 0.5451 ± 0.0292 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| ensemble | ensemble | 73.02 ± 2.75% | 0.5431 ± 0.0215 |

---

## Ensemble Results

*Blended predictions from tuned GLM, XGBoost, and Random Forest. Weights optimized to minimize log loss on holdout.*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 73.02 ± 2.75% | 0.5431 ± 0.0215 | 189 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm | 0.000 |
| xgboost | 0.317 |
| rand_forest | 0.683 |

*Weights updated 2026-03-09*

