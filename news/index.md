# Changelog

## optic (development version)

## [optic 1.2.8](https://github.com/RANDCorporation/optic/releases/tag/v1.2.8)

### Improvements

- The power analysis vignette now includes an “Examining other metrics
  of performance” section showing bias, simulation variance, model-based
  variance, relative RMSE, and coverage curves alongside the existing
  power curves, and the summary chunk now uses
  [`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
  directly to demonstrate the full set of metrics it returns. Bias and
  RMSE are reported as a percentage of the outcome mean.
- [`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
  now falls back to an uncorrected 95% CI when no t-statistics are
  available, so coverage and `correct_rejection_rate` are populated for
  methods like ASCM and CSA whose default inference does not produce a
  t-statistic.

### Bug fixes

- [`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
  [`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
  and
  [`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md)
  now return `NA_real_` (rather than `numeric(0)`) when no test
  statistics are available, replacing a confusing zero-length result
  with an explicit missing value.

## [optic 1.2.7](https://github.com/RANDCorporation/optic/releases/tag/v1.2.7)

### Internal changes

- Updated the autoeffect result extractors
  (`.extract_results_autoeffect` and `.se_adjust_cluster_autoeffect`) to
  read the renamed tidymodels-style columns from
  [`autoeffect::cumulative_effects()`](https://rdrr.io/pkg/autoeffect/man/cumulative_effects.html)
  (`lag`, `estimate`, `std.error`, `statistic`, `p.value`). Requires
  autoeffect \>= 0.2.11.
  ([\#43](https://github.com/RANDCorporation/optic/pull/43))

## [optic 1.2.6](https://github.com/RANDCorporation/optic/releases/tag/v1.2.6)

### Bug fixes

- [`optic_simulation()`](https://randcorporation.github.io/optic/reference/optic_simulation.md)
  now stops early with an informative message when the input data
  contains NAs in any required column: `unit_var`, `time_var`, the
  outcome (formula LHS) of any model, or any covariate (formula RHS
  minus treatment-construct terms) referenced by any model. Treatment
  columns are excluded because they are simulated by optic. This
  replaces a class of cryptic downstream errors (e.g. autoeffect’s
  “spec_strategy out of sync” message) with a single upfront error that
  names the offending columns.
  ([\#41](https://github.com/RANDCorporation/optic/pull/41))

## [optic 1.2.5](https://github.com/RANDCorporation/optic/releases/tag/v1.2.5)

### New features

- Added a power analysis vignette demonstrating simulation-based power
  analysis for TWFE, debiased AR, ASCM, and CSA estimators using the
  `overdoses` dataset. Adds `augsynth` and `ggplot2` to `Suggests`.
  ([\#40](https://github.com/RANDCorporation/optic/pull/40))

## [optic 1.2.4](https://github.com/RANDCorporation/optic/releases/tag/v1.2.4)

### New features

- Added simulation performance statistics functions:
  [`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
  [`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
  [`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
  [`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
  [`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
  [`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
  [`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
  and
  [`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
  for computing Type I error, power, bias, coverage, and related metrics
  from Monte Carlo simulation output.

## [optic 1.2.3](https://github.com/RANDCorporation/optic/releases/tag/v1.2.3)

### Bug fixes

- Autoeffect model p-values now use the t-distribution (via
  [`cumulative_effects()`](https://rdrr.io/pkg/autoeffect/man/cumulative_effects.html))
  instead of manually computing with the normal distribution. This fixes
  inflated Type I error rates for DAR models.
  ([\#38](https://github.com/RANDCorporation/optic/pull/38))

## [optic 1.2.2](https://github.com/RANDCorporation/optic/releases/tag/v1.2.2)

### New features

- Clustered standard error support for autoeffect models, delegating to
  `autoeffect::cumulative_effects(cluster=)`. Unsupported SE adjustments
  (huber, arellano) now warn and skip.
  ([\#37](https://github.com/RANDCorporation/optic/pull/37))

### Internal changes

- Moved SE adjustment behavior into the model type registry, replacing
  if-else chain in `noconf_postmodel()`.
  ([\#37](https://github.com/RANDCorporation/optic/pull/37))

## [optic 1.2.1](https://github.com/RANDCorporation/optic/releases/tag/v1.2.1)

### Improvements

- Autoeffect models now extract covariates from the formula RHS like
  reg/autoreg models, eliminating separate `x_formula` or `cov_names`
  parameters. Deprecation warnings added for the old interface.
  ([\#36](https://github.com/RANDCorporation/optic/pull/36))

### Internal changes

- Restored [`match.arg()`](https://rdrr.io/r/base/match.arg.html) for
  type and call validation in `new_optic_model()` constructor.
  ([\#35](https://github.com/RANDCorporation/optic/pull/35))

## [optic 1.2.0](https://github.com/RANDCorporation/optic/releases/tag/v1.2.0)

### New features

- Added autoeffect model type support for debiased autoregressive
  treatment effect estimation. New model type `"autoeffect"` integrates
  with the `autoeffect` package.
  ([\#34](https://github.com/RANDCorporation/optic/pull/34))
