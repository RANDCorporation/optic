# Changelog

## optic (development version)

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
  `cumulative_effects()`) instead of manually computing with the normal
  distribution. This fixes inflated Type I error rates for DAR models.
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
