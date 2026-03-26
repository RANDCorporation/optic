# Changelog

## optic (development version)

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

### Bug fixes

- Fixed GitHub Actions workflow errors: resolved missing knitr
  dependencies for vignette building and covr conflicts with parallel
  test execution.
  ([\#34](https://github.com/RANDCorporation/optic/pull/34))

## [optic 1.2.0](https://github.com/RANDCorporation/optic/releases/tag/v1.2.0)

### New features

- Added `type="autoeffect"` model type support to the simulation
  framework.
  ([`6b791fe`](https://github.com/RANDCorporation/optic/commit/6b791fe))

### Internal changes

- Refactored model-specific behavior out of method scripts into central
  registry in `model-type-behaviors.R`, consolidating duplicated if/else
  chains.
  ([`f92688f`](https://github.com/RANDCorporation/optic/commit/f92688f))
- Consolidated duplicated SE adjustment code into centralized helper
  functions.
  ([`005e88c`](https://github.com/RANDCorporation/optic/commit/005e88c))

## [optic 1.1.3](https://github.com/RANDCorporation/optic/releases/tag/v1.1.3)

### New features

- Enhanced validation system for model specifications, improving error
  messages and catching configuration issues earlier.
- Introduced snapshot testing for simulation results to ensure
  consistent output structure across versions.

### Improvements

- Major refactoring of
  [`dispatch_simulations()`](https://randcorporation.github.io/optic/reference/dispatch_simulations.md)
  function for improved clarity and maintainability.
- Enhanced `optic_model` class with better validation and documentation.
- Replaced deprecated
  [`purrr::cross()`](https://purrr.tidyverse.org/reference/cross.html)
  with
  [`tidyr::expand_grid()`](https://tidyr.tidyverse.org/reference/expand_grid.html)
  for more reliable parameter expansion.
- Improved vignettes with updated examples using refactored functions.
- Clarified package description and documentation.

### Bug fixes

- Fixed parameter handling in
  [`dispatch_simulations()`](https://randcorporation.github.io/optic/reference/dispatch_simulations.md)
  where params list structure was incorrect.
- Fixed `expand_grid` issue in parameter combinations.
- Fixed no-confounding method tests to use correct outcome variables.
- Corrected CRAN check issues related to `dispatch_simulations`
  documentation.
- Fixed vignette build issues
  ([\#22](https://github.com/randcorporation/optic/issues/22)).

### Internal changes

- Removed `augsynth` from package dependencies to move to Suggests
  (addresses installation issues on some platforms).
- Improved GitHub Actions workflows with separate test and check
  workflows for better CI/CD reliability.
- Added comprehensive snapshot tests for confounding and no-confounding
  methods.
- Updated NAMESPACE to remove unused exports.

## [optic 1.0.2](https://github.com/RANDCorporation/optic/releases/tag/v1.0.2)

- Bug fixes for spillover analysis.

## [optic 1.0.1](https://github.com/RANDCorporation/optic/releases/tag/v1.0.1)

CRAN release: 2023-08-08

- reduces run-time of vignettes by reducing number of replications.

## [optic 1.0.0](https://github.com/RANDCorporation/optic/releases/tag/v1.0.0)

CRAN release: 2023-05-26

- First package release.
