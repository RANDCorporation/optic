# optic (development version)

* No changes yet.

# optic 1.1.0

## New features

* Enhanced validation system for model specifications, improving error messages and catching configuration issues earlier.
* Introduced snapshot testing for simulation results to ensure consistent output structure across versions.

## Improvements

* Major refactoring of `dispatch_simulations()` function for improved clarity and maintainability.
* Enhanced `optic_model` class with better validation and documentation.
* Replaced deprecated `purrr::cross()` with `tidyr::expand_grid()` for more reliable parameter expansion.
* Improved vignettes with updated examples using refactored functions.
* Clarified package description and documentation.

## Bug fixes

* Fixed issues with spillover analysis validation and testing.
* Fixed parameter handling in `dispatch_simulations()` where params list structure was incorrect.
* Fixed `expand_grid` issue in parameter combinations.
* Fixed no-confounding method tests to use correct outcome variables.
* Corrected CRAN check issues related to `dispatch_simulations` documentation.
* Fixed vignette build issues (#22).

## Internal changes

* Removed `augsynth` from package dependencies to move to Suggests (addresses installation issues on some platforms).
* Improved GitHub Actions workflows with separate test and check workflows for better CI/CD reliability.
* Added comprehensive snapshot tests for confounding and no-confounding methods.
* Updated NAMESPACE to remove unused exports.

# optic 1.0.2

* Bug fixes for spillover analysis.

# optic 1.0.1

* reduces run-time of vignettes by reducing number of replications.

# optic 1.0.0

* First package release.

