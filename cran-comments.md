## Release Summary

This is a minor release of optic (version 1.1.0). This version includes:

* New time-varying treatment methods for analyzing policies with time-varying effects
* Major refactoring of dispatch_simulations() function with improved documentation
* Enhanced validation system for model specifications
* Multiple bug fixes for spillover analysis and parameter handling
* Improved vignettes and documentation
* Removed augsynth from dependencies (moved to Suggests) to address installation issues

## Test environments

* Local: macOS (Darwin 23.6.0), R 4.4.0
* GitHub Actions:
  - macOS-latest (R release)
  - windows-latest (R release)
  - ubuntu-latest (R release, oldrel-1)
* win-builder: (will be tested prior to submission)

## R CMD check results

There were no ERRORs or WARNINGs.

There may be 1 NOTE:

* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can be ignored.

## Downstream dependencies

There are currently no downstream dependencies for this package.
