# Release Notes Template

Use this template when drafting GitHub release notes.

---

## optic vX.Y.Z

**Release Date:** YYYY-MM-DD
**Release Type:** [Major/Minor/Patch]
**CRAN Submission:** [Yes/No]

### Summary

[2-3 sentence overview of what this release accomplishes. Focus on the user benefit and main theme of the release.]

Example:
> This release introduces new spillover detection methods and improves performance for large datasets. We've also fixed several bugs related to missing data handling and updated documentation throughout the package.

### What's New

[List new features and enhancements. Use bullet points. Each item should:]
- Start with a verb in past tense (Added, Introduced, Implemented)
- Describe the user-visible benefit
- Be specific but concise
- Include issue/PR numbers if available

Example:
* Added support for synthetic control methods with `synth_method()` function
* Implemented parallel processing for simulations, reducing runtime by up to 50%
* Introduced new vignette demonstrating advanced spillover analysis (#45)

### Bug Fixes

[List bugs fixed. Each item should:]
- Describe what was broken and how it's fixed
- Use past tense (Fixed, Resolved, Corrected)
- Reference issue numbers

Example:
* Fixed issue where missing data caused simulation failures (#42)
* Resolved error in plot output for negative treatment effects (#38)
* Corrected documentation typos in `dispatch_simulation()` (#40)

### Breaking Changes

[Only include this section if there are breaking changes. Each item should:]
- Clearly state what changed
- Explain the impact on users
- Provide migration guidance if possible

Example:
* **BREAKING:** `calculate_ate()` now requires explicit specification of the treatment variable. Update your code:
  ```r
  # Old (deprecated)
  calculate_ate(data)

  # New (required)
  calculate_ate(data, treatment = "treated")
  ```

### Improvements

[List non-breaking improvements to existing features]

Example:
* Improved error messages throughout the package
* Enhanced performance of core simulation engine
* Updated default parameters for better convergence

### Documentation

[List documentation improvements if significant]

Example:
* Updated Getting Started vignette with more examples
* Added new case study demonstrating multi-period analysis
* Improved function documentation with more examples

### Dependencies

[List any dependency changes]

Example:
* Updated minimum R version to 4.1.0 (from 4.0.0)
* Added `progressr` package for progress bars
* Removed dependency on deprecated `tidyselect` features

### Internal Changes

[Optional: List internal changes that don't affect users but are worth noting]

Example:
* Refactored simulation engine for better maintainability
* Expanded test coverage to 90%
* Improved CI/CD pipeline with additional checks

---

## Installation

```r
# From CRAN (after CRAN acceptance)
install.packages("optic")

# From GitHub (immediately available)
remotes::install_github("randcorporation/optic@vX.Y.Z")

# Development version
remotes::install_github("randcorporation/optic")
```

## Upgrade Notes

[Any special notes for users upgrading from previous versions]

Example:
Users upgrading from version 1.0.x should note that the `calculate_ate()` function now requires explicit treatment specification. See Breaking Changes above.

## Full Changelog

For a complete list of changes, see [NEWS.md](https://github.com/randcorporation/optic/blob/main/NEWS.md).

## Acknowledgments

[Optional: Thank contributors]

Example:
Thanks to @contributor1 for bug reports and @contributor2 for documentation improvements!

---

## For CRAN Releases

If this is a CRAN release, add this section:

### CRAN Submission

This release has been submitted to CRAN. Until it's accepted, you can install from GitHub:

```r
remotes::install_github("randcorporation/optic@vX.Y.Z")
```

We'll update this release note once the package is available on CRAN.

---

## For Patch Releases

If this is a patch release (GitHub only), add this note:

### Note

This is a patch release and will not be submitted to CRAN. Install from GitHub:

```r
remotes::install_github("randcorporation/optic@vX.Y.Z")
```

The next minor or major release will include these changes and be submitted to CRAN.

---

## Tips for AI Agents

When using this template:

1. **Analyze commit messages**: Group commits by type (feat:, fix:, docs:, etc.)
2. **Synthesize, don't just list**: Combine related commits into single bullet points
3. **Focus on user impact**: Explain what users can now do, not just what code changed
4. **Use clear language**: Avoid jargon, explain technical terms
5. **Be specific**: "Improved performance by 50%" is better than "Improved performance"
6. **Include examples**: Show code examples for new features or breaking changes
7. **Reference issues**: Link to GitHub issues/PRs when relevant (#123)
8. **Order by importance**: Most important items first
9. **Keep it scannable**: Use headings, bullets, and formatting
10. **Update the date**: Set release date when creating the release

## Example Real Release Notes

```markdown
## optic v1.1.0

**Release Date:** 2024-01-15
**Release Type:** Minor
**CRAN Submission:** Yes

### Summary

This release significantly expands optic's capabilities for analyzing spillover effects in longitudinal data. We've added new methods for synthetic control analysis, improved performance for large datasets through parallel processing, and fixed several bugs related to edge cases in the simulation engine.

### What's New

* Added synthetic control method support with `synth_method()` (#50)
* Implemented parallel processing for simulations using `future` backend (#48)
* Introduced new vignette "Advanced Spillover Analysis" demonstrating multi-period designs (#45)
* Added progress bars for long-running simulations (#43)

### Bug Fixes

* Fixed crash when data contains missing values in covariates (#42)
* Resolved plotting error for negative treatment effects (#38)
* Corrected variance calculation in `calculate_se()` for clustered data (#35)

### Improvements

* Improved error messages to be more actionable
* Enhanced documentation with more examples throughout
* Optimized memory usage for large datasets (up to 40% reduction)

### Dependencies

* Added `progressr` for progress reporting
* Updated `did` package requirement to >= 2.1.0

## Installation

```r
# From CRAN (available soon)
install.packages("optic")

# From GitHub (available now)
remotes::install_github("randcorporation/optic@v1.1.0")
```

## Full Changelog

See [NEWS.md](https://github.com/randcorporation/optic/blob/main/NEWS.md) for complete details.

## Acknowledgments

Thanks to @user123 for reporting issue #42 and providing a reproducible example!
```
