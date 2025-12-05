# NEWS.md Entry Template

Use this template when drafting NEWS.md entries. NEWS.md follows a simpler, more concise format than GitHub release notes.

---

## Format

```markdown
# optic X.Y.Z

[Brief one-line summary of the release - optional but recommended for major releases]

## New features

* Feature description (#issue)
* Another feature

## Bug fixes

* Bug fix description (#issue)
* Another fix

## Breaking changes

* Breaking change with migration notes

## Improvements

* Improvement description
* Another improvement

## Deprecated

* Function/feature being deprecated with timeline

## Internal

* Internal change (optional, only if significant)
```

---

## Guidelines

### General Rules

1. **Use present tense for headers** (`New features`, not `New Features`)
2. **Use past tense for items** (`Added X`, not `Add X`)
3. **Be concise**: NEWS.md is more compact than release notes
4. **Focus on user-visible changes**: Skip most internal refactoring
5. **Group related changes**: Combine similar commits into one bullet
6. **Reference issues**: Include issue numbers (#123)
7. **No code examples**: Keep it text-only (examples go in release notes)

### Section Guidelines

**New features** (required if applicable):
- Start with action verb: "Added", "Implemented", "Introduced"
- One line per feature
- Most important features first

**Bug fixes** (required if applicable):
- Start with "Fixed", "Resolved", "Corrected"
- Describe what was broken
- Include issue number

**Breaking changes** (include section only if applicable):
- Mark clearly with bold or caps
- Explain what changed and why
- Brief migration note

**Improvements** (optional):
- Non-breaking enhancements
- Performance improvements
- Better error messages, etc.

**Deprecated** (include only if applicable):
- What's being deprecated
- When it will be removed
- What to use instead

**Internal** (optional, use sparingly):
- Only for significant internal changes
- Major refactors, test improvements, etc.
- Skip minor internal tweaks

---

## Examples

### Minimal Example (Patch Release)

```markdown
# optic 1.0.3

## Bug fixes

* Fixed error in `dispatch_simulation()` when using custom methods (#45)
* Corrected documentation typo in vignette (#44)
```

### Medium Example (Minor Release)

```markdown
# optic 1.1.0

New capabilities for spillover analysis and improved performance.

## New features

* Added synthetic control method support via `synth_method()` (#50)
* Implemented parallel processing for simulations (#48)
* Added progress bars for long-running operations (#43)

## Bug fixes

* Fixed crash with missing covariate data (#42)
* Resolved plotting error for negative effects (#38)

## Improvements

* Enhanced error messages throughout package
* Improved memory efficiency for large datasets
* Updated documentation with more examples
```

### Complex Example (Major Release)

```markdown
# optic 2.0.0

Major update with new analysis methods and API improvements.

## Breaking changes

* `calculate_ate()` now requires explicit treatment variable specification
* Removed deprecated `old_method()` function (use `new_method()` instead)
* Changed default for `robust_se` to `TRUE` (was `FALSE`)

## New features

* Added support for multiple treatment periods
* Implemented difference-in-differences with multiple time periods
* New `plot_diagnostics()` function for model checking
* Added Bayesian estimation methods

## Bug fixes

* Fixed variance calculation in clustered designs (#67)
* Resolved edge case in spillover detection (#65)
* Corrected bias in small sample settings (#62)

## Improvements

* Performance improvements (2-3x faster for large datasets)
* Better handling of missing data
* Improved convergence diagnostics
* Enhanced visualization functions

## Deprecated

* `old_simulation()` is deprecated and will be removed in v2.1.0
  Use `dispatch_simulation()` instead

## Internal

* Complete rewrite of simulation engine for better maintainability
* Expanded test coverage to 95%
* Refactored plotting functions for consistency
```

---

## AI Agent Checklist

When drafting NEWS.md entries:

- [ ] Read commits since last release
- [ ] Categorize commits into sections
- [ ] Combine related commits into single bullets
- [ ] Write in past tense ("Added", "Fixed")
- [ ] Keep entries concise (one line per change)
- [ ] Include issue numbers where applicable
- [ ] Put most important changes first in each section
- [ ] Check for breaking changes and mark clearly
- [ ] Omit trivial internal changes
- [ ] Ensure version number matches DESCRIPTION
- [ ] Keep development section at top of file
- [ ] Maintain consistent formatting with previous entries

---

## Commit Message Patterns to Sections

Map commit patterns to NEWS.md sections:

| Commit Pattern | NEWS Section |
|----------------|--------------|
| `feat:`, `add:`, `new:` | New features |
| `fix:`, `bug:` | Bug fixes |
| `break:`, `breaking:`, `!` | Breaking changes |
| `improve:`, `enhance:`, `perf:` | Improvements |
| `deprecate:` | Deprecated |
| `docs:`, `doc:` | Usually omit unless significant |
| `test:`, `tests:` | Internal (if significant) |
| `refactor:`, `chore:` | Internal (if significant) |
| `ci:`, `build:` | Usually omit |

---

## Full Example with Context

```markdown
# optic (development version)

* No changes yet.

# optic 1.1.0

Expanded spillover analysis capabilities and performance improvements.

## New features

* Added synthetic control methods with `synth_method()` function (#50)
* Implemented parallel processing using `future` backend (#48)
* Added new vignette for advanced spillover analysis (#45)
* Progress bars now display for long-running simulations (#43)

## Bug fixes

* Fixed crash when covariates contain missing values (#42)
* Resolved plotting error for negative treatment effects (#38)
* Corrected standard error calculation for clustered data (#35)

## Improvements

* Enhanced error messages to be more actionable
* Optimized memory usage for large datasets (40% reduction)
* Improved documentation with additional examples

## Internal

* Refactored simulation engine for better maintainability
* Increased test coverage to 90%

# optic 1.0.2

## Bug fixes

* Reduced vignette run-time by using fewer replications
* Fixed intermittent CRAN check issue

# optic 1.0.1

## Bug fixes

* Reduces run-time of vignettes by reducing number of replications

# optic 1.0.0

* First package release
```

---

## Tips for Conciseness

### Too Verbose (Don't)
```markdown
* We have added a new feature that allows users to run synthetic control
  methods by providing a new function called `synth_method()` which can be
  used in conjunction with the existing simulation framework to enable
  comparative analysis using synthetic control methodology
```

### Concise (Do)
```markdown
* Added synthetic control method support via `synth_method()` (#50)
```

### Too Vague (Don't)
```markdown
* Fixed bug
* Improved performance
* Updated documentation
```

### Specific (Do)
```markdown
* Fixed crash when covariates contain NA values (#42)
* Improved simulation speed by 50% through parallel processing (#48)
* Added examples to all exported functions
```
