# Vignette Build Configuration

## Overview

The OPTIC package vignettes use computationally expensive simulations.
To balance build time with comprehensive results, we use
environment-variable-controlled iteration counts.

## How It Works

### Iteration Counts

- **Quick builds** (default): 10 iterations
  - Used for R CMD check
  - Used for local development
  - Fast, suitable for testing
- **Full builds**: 1000 iterations
  - Used for pkgdown website
  - Comprehensive results for documentation
  - Triggered by `BUILD_VIGNETTES_FULL=true` environment variable

### Workflow Configuration

#### R CMD Check (`.github/workflows/R-CMD-check.yml`)

- No environment variable set
- Vignettes build with 10 iterations
- Fast checks (~minutes)

#### pkgdown Website (`.github/workflows/pkgdown.yaml`)

- Sets `BUILD_VIGNETTES_FULL=true`
- Vignettes build with 1000 iterations
- Comprehensive results for users
- Only runs on main branch pushes, releases, or manual trigger

## Local Testing

### Quick Build (10 iterations)

``` r
# Default behavior - no setup needed
devtools::build_vignettes()
```

### Full Build (1000 iterations)

``` r
# Set environment variable before building
Sys.setenv(BUILD_VIGNETTES_FULL = "true")
devtools::build_vignettes()

# Or from terminal:
BUILD_VIGNETTES_FULL=true Rscript -e "devtools::build_vignettes()"
```

### Testing a Specific Iteration Count

You can also set a custom iteration count for testing:

``` r
# Edit vignettes/intro_optic.Rmd temporarily
# Change n_iter assignment to your desired number
```

## Maintenance

### Adding New Vignettes

When adding new computationally expensive vignettes:

1.  Add the iteration control logic to the setup chunk:

``` r
n_iter <- if (identical(Sys.getenv("BUILD_VIGNETTES_FULL"), "true")) {
  100  # Full iterations for pkgdown
} else {
  10    # Quick iterations for testing
}
```

2.  Use `n_iter` in your simulation calls:

``` r
sim_config <- optic_simulation(
  # ... other parameters ...
  iters = n_iter
)
```

3.  Add a note explaining the iteration count:

``` r
if (n_iter < 100) {
  cat("Note: Using", n_iter, "iterations for demonstration. ")
  cat("Recommend 300-1000 iterations for production use.")
}
```

### Changing Iteration Counts

To adjust iteration counts, edit `vignettes/intro_optic.Rmd`: - Line
~37: Change `1000` for full builds - Line ~39: Change `10` for quick
builds

## Benefits of This Approach

1.  **Fast CI/CD**: R CMD check completes quickly with 10 iterations
2.  **Quality Documentation**: Website shows comprehensive
    1000-iteration results
3.  **No Manual Steps**: Fully automated based on build context
4.  **Easy Testing**: Simple environment variable for local full builds
5.  **Transparent**: Vignette clearly shows what iteration count is
    being used
