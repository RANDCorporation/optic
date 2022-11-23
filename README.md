
<!-- README.md is generated from README.Rmd. Please edit that file -->

# optic <a href='https://optic-tools.github.io/optic/'><img src='man/figures/optic.png' align="right" height="139"  style="height:139px !important;" /></a>

**Simulation test-bed for Longitudinal Causal Inference models (or a
better name)**

<!-- badges: start -->

[![R-CMD-check](https://github.com/optic-tools/optic/workflows/R-CMD-check/badge.svg)](https://github.com/optic-tools/optic/actions)
[![Test
Coverage](https://github.com/optic-tools/optic/workflows/test-coverage/badge.svg)](https://github.com/optic-tools/optic/actions)
[![codecov](https://codecov.io/gh/optic-tools/optic/branch/develop/graph/badge.svg?token=5XYDOFFJMH)](https://codecov.io/gh/optic-tools/optic)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The `optic` package helps you scrutinize candidate causal inference
models using **your** longitudinal data.

The recent Diff-in-Diff literature revealed issues with the traditional
Diff-in-Diff model, but we found it very difficult to evaluate the
performance of causal inference methods using *our data*. Using
real-world data, [we find that autoregressive models outperform
diff-in-diff
models](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-021-01471-y).

`optic` is an R package we developed to support our work, and now you
can use it on your datasets as well.

The package supports the traditional two-way Diff-in-Diff model, Callway
and Santana, Autoregressive models, and multisynth.

### Why `optic`?

`optic` is named after the **Opioid Policy Tools and Information Center
(OPTIC)** project.

## Installation

You can install `optic` like so:

``` r
# install optic from cran (in the future)
install.packages("optic")

# install from github:
remotes::install_github("optic-tools/optic")
```

## Usage

`optic` provides two main workhorse functions: `optic_model` and
`optic_simulation`. Use `optic_model` to define model specifications for
each causal model to be tested in the simulation experiment. Then, pass
your models, your data, and parameters to the `optic_simulation`
function, that specifies a set of simulations to be performed for each
`optic_model` specified.

``` r
library(optic)
## basic example code

# overdose example data provided with the package:
data(overdoses)
x <- overdoses

model_1 <- optic_model(
         name="fixedeff_linear",
         type="reg",
         call="lm",
         formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1_level + treatment2_level,
         args=list(weights=as.name('population')),
         se_adjust=c("none", "cluster"))

model_2 <- optic_model(name="autoreg_linear",
              type="autoreg",
              call="lm",
              formula=deaths ~ unemploymentrate + as.factor(year) + treatment1_change + treatment2_change,
              args=list(weights=as.name('population')),
              se_adjust=c("none", "cluster"))


# we will define two scenarios for different effect magnitudes for
# the policies using 5, 10, and 15 percent changes in the outcome
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)

scenario1 <- c(linear10, linear10)
scenario2 <- c(linear5, linear15)

optic_sim <- optic_simulation(
  x=overdoses,
  models=list(model_1, model_2),
  iters=10,
  method_sample=concurrent_sample,
  method_pre_model=concurrent_premodel,
  method_model=concurrent_model,
  method_post_model=concurrent_postmodel,
  method_results=concurrent_results,
  
  params=list(
    unit_var="state",
    time_var="year",
    effect_magnitude=list(scenario1, scenario2),
    n_units=c(30),
    effect_direction=c("null", "neg"),
    policy_speed=c("instant", "slow"),
    n_implementation_periods=c(3),
    rhos=c(0, 0.25, 0.5, 0.75, 0.9),
    years_apart=2,
    ordered=TRUE
  )
)
```

After those steps, run `simulate()` on your `optic_simulation` object.
Doing so will run your simulations in parallel.

``` r
results <- simulate(
  optic_sim,
  use_future=TRUE,
  seed=9782,
  verbose=2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("dplyr", "optic")
)
```

Max: Explain what the user can do with this result.
