
<!-- README.md is generated from README.Rmd. Please edit that file -->

# optic <a href='https://github.com/RANDCorporation/optic'><img src='man/figures/optic.png' align="right" height="139"  style="height:139px !important;" /></a>

**Simulation Tool for Causal Inference Using Longitudinal Data**

[![R-CMD-check](https://github.com/RANDCorporation/optic/workflows/R-CMD-check/badge.svg)](https://github.com/RANDCorporation/optic/actions)
[![Test
Coverage](https://github.com/RANDCorporation/optic/workflows/test-coverage/badge.svg)](https://github.com/RANDCorporation/optic/actions)

<!-- badges: end
[![codecov](https://codecov.io/gh/RANDCorporation/optic/branch/develop/graph/badge.svg?token=5XYDOFFJMH)](https://codecov.io/gh/RANDCorporation/optic)
-->

The `optic` R package helps you scrutinize candidate causal inference
models using **your own** longitudinal data.

The recent Diff-in-Diff literature revealed issues with the traditional
Diff-in-Diff model, but we found it very difficult to evaluate the
relative performance of different causal inference methods using *our
own data*.

Thus, we designed a series of simulations ([Griffin et al.
2021](#ref-http://zotero.org/users/3390799/items/ZNCVTPJF); [Griffin et
al. 2022](#ref-http://zotero.org/users/3390799/items/V3Q6ARUA)) to study
the performance of various methods under different scenarios. Our
publications to date include:

1.  In Griffin et al.
    ([2021](#ref-http://zotero.org/users/3390799/items/ZNCVTPJF)), we
    use real-world data on opioid mortality rates to assess commonly
    used statistical models for Difference-In-Differences (DID) designs,
    which are widely used in state policy evaluations. These experiments
    demonstrated notable limitations of those methods. In contrast, the
    optimal model we identified–the autoregressive model (AR) model-
    showed a lot of promise. That said, don’t just take our word for
    it - try it out with your own data and see how various approaches do
    relative to each other. See below for details.

2.  In Griffin et al.
    ([2022](#ref-http://zotero.org/users/3390799/items/V3Q6ARUA)), we
    also demonstrate it is critical to be able to control for effects of
    co-occurring policies, and understand the potential bias that might
    arise from not controlling for those policies. Our package can also
    help you assess the impact of co-occurring policies on the
    performance of commonly-used statistical models in state policy
    evaluations.

Assessing those methods in a systematic way might be challenging, but
now you can now use our `optic` R package to simulate policy effects and
compare causal inference models using your own data.

The package supports the traditional two-way fixed effects DID model and
the AR model as well as other leading methods like augment synthetic
control and the Callaway-Santa’Anna approach to DID.

### Why `optic`?

`optic` is named after the **Opioid Policy Tools and Information Center
(OPTIC)** project. The research was financially supported through a
National Institutes of Health (NIH) grant (P50DA046351) to RAND (PI:
Stein).

## Installation

You will need [R (\>= 4.1.0)](https://www.r-project.org) to use this
package. You can install the `optic` R package from the `R` console:

``` r
# in the near future, you will be able to install from CRAN with
install.packages("optic")

# or install the development version from github:
# install remotes if needed
install.packages("remotes")
remotes::install_github("RANDCorporation/optic", build_vignettes = T)
```

## Usage

Please see the introductory vignette by running
`vignette("intro_optic")` after installing the package. The vignette
provides a working example using a sample `overdoses` dataset provided
with the package. `optic` provides three main functions: `optic_model`,
`optic_simulation`, and `dispatch_simulations`. Use `optic_model` to
define model specifications for each causal model to be tested in the
simulation experiment. Then, pass your models, your data, and parameters
to the `optic_simulation` function, that specifies a set of simulations
to be performed for each `optic_model` included in your `list` of
models. Finally, use `dispatch_simulations` to run your simulations in
parallel.

## Contact

Reach out to [Beth Ann
Griffin](https://www.rand.org/about/people/g/griffin_beth_ann.html) for
questions related to this repository.

## License

Copyright (C) 2023 by The [RAND Corporation](https://www.rand.org). This
repository is released as open-source software under a GPL-3.0 license.
See the LICENSE file.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-http://zotero.org/users/3390799/items/V3Q6ARUA"
class="csl-entry">

Griffin, Beth Ann, Megan S. Schuler, Joseph Pane, Stephen W. Patrick,
Rosanna Smart, Bradley D. Stein, Geoffrey Grimm, and Elizabeth A.
Stuart. 2022. “Methodological Considerations for Estimating Policy
Effects in the Context of Co-Occurring Policies.” *Health Services and
Outcomes Research Methodology*, July.
<https://doi.org/10.1007/s10742-022-00284-w>.

</div>

<div id="ref-http://zotero.org/users/3390799/items/ZNCVTPJF"
class="csl-entry">

Griffin, Beth Ann, Megan S. Schuler, Elizabeth A. Stuart, Stephen
Patrick, Elizabeth McNeer, Rosanna Smart, David Powell, Bradley D.
Stein, Terry L. Schell, and Rosalie Liccardo Pacula. 2021. “Moving
Beyond the Classic Difference-in-Differences Model: A Simulation Study
Comparing Statistical Methods for Estimating Effectiveness of
State-Level Policies.” *BMC Medical Research Methodology* 21 (1): 279.
<https://doi.org/10.1186/s12874-021-01471-y>.

</div>

</div>
