
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
models using **your own** longitudinal data. Researchers from the Opioid
Policy Tools and Information Center (OPTIC) initially created the tool
to examine longitudinal data related to opioids, but its framework can
be used with longitudinal data on topics other than opioids.

## Background and Rationale

Recent difference-in-differences (DID) literature revealed issues with
the traditional DID model, but we found it very difficult to evaluate
the relative performance of different causal inference methods using our
own data. Thus, we designed a series of simulations ([Griffin et al.
2021](#ref-griffinMovingClassicDifferenceindifferences2021); [Griffin et
al. 2023](#ref-griffinMethodologicalConsiderationsEstimating2023)) to
study the performance of various methods under different scenarios. Our
publications to date are as follows:

1.  In Griffin et al.
    ([2021](#ref-griffinMovingClassicDifferenceindifferences2021)), we
    use real-world data on opioid mortality rates to assess commonly
    used statistical models for DID designs, which are widely used in
    state policy evaluations. These experiments demonstrated notable
    limitations of those methods. In contrast, the optimal model we
    identified—the autoregressive (AR) model—showed a lot of promise.
    That said, do not just take our word for it; try it out with your
    own data and see how various approaches perform relative to another.
    See the “Usage” section for details.

2.  In Griffin et al.
    ([2023](#ref-griffinMethodologicalConsiderationsEstimating2023)), we
    demonstrate that it is critical to be able to control for effects of
    co-occurring policies and understand the potential bias that might
    arise from not controlling for those policies. Our package can help
    you assess the impact of co-occurring policies on the performance of
    commonly used statistical models in state policy evaluations.

Assessing those methods in a systematic way might be challenging, but
you can now use our `optic` R package to simulate policy effects and
compare causal inference models using your own data.

The package supports the traditional two-way fixed effects DID model and
the AR model, as well as other leading methods, such as augmented
synthetic control and the Callaway-Sant’Anna approach to DID
([Ben-Michael, Feller, and Rothstein
2021](#ref-ben-michaelAugmentedSyntheticControl2021); [Callaway and
Sant’Anna 2021](#ref-callawayDifferenceinDifferencesMultipleTime2021)).

### Why `optic`?

`optic` is named after the **Opioid Policy Tools and Information Center
(OPTIC)** project.

## Installation

You will need [R (version 4.1.0 or above)](https://www.r-project.org) to
use this package. You can install the `optic` R package from the `R`
console:

``` r
# install from CRAN:
install.packages("optic")

# or install the development version from github:
# install remotes if needed
install.packages("remotes")
remotes::install_github("RANDCorporation/optic")
```

## Usage

The [introductory
vignette](https://randcorporation.github.io/optic/articles/intro_optic.html)
provides a working example using a sample `overdoses` dataset provided
with the package. `optic` provides three main functions: `optic_model`,
`optic_simulation`, and `dispatch_simulations`. Use `optic_model` to
define model specifications for each causal model to be tested in the
simulation experiment. Then, pass your models, your data, and your
parameters to the `optic_simulation` function, which specifies a set of
simulations to be performed for each `optic_model` included in your
`list` of models. Finally, use `dispatch_simulations` to run your
simulations in parallel.

## Contact

Reach out to [Beth Ann
Griffin](https://www.rand.org/about/people/g/griffin_beth_ann.html) for
questions related to this repository.

## License

Copyright (C) 2023 by The [RAND Corporation](https://www.rand.org). This
repository is released as open-source software under a GPL-3.0 license.
See the LICENSE file.

## About this Tool

This research was financially supported through a National Institute on
Drug Abuse grant (P50DA046351) to the RAND Corporation and carried out
within the Access and Delivery Program in RAND Health Care.

RAND Health Care, a division of the RAND Corporation, promotes healthier
societies by improving health care systems in the United States and
other countries. We do this by providing health care decisionmakers,
practitioners, and consumers with actionable, rigorous, objective
evidence to support their most complex decisions. For more information,
see [www.rand.org/health-care](https://www.rand.org/health-care), or
contact

**RAND Health Care Communications**  
1776 Main Street  
P.O. Box 2138  
Santa Monica, CA 90407-2138  
(310) 393-0411, ext. 7775  
<RAND_Health-Care@rand.org>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-ben-michaelAugmentedSyntheticControl2021"
class="csl-entry">

Ben-Michael, Eli, Avi Feller, and Jesse Rothstein. 2021. “The Augmented
Synthetic Control Method.” *Journal of the American Statistical
Association* 116 (536): 1789–1803.
<https://doi.org/10.1080/01621459.2021.1929245>.

</div>

<div id="ref-callawayDifferenceinDifferencesMultipleTime2021"
class="csl-entry">

Callaway, Brantly, and Pedro H.C. Sant’Anna. 2021.
“Difference-in-Differences with Multiple Time Periods.” *Journal of
Econometrics* 225 (2): 200–230.
<https://doi.org/10.1016/j.jeconom.2020.12.001>.

</div>

<div id="ref-griffinMethodologicalConsiderationsEstimating2023"
class="csl-entry">

Griffin, Beth Ann, Megan S. Schuler, Joseph Pane, Stephen W. Patrick,
Rosanna Smart, Bradley D. Stein, Geoffrey Grimm, and Elizabeth A.
Stuart. 2023. “Methodological Considerations for Estimating Policy
Effects in the Context of Co-Occurring Policies.” *Health Services and
Outcomes Research Methodology*, June.
<https://doi.org/10.1007/s10742-022-00284-w>.

</div>

<div id="ref-griffinMovingClassicDifferenceindifferences2021"
class="csl-entry">

Griffin, Beth Ann, Megan S. Schuler, Elizabeth A. Stuart, Stephen
Patrick, Elizabeth McNeer, Rosanna Smart, David Powell, Bradley D.
Stein, Terry L. Schell, and Rosalie Liccardo Pacula. 2021. “Moving
Beyond the Classic Difference-in-Differences Model: A Simulation Study
Comparing Statistical Methods for Estimating Effectiveness of
State-Level Policies.” *BMC Medical Research Methodology* 21 (1): 279.
<https://doi.org/10.1186/s12874-021-01471-y>.

</div>

<div id="ref-opticIntroductionOPTIC" class="csl-entry">

optic. n.d. “Introduction to OPTIC.” Accessed June 16, 2023.
<https://randcorporation.github.io/optic/articles/intro_optic.html>.

</div>

<div id="ref-rProjectStatisticalComputing" class="csl-entry">

R. n.d. “The R Project for Statistical Computing.” Accessed June 16,
2023. <https://www.r-project.org/>.

</div>

<div id="ref-randcorporationBethAnnGriffin" class="csl-entry">

RAND Corporation. n.d.a. “Beth Ann Griffin.” Accessed June 20, 2023.
<https://www.rand.org/about/people/g/griffin_beth_ann.html>.

</div>

<div id="ref-randcorporationHomepage" class="csl-entry">

———. n.d.b. “Homepage.” Accessed June 20, 2023. <https://www.rand.org>.

</div>

<div id="ref-randcorporationRANDUSCSchaefferOpioid" class="csl-entry">

———. n.d.c. “RAND-USC Schaeffer Opioid Policy Tools and Information
Center (OPTIC).” Accessed June 20, 2023.
<https://www.rand.org/health-care/centers/optic.html>.

</div>

<div id="ref-rdrr.ioInstallPackagesInstall" class="csl-entry">

rdrr.io. n.d. “Install.packages: Install Packages from Repositories or
Local Files.” Accessed June 20, 2023.
<https://rdrr.io/r/utils/install.packages.html>.

</div>

</div>
