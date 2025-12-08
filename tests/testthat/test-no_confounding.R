#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Testing an example of the no_confounding method
library(dplyr)

data(overdoses)

linear0 <- 0
linear5 <- .05*mean(overdoses$crude.rate, na.rm=T)

# create optic_model object:
fixedeff_linear <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights= as.name("population"),
  se_adjust=c("none", "cluster-unit")
)

# Remove states for consistency:
data <- overdoses %>%
  dplyr::filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon")))

#------------------------------------------------------------------------------#
# Test 1: Simple test for R CMD check (CRAN-compatible, fast)
#------------------------------------------------------------------------------#
test_that("no_confounding basic test (CRAN check)", {
  
  simple_config <- optic_simulation(
    x=data,
    models=list(fixedeff_linear),
    iters=5,  # Reduced iterations for faster CRAN checks
    method = "no_confounding",
    globals=NULL,
    unit_var="state",
    treat_var="state",
    time_var="year",
    effect_magnitude=list(linear0),
    n_units= c(5),
    effect_direction=c("neg"),
    policy_speed=c("instant"),
    n_implementation_periods=c(1)
  )
  
  suppressWarnings({
    simple_results <- dispatch_simulations(
      simple_config,
      use_future=F,  # Simpler execution for CRAN
      seed=9782,
      verbose=0,
      future.globals=c("cluster_adjust_se"),
      future.packages=c("MASS", "dplyr", "optic")
    )
  })
  
  col_types <- sapply(simple_results, class)
  expect_snapshot(list(
    test_type = "simple_cran",
    dim = dim(simple_results),
    colnames = colnames(simple_results),
    nrow = nrow(simple_results),
    col_types = col_types
  ))
})

#------------------------------------------------------------------------------#
# Test 2: Comprehensive test with augsynth and multiple models (full test suite)
#------------------------------------------------------------------------------#
test_that("no_confounding comprehensive test with all models", {
  skip_if_not(nzchar(Sys.getenv("OPTIC_FULL_MODEL_SUITE")), 
              "Skipping full model suite test - set OPTIC_FULL_MODEL_SUITE=true to run")
  
  library(augsynth)
  
  fixedeff_linear_two <- optic_model(
    name="fixedeff_linear_two",
    type="reg",
    call="lm",
    formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
    weights= as.name("population"),
    se_adjust=c("none", "cluster-unit", "cluster-treat", "huber", "arellano")
  )
  
  lm_ar <- optic_model(
    name = "auto_regressive_linear",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ unemploymentrate + year + treatment_change,
    se_adjust = "cluster-unit"
  )
  
  m_aug <- optic_model(
    name = "augsynth",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1,
    se_adjust = "none",
    fixedeff = F
  )
  
  m_csa <- optic_model(
    name = "csa_did",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    yname = "crude.rate",
    tname = 'year',
    idname = 'state',
    gname = 'treatment_date',
    se_adjust = "none"
  )
  
  comprehensive_config <- optic_simulation(
    x=data,
    models=list(m_aug, m_csa, fixedeff_linear, fixedeff_linear_two, lm_ar),
    iters=11,
    method = "no_confounding",
    globals=NULL,
    unit_var="state",
    treat_var="state",
    time_var="year",
    effect_magnitude=list(linear0, linear5),
    n_units= c(5),
    effect_direction=c("neg"),
    policy_speed=c("instant"),
    n_implementation_periods=c(1)
  )
  
  suppressWarnings({
    comprehensive_results <- dispatch_simulations(
      comprehensive_config,
      use_future=T,
      seed=9782,
      verbose=0,
      future.globals=c("cluster_adjust_se"),
      future.packages=c("MASS", "dplyr", "optic", "augsynth", "did")
    )
  })
  
  col_types <- sapply(comprehensive_results, class)
  expect_snapshot(list(
    test_type = "comprehensive_full_suite",
    dim = dim(comprehensive_results),
    colnames = colnames(comprehensive_results),
    nrow = nrow(comprehensive_results),
    col_types = col_types
  ))
})


