#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test causalAR (debiased autoregressive) model type

library(dplyr)
if (requireNamespace("causalAR", quietly = TRUE)) {
  library(causalAR)
}

test_data <- optic::overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_model creates valid causalAR model", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_basic",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "causalAR")
  expect_equal(model$model_call, "causalAR")
})

test_that("causalAR model requires lags parameter", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_lags",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 3
  )

  expect_equal(model$model_args$lags, 3)
})

test_that("causalAR model stores trt_name parameter", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_trt",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2,
    trt_name = "treatment_indicator"
  )

  expect_equal(model$model_args$trt_name, "treatment_indicator")
})

test_that("effect_lag is deprecated and warns when different from lags", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_effect_lag",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 3,
    effect_lag = 2
  )

  expect_warning(
    args <- resolve_causal_ar_args(model, "state", "year"),
    "effect_lag.*deprecated"
  )
  expect_equal(args$effect_lag, 3)  # uses lags, not effect_lag
})

test_that("covariates extracted from formula RHS", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_formula_cov",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none",
    lags = 2
  )

  args <- resolve_causal_ar_args(model, "state", "year")
  expect_true(inherits(args$x_formula, "formula"))
  expect_equal(all.vars(args$x_formula), "unemploymentrate")
})

test_that("factor covariates preserved in extracted x_formula", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_factor_cov",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level + unemploymentrate + as.factor(year),
    se_adjust = "none",
    lags = 2
  )

  args <- resolve_causal_ar_args(model, "state", "year")
  expect_true(inherits(args$x_formula, "formula"))
  formula_str <- deparse(args$x_formula)
  expect_true(grepl("unemploymentrate", formula_str))
  expect_true(grepl("as.factor\\(year\\)", formula_str))
})

test_that("no covariates when formula has only treatment variable", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_no_cov",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2
  )

  args <- resolve_causal_ar_args(model, "state", "year")
  expect_null(args$x_formula)
})

test_that("deprecated cov_names triggers warning and converts to x_formula", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_deprecated_cov",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2,
    cov_names = c("unemploymentrate", "population")
  )

  expect_warning(
    args <- resolve_causal_ar_args(model, "state", "year"),
    "cov_names.*deprecated"
  )
  expect_true(inherits(args$x_formula, "formula"))
  expect_null(args$cov_names)
})

test_that("deprecated x_formula in ... triggers warning", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_deprecated_xfml",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2,
    x_formula = ~unemploymentrate
  )

  expect_warning(
    args <- resolve_causal_ar_args(model, "state", "year"),
    "x_formula.*deprecated"
  )
  expect_true(inherits(args$x_formula, "formula"))
})

test_that("causalAR only works with causalAR call", {
  skip_if_not_installed("causalAR")

  expect_error(
    optic_model(
      name = "causalAR_invalid",
      type = "causalAR",
      call = "lm",
      formula = crude.rate ~ treatment_level,
      se_adjust = "none"
    )
  )
})

test_that("end-to-end: causalAR model runs through dispatch_simulations", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_e2e",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 1,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 999, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("causalAR_e2e" %in% results$model_name)
  expect_true(all(c("estimate", "se") %in% colnames(results)))
})

test_that("end-to-end: causalAR model with covariates in formula", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_cov_e2e",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none",
    lags = 2
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 1,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 888, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 1)
})

test_that("end-to-end: causalAR with cluster-unit SE produces two result rows", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_cluster_e2e",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = c("none", "cluster-unit"),
    lags = 2
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 1,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 555, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 2)
  expect_true("none" %in% results$se_adjustment)
  expect_true("cluster-unit" %in% results$se_adjustment)

  # Verify both rows have valid SE values
  expect_true(all(!is.na(results$se)))
})

test_that("causalAR with unsupported huber SE warns and skips", {
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_huber_warn",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = c("none", "huber"),
    lags = 2
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 1,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  expect_warning(
    results <- dispatch_simulations(sim, use_future = FALSE, seed = 444, verbose = 0),
    "not supported for causalAR"
  )

  # Only "none" row should be present (huber skipped)
  expect_equal(nrow(results), 1)
  expect_equal(results$se_adjustment, "none")
})

test_that("end-to-end: causalAR model with confounding method", {
  skip("Confounding method needs fix: selbias code tries to access $coefficients on causalAR models")
  skip_if_not_installed("causalAR")

  model <- optic_model(
    name = "causalAR_confounding_e2e",
    type = "causalAR",
    call = "causalAR",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    lags = 2
  )

  bias_vals <- list(
    linear = list(
      level = list(
        small = c(b0 = -3.9, b1 = 0.06, b2 = 0.06, b3 = 0, b4 = 0, b5 = 0,
                 a1 = 0.2, a2 = 0.05, a3 = 0, a4 = 0, a5 = 0)
      )
    )
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 1,
    method = "confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    conf_var = "unemploymentrate",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = c("null"),
    policy_speed = list("instant"),
    n_implementation_periods = c(0),
    bias_type = "linear",
    bias_size = "small",
    globals = list(bias_vals = bias_vals),
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 777, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("causalAR_confounding_e2e" %in% results$model_name)
})
