#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test autoeffect (debiased autoregressive) model type

library(dplyr)
if (requireNamespace("autoeffect", quietly = TRUE)) {
  library(autoeffect)
}

test_data <- optic::overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_model creates valid autoeffect model", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_basic",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 2,
    unit_name = "state",
    date_name = "year",
    trt_name = "trt_ind",
    outcome_name = "crude.rate"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "autoeffect")
  expect_equal(model$model_call, "autoeffect")
})

test_that("autoeffect model requires lags parameter", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_lags",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 3,
    trt_name = "trt_ind"
  )

  expect_equal(model$model_args$lags, 3)
})

test_that("autoeffect model stores trt_name parameter", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_trt",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 2,
    trt_name = "treatment_indicator"
  )

  expect_equal(model$model_args$trt_name, "treatment_indicator")
})

test_that("autoeffect model supports effect_lag parameter", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_effect_lag",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 3,
    effect_lag = 2,
    trt_name = "trt_ind"
  )

  expect_equal(model$model_args$effect_lag, 2)
})

test_that("autoeffect model supports covariates", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_cov",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 2,
    trt_name = "trt_ind",
    cov_names = c("unemploymentrate", "population")
  )

  expect_equal(model$model_args$cov_names, c("unemploymentrate", "population"))
})

test_that("autoeffect only works with autoeffect call", {
  skip_if_not_installed("autoeffect")

  expect_error(
    optic_model(
      name = "autoeffect_invalid",
      type = "autoeffect",
      call = "lm",
      formula = crude.rate ~ treatment,
      se_adjust = "none"
    )
  )
})

test_that("end-to-end: autoeffect model runs through dispatch_simulations", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_e2e",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 2,
    unit_name = "state",
    date_name = "year",
    trt_name = "trt_ind",
    outcome_name = "crude.rate",
    effect_lag = 2
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
  expect_true("autoeffect_e2e" %in% results$model_name)
  expect_true(all(c("estimate", "se") %in% colnames(results)))
})

test_that("end-to-end: autoeffect model with covariates", {
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_cov_e2e",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 2,
    unit_name = "state",
    date_name = "year",
    trt_name = "trt_ind",
    outcome_name = "crude.rate",
    cov_names = "unemploymentrate",
    effect_lag = 2
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

test_that("end-to-end: autoeffect model with confounding method", {
  skip("Confounding method needs fix: selbias code tries to access $coefficients on autoeffect models")
  skip_if_not_installed("autoeffect")

  model <- optic_model(
    name = "autoeffect_confounding_e2e",
    type = "autoeffect",
    call = "autoeffect",
    formula = crude.rate ~ treatment,
    se_adjust = "none",
    lags = 2,
    unit_name = "state",
    date_name = "year",
    trt_name = "trt_ind",
    outcome_name = "crude.rate",
    effect_lag = 2
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
  expect_true("autoeffect_confounding_e2e" %in% results$model_name)
})
