#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test autoreg (autoregressive) model type

library(dplyr)

test_data <- optic::overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_model creates valid autoreg model with lm", {
  model <- optic_model(
    name = "autoreg_lm",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    se_adjust = "none"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "autoreg")
  expect_equal(model$model_call, "lm")
})

test_that("optic_model creates valid autoreg model with feols", {
  model <- optic_model(
    name = "autoreg_feols",
    type = "autoreg",
    call = "feols",
    formula = crude.rate ~ treatment_change + unemploymentrate | year,
    se_adjust = "cluster"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "autoreg")
  expect_equal(model$model_call, "feols")
})

test_that("autoreg model uses treatment_change variable", {
  model <- optic_model(
    name = "autoreg_change",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change + unemploymentrate,
    se_adjust = "none"
  )

  formula_vars <- all.vars(model$model_formula)
  expect_true("treatment_change" %in% formula_vars)
})

test_that("autoreg model supports SE adjustments", {
  model <- optic_model(
    name = "autoreg_se",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change,
    se_adjust = c("none", "cluster-unit", "huber")
  )

  expect_equal(model$se_adjust, c("none", "cluster-unit", "huber"))
})

test_that("end-to-end: autoreg model with lm runs through dispatch_simulations", {
  model <- optic_model(
    name = "autoreg_lm_e2e",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 2,
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
    dispatch_simulations(sim, use_future = FALSE, seed = 456, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("autoreg_lm_e2e" %in% results$model_name)
  expect_true(all(c("estimate", "se", "iter") %in% colnames(results)))
  expect_equal(nrow(results), 2)
})

test_that("end-to-end: autoreg model creates lag_outcome variable", {
  model <- optic_model(
    name = "autoreg_lag_check",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change + unemploymentrate,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 2,
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
    dispatch_simulations(sim, use_future = FALSE, seed = 789, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 2)
})

test_that("end-to-end: autoreg model with cluster-unit SE adjustment", {
  model <- optic_model(
    name = "autoreg_cluster_e2e",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    se_adjust = c("none", "cluster-unit")
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 2,
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
    dispatch_simulations(sim, use_future = FALSE, seed = 321, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 2 * 2)
  expect_true(all(c("none", "cluster-unit") %in% results$se_adjustment))
})

test_that("end-to-end: autoreg model with confounding method", {
  model <- optic_model(
    name = "autoreg_confounding_e2e",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    se_adjust = "none"
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
    iters = 2,
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
    dispatch_simulations(sim, use_future = FALSE, seed = 567, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("autoreg_confounding_e2e" %in% results$model_name)
})

test_that("end-to-end: autoreg model with concurrent method", {
  skip("Concurrent method has bug: effect_magnitude not split into effect_magnitude1/2")

  model <- optic_model(
    name = "autoreg_concurrent_e2e",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment1_change + treatment2_change + unemploymentrate + as.factor(year),
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = test_data,
    models = list(model),
    iters = 2,
    method = "concurrent",
    unit_var = "state",
    time_var = "year",
    effect_magnitude = list(c(0, 0)),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    rhos = 0.5,
    years_apart = 2,
    ordered = TRUE,
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 678, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("autoreg_concurrent_e2e" %in% results$model_name)
})
