#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test dispatch_simulations function

library(dplyr)
data(overdoses)

simple_data <- overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("dispatch_simulations returns data.frame", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
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
    dispatch_simulations(sim, use_future = FALSE, verbose = 0)
  )

  expect_true(is.data.frame(results))
})

test_that("dispatch_simulations with seed produces reproducible results", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model),
    iters = 3,
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

  results1 <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 12345, verbose = 0)
  )

  results2 <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 12345, verbose = 0)
  )

  expect_equal(results1$estimate, results2$estimate)
})

test_that("dispatch_simulations adds seed column when seed provided", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
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
    dispatch_simulations(sim, use_future = FALSE, seed = 99999, verbose = 0)
  )

  expect_true("seed" %in% colnames(results))
  expect_equal(unique(results$seed), 99999)
})

test_that("dispatch_simulations without seed has no seed column", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
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
    dispatch_simulations(sim, use_future = FALSE, seed = NULL, verbose = 0)
  )

  expect_false("seed" %in% colnames(results))
})

test_that("dispatch_simulations returns correct number of rows", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model),
    iters = 3,
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
    dispatch_simulations(sim, use_future = FALSE, verbose = 0)
  )

  n_scenarios <- nrow(sim$simulation_params)
  n_models <- length(sim$models)
  n_iters <- sim$iters

  expect_equal(nrow(results), n_scenarios * n_models * n_iters)
})

test_that("dispatch_simulations includes iter column", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model),
    iters = 3,
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
    dispatch_simulations(sim, use_future = FALSE, verbose = 0)
  )

  expect_true("iter" %in% colnames(results))
  expect_true(all(results$iter >= 1 & results$iter <= 3))
})

test_that("dispatch_simulations works with multiple models", {
  model1 <- optic_model(
    name = "model1",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  model2 <- optic_model(
    name = "model2",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ treatment_change,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model1, model2),
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
    dispatch_simulations(sim, use_future = FALSE, verbose = 0)
  )

  expect_true("model1" %in% results$model_name)
  expect_true("model2" %in% results$model_name)
})

test_that("dispatch_simulations verbose output works", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
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

  expect_output(
    suppressWarnings(dispatch_simulations(sim, use_future = FALSE, verbose = 1)),
    "TOTAL RUNS"
  )
})

test_that("dispatch_simulations fails with invalid object", {
  expect_error(
    dispatch_simulations("not an OpticSim object"),
    "OpticSim"
  )
})

test_that("dispatch_simulations returns expected columns", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
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
    dispatch_simulations(sim, use_future = FALSE, verbose = 0)
  )

  expected_cols <- c("model_name", "estimate", "se", "iter")
  expect_true(all(expected_cols %in% colnames(results)))
})
