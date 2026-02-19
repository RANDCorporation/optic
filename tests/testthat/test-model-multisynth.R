#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test multisynth (augmented synthetic control) model type

library(dplyr)
if (requireNamespace("augsynth", quietly = TRUE)) {
  library(augsynth)
}

test_data <- optic::overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_model creates valid multisynth model", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_basic",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "multisynth")
  expect_equal(model$model_call, "multisynth")
})

test_that("multisynth model requires unit and time parameters", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_params",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.5
  )

  expect_true(is.name(model$model_args$unit))
  expect_true(is.name(model$model_args$time))
  expect_equal(as.character(model$model_args$unit), "state")
  expect_equal(as.character(model$model_args$time), "year")
})

test_that("multisynth model stores lambda parameter", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_lambda",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.2
  )

  expect_equal(model$model_args$lambda, 0.2)
})

test_that("multisynth model supports fixedeff parameter", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_fixedeff",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1,
    fixedeff = TRUE
  )

  expect_equal(model$model_args$fixedeff, TRUE)
})

test_that("multisynth only works with multisynth call", {
  skip_if_not_installed("augsynth")

  expect_error(
    optic_model(
      name = "multisynth_invalid",
      type = "multisynth",
      call = "lm",
      formula = crude.rate ~ treatment_level,
      se_adjust = "none"
    )
  )
})

test_that("end-to-end: multisynth model runs through dispatch_simulations", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_e2e",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1,
    fixedeff = FALSE
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
  expect_true("multisynth_e2e" %in% results$model_name)
  expect_true(all(c("estimate", "se") %in% colnames(results)))
})

test_that("end-to-end: multisynth model with different lambda values", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_lambda_e2e",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.5,
    fixedeff = FALSE
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
    dispatch_simulations(sim, use_future = FALSE, seed = 444, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 1)
})

test_that("end-to-end: multisynth model with fixedeff = TRUE", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_fe_e2e",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1,
    fixedeff = TRUE
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
    dispatch_simulations(sim, use_future = FALSE, seed = 333, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 1)
})

test_that("end-to-end: multisynth model with confounding method", {
  skip_if_not_installed("augsynth")

  model <- optic_model(
    name = "multisynth_confounding_e2e",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1,
    fixedeff = FALSE
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
    dispatch_simulations(sim, use_future = FALSE, seed = 222, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("multisynth_confounding_e2e" %in% results$model_name)
})
