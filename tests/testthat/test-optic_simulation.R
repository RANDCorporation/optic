#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test optic_simulation function

library(dplyr)
data(overdoses)

simple_data <- overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_simulation creates valid no_confounding simulation", {
  model <- optic_model(
    name = "test_model",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model),
    iters = 5,
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

  expect_s3_class(sim, "OpticSim")
  expect_equal(sim$iters, 5)
})

test_that("optic_simulation creates valid concurrent simulation", {
  model <- optic_model(
    name = "concurrent_model",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment1_level + treatment2_level,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model),
    iters = 5,
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

  expect_s3_class(sim, "OpticSim")
})

test_that("optic_simulation creates valid confounding simulation", {
  model <- optic_model(
    name = "conf_model",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model),
    iters = 5,
    method = "confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    conf_var = "unemploymentrate",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = "null",
    policy_speed = "instant",
    n_implementation_periods = 1,
    bias_type = "linear",
    bias_size = "small",
    verbose = FALSE
  )

  expect_s3_class(sim, "OpticSim")
})

test_that("optic_simulation accepts multiple models", {
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
    se_adjust = "cluster"
  )

  sim <- optic_simulation(
    x = simple_data,
    models = list(model1, model2),
    iters = 5,
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

  expect_equal(length(sim$models), 2)
})

test_that("optic_simulation accepts multiple effect magnitudes", {
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
    iters = 5,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0, 0.5, 1.0),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  expect_true(nrow(sim$simulation_params) >= 3)
})

test_that("optic_simulation accepts multiple n_units", {
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
    iters = 5,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = c(5, 10),
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  expect_true(nrow(sim$simulation_params) >= 2)
})

test_that("optic_simulation accepts multiple effect directions", {
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
    iters = 5,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = c("pos", "neg"),
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  expect_true(nrow(sim$simulation_params) >= 2)
})

test_that("optic_simulation accepts multiple policy speeds", {
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
    iters = 5,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 5,
    effect_direction = "pos",
    policy_speed = c("instant", "slow"),
    n_implementation_periods = 3,
    verbose = FALSE
  )

  expect_true(nrow(sim$simulation_params) >= 2)
})

test_that("optic_simulation creates simulation parameter grid", {
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
    iters = 5,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0, 0.5),
    n_units = c(5, 10),
    effect_direction = c("pos", "neg"),
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  expected_scenarios <- 2 * 2 * 2
  expect_equal(nrow(sim$simulation_params), expected_scenarios)
})

test_that("optic_simulation fails without required parameters", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  expect_error(
    optic_simulation(
      x = simple_data,
      models = list(model),
      iters = 5,
      method = "no_confounding",
      time_var = "year",
      effect_magnitude = list(0),
      n_units = 5,
      effect_direction = "pos",
      policy_speed = "instant",
      n_implementation_periods = 1,
      verbose = FALSE
    )
  )
})

test_that("optic_simulation fails with invalid method", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  expect_error(
    optic_simulation(
      x = simple_data,
      models = list(model),
      iters = 5,
      method = "invalid_method",
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
  )
})

test_that("optic_simulation fails with invalid effect direction", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  expect_error(
    optic_simulation(
      x = simple_data,
      models = list(model),
      iters = 5,
      method = "no_confounding",
      unit_var = "state",
      treat_var = "state",
      time_var = "year",
      effect_magnitude = list(0),
      n_units = 5,
      effect_direction = "invalid",
      policy_speed = "instant",
      n_implementation_periods = 1,
      verbose = FALSE
    )
  )
})

test_that("optic_simulation fails with invalid policy speed", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  expect_error(
    optic_simulation(
      x = simple_data,
      models = list(model),
      iters = 5,
      method = "no_confounding",
      unit_var = "state",
      treat_var = "state",
      time_var = "year",
      effect_magnitude = list(0),
      n_units = 5,
      effect_direction = "pos",
      policy_speed = "invalid",
      n_implementation_periods = 1,
      verbose = FALSE
    )
  )
})

test_that("optic_simulation requires conf_var for confounding method", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none"
  )

  expect_error(
    optic_simulation(
      x = simple_data,
      models = list(model),
      iters = 5,
      method = "confounding",
      unit_var = "state",
      treat_var = "state",
      time_var = "year",
      effect_magnitude = list(0),
      n_units = 5,
      effect_direction = "null",
      policy_speed = "instant",
      n_implementation_periods = 1,
      bias_type = "linear",
      bias_size = "small",
      verbose = FALSE
    )
  )
})

test_that("optic_simulation requires rhos for concurrent method", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment1_level + treatment2_level,
    se_adjust = "none"
  )

  expect_error(
    optic_simulation(
      x = simple_data,
      models = list(model),
      iters = 5,
      method = "concurrent",
      unit_var = "state",
      time_var = "year",
      effect_magnitude = list(c(0, 0)),
      n_units = 5,
      effect_direction = "pos",
      policy_speed = "instant",
      n_implementation_periods = 1,
      years_apart = 2,
      ordered = TRUE,
      verbose = FALSE
    )
  )
})

test_that("optic_simulation print method works", {
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
    iters = 5,
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

  expect_output(print(sim), "Number of Simulations")
  expect_output(print(sim), "Number of Models")
})
