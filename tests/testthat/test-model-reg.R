#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test reg (regression) model type

library(dplyr)

test_data <- optic::overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_model creates valid reg model with lm", {
  model <- optic_model(
    name = "reg_lm",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate + as.factor(year),
    se_adjust = "none"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "reg")
  expect_equal(model$model_call, "lm")
})

test_that("optic_model creates valid reg model with feols", {
  model <- optic_model(
    name = "reg_feols",
    type = "reg",
    call = "feols",
    formula = crude.rate ~ treatment_level + unemploymentrate | year + state,
    se_adjust = "cluster"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "reg")
  expect_equal(model$model_call, "feols")
})

test_that("optic_model creates valid reg model with glm.nb", {
  model <- optic_model(
    name = "reg_glmnb",
    type = "reg",
    call = "glm.nb",
    formula = crude.rate ~ treatment_level + unemploymentrate,
    se_adjust = "none"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "reg")
  expect_equal(model$model_call, "glm.nb")
})

test_that("reg model supports all SE adjustments", {
  se_options <- c("none", "cluster", "cluster-unit", "cluster-treat", "huber", "arellano")

  model <- optic_model(
    name = "reg_multi_se",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = se_options
  )

  expect_equal(model$se_adjust, se_options)
})

test_that("reg model with weights", {
  model <- optic_model(
    name = "reg_weighted",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    weights = "population"
  )

  expect_true(is.name(model$model_args$weights))
  expect_equal(as.character(model$model_args$weights), "population")
})

test_that("end-to-end: reg model with lm runs through dispatch_simulations", {
  model <- optic_model(
    name = "reg_lm_e2e",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
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
    dispatch_simulations(sim, use_future = FALSE, seed = 123, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("reg_lm_e2e" %in% results$model_name)
  expect_true(all(c("estimate", "se", "iter") %in% colnames(results)))
  expect_equal(nrow(results), 2)
})

test_that("end-to-end: reg model with feols runs through dispatch_simulations", {
  skip_if_not_installed("fixest")
  library(fixest)

  model <- optic_model(
    name = "reg_feols_e2e",
    type = "reg",
    call = "feols",
    formula = crude.rate ~ treatment_level + unemploymentrate | year + state,
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
    dispatch_simulations(sim, use_future = FALSE, seed = 123, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("reg_feols_e2e" %in% results$model_name)
  expect_equal(nrow(results), 2)
})

test_that("end-to-end: reg model with multiple SE adjustments", {
  model <- optic_model(
    name = "reg_multi_se_e2e",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
    se_adjust = c("none", "cluster-unit", "huber")
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
    dispatch_simulations(sim, use_future = FALSE, seed = 123, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 2 * 3)
  expect_true(all(c("none", "cluster-unit", "huber") %in% results$se_adjustment))
})

test_that("end-to-end: reg model with confounding method", {
  model <- optic_model(
    name = "reg_confounding_e2e",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
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
    dispatch_simulations(sim, use_future = FALSE, seed = 234, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("reg_confounding_e2e" %in% results$model_name)
})

test_that("end-to-end: reg model with concurrent method", {
  skip("Concurrent method has bug: effect_magnitude not split into effect_magnitude1/2")

  model <- optic_model(
    name = "reg_concurrent_e2e",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ treatment1_level + treatment2_level + unemploymentrate + as.factor(year) + as.factor(state),
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
    dispatch_simulations(sim, use_future = FALSE, seed = 345, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("reg_concurrent_e2e" %in% results$model_name)
})
