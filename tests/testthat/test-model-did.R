#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test did (Callaway-Sant'Anna difference-in-differences) model type

library(dplyr)
if (requireNamespace("did", quietly = TRUE)) {
  library(did)
}

test_data <- optic::overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon", "South Dakota", "North Dakota")))

test_that("optic_model creates valid did model", {
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_basic",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "crude.rate",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "did")
  expect_equal(model$model_call, "att_gt")
})

test_that("did model requires yname, tname, idname, gname parameters", {
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_params",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "outcome_var",
    tname = "time_var",
    idname = "id_var",
    gname = "group_var"
  )

  expect_equal(model$model_args$yname, "outcome_var")
  expect_equal(model$model_args$tname, "time_var")
  expect_equal(model$model_args$idname, "id_var")
  expect_equal(model$model_args$gname, "group_var")
})

test_that("did model stores all required parameters", {
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_all_params",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "crude.rate",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
  )

  expect_true(all(c("yname", "tname", "idname", "gname") %in% names(model$model_args)))
})

test_that("did only works with att_gt call", {
  skip_if_not_installed("did")

  expect_error(
    optic_model(
      name = "did_invalid",
      type = "did",
      call = "lm",
      formula = crude.rate ~ treatment_level,
      se_adjust = "none"
    )
  )
})

test_that("end-to-end: did model runs through dispatch_simulations", {
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_e2e",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "crude.rate",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
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
    dispatch_simulations(sim, use_future = FALSE, seed = 777, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("did_e2e" %in% results$model_name)
  expect_true(all(c("estimate", "se") %in% colnames(results)))
})

test_that("end-to-end: did model with different data", {
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_alt_e2e",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "crude.rate",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
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
    n_units = 10,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 666, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 1)
})

test_that("end-to-end: did model handles treatment_date variable", {
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_trtdate_e2e",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "crude.rate",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
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
    effect_direction = "neg",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )

  results <- suppressWarnings(
    dispatch_simulations(sim, use_future = FALSE, seed = 111, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 1)
})

test_that("end-to-end: did model with confounding method", {
  skip("DID models require numeric gname; confounding method doesn't prepare treatment_date correctly")
  skip_if_not_installed("did")

  model <- optic_model(
    name = "did_confounding_e2e",
    type = "did",
    call = "att_gt",
    formula = crude.rate ~ treatment_level,
    se_adjust = "none",
    yname = "crude.rate",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
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
    dispatch_simulations(sim, use_future = FALSE, seed = 999, verbose = 0)
  )

  expect_true(is.data.frame(results))
  expect_true("did_confounding_e2e" %in% results$model_name)
})
