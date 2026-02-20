#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test optic_model function - general constructor and validation tests
# Model-type-specific tests are in test-model-*.R files

test_that("optic_model creates valid model object", {
  model <- optic_model(
    name = "test_model",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level + covariate,
    se_adjust = "none"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$name, "test_model")
  expect_equal(model$type, "reg")
  expect_equal(model$model_call, "lm")
  expect_equal(model$se_adjust, "none")
  expect_true(rlang::is_formula(model$model_formula))
})

test_that("optic_model creates list structure with required fields", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment,
    se_adjust = "none"
  )

  expect_true(is.list(model))
  expect_true(all(c("name", "type", "model_call", "model_formula", "se_adjust", "model_args") %in% names(model)))
})

test_that("optic_model accepts multiple se_adjust values", {
  model <- optic_model(
    name = "test_multi_se",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = c("none", "cluster")
  )

  expect_equal(model$se_adjust, c("none", "cluster"))
})

test_that("optic_model accepts all SE adjustment options", {
  se_options <- c("none", "cluster", "cluster-unit", "cluster-treat", "huber", "arellano")

  model <- optic_model(
    name = "test_all_se",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = se_options
  )

  expect_equal(model$se_adjust, se_options)
})

test_that("optic_model stores weights as name", {
  model <- optic_model(
    name = "test_weights",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = "none",
    weights = "population"
  )

  expect_true(is.name(model$model_args$weights))
  expect_equal(as.character(model$model_args$weights), "population")
})

test_that("optic_model accepts additional arguments via ...", {
  model <- optic_model(
    name = "test_args",
    type = "multisynth",
    call = "multisynth",
    formula = outcome ~ treatment,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.5,
    fixedeff = FALSE
  )

  expect_equal(model$model_args$lambda, 0.5)
  expect_equal(model$model_args$fixedeff, FALSE)
  expect_true(is.name(model$model_args$unit))
  expect_true(is.name(model$model_args$time))
})

test_that("optic_model handles complex formula", {
  model <- optic_model(
    name = "complex",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment_level,
    se_adjust = "cluster"
  )

  expect_s3_class(model, "optic_model")
  expect_true(rlang::is_formula(model$model_formula))
})

test_that("optic_model fails with missing required arguments", {
  expect_error(
    optic_model(
      type = "reg",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "name"
  )

  expect_error(
    optic_model(
      name = "test",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "type"
  )

  expect_error(
    optic_model(
      name = "test",
      type = "reg",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "call"
  )
})

test_that("optic_model fails with invalid type", {
  expect_error(
    optic_model(
      name = "test",
      type = "invalid_type",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "'arg' should be one of"
  )
})

test_that("optic_model fails with invalid call", {
  expect_error(
    optic_model(
      name = "test",
      type = "reg",
      call = "invalid_call",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "'arg' should be one of"
  )
})

test_that("optic_model fails with incompatible type and call combinations", {
  expect_error(
    optic_model(
      name = "test",
      type = "autoeffect",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "not compatible"
  )

  expect_error(
    optic_model(
      name = "test",
      type = "multisynth",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "not compatible"
  )

  expect_error(
    optic_model(
      name = "test",
      type = "did",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    ),
    "not compatible"
  )
})

test_that("optic_model fails with non-formula input", {
  expect_error(
    optic_model(
      name = "test",
      type = "reg",
      call = "lm",
      formula = "not a formula",
      se_adjust = "none"
    )
  )

  expect_error(
    optic_model(
      name = "test",
      type = "reg",
      call = "lm",
      formula = NULL,
      se_adjust = "none"
    )
  )
})

test_that("optic_model stores model_args correctly", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment,
    se_adjust = "none",
    custom_arg1 = "value1",
    custom_arg2 = 42
  )

  expect_true(is.list(model$model_args))
  expect_equal(model$model_args$custom_arg1, "value1")
  expect_equal(model$model_args$custom_arg2, 42)
})
