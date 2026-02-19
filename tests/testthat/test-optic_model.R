#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test optic_model function

test_that("optic_model creates valid reg model", {
  model <- optic_model(
    name = "test_reg",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level + covariate,
    se_adjust = "none"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$name, "test_reg")
  expect_equal(model$type, "reg")
  expect_equal(model$model_call, "lm")
  expect_equal(model$se_adjust, "none")
})

test_that("optic_model creates valid autoreg model", {
  model <- optic_model(
    name = "test_ar",
    type = "autoreg",
    call = "lm",
    formula = outcome ~ treatment_change + covariate,
    se_adjust = "cluster"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "autoreg")
  expect_equal(model$se_adjust, "cluster")
})

test_that("optic_model creates valid autoeffect model", {
  model <- optic_model(
    name = "test_autoeffect",
    type = "autoeffect",
    call = "autoeffect",
    formula = outcome ~ treatment,
    se_adjust = "none",
    lags = 2,
    unit_name = "unit",
    date_name = "year",
    trt_name = "trt_ind"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "autoeffect")
  expect_equal(model$model_call, "autoeffect")
  expect_equal(model$model_args$lags, 2)
})

test_that("optic_model creates valid multisynth model", {
  model <- optic_model(
    name = "test_multisynth",
    type = "multisynth",
    call = "multisynth",
    formula = outcome ~ treatment_level,
    se_adjust = "none",
    unit = as.name("state"),
    time = as.name("year"),
    lambda = 0.1
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "multisynth")
  expect_equal(model$model_call, "multisynth")
})

test_that("optic_model creates valid DID model", {
  model <- optic_model(
    name = "test_did",
    type = "did",
    call = "att_gt",
    formula = outcome ~ treatment,
    se_adjust = "none",
    yname = "outcome",
    tname = "year",
    idname = "state",
    gname = "treatment_date"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$type, "did")
  expect_equal(model$model_call, "att_gt")
})

test_that("optic_model creates model with feols call", {
  model <- optic_model(
    name = "test_feols",
    type = "reg",
    call = "feols",
    formula = outcome ~ treatment_level,
    se_adjust = "cluster"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$model_call, "feols")
})

test_that("optic_model creates model with glm.nb call", {
  model <- optic_model(
    name = "test_glmnb",
    type = "reg",
    call = "glm.nb",
    formula = count ~ treatment_level,
    se_adjust = "none"
  )

  expect_s3_class(model, "optic_model")
  expect_equal(model$model_call, "glm.nb")
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

test_that("optic_model accepts cluster-unit and cluster-treat se_adjust", {
  model1 <- optic_model(
    name = "test1",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = "cluster-unit"
  )

  model2 <- optic_model(
    name = "test2",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = "cluster-treat"
  )

  expect_equal(model1$se_adjust, "cluster-unit")
  expect_equal(model2$se_adjust, "cluster-treat")
})

test_that("optic_model accepts huber and arellano se_adjust", {
  model1 <- optic_model(
    name = "test1",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = "huber"
  )

  model2 <- optic_model(
    name = "test2",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment_level,
    se_adjust = "arellano"
  )

  expect_equal(model1$se_adjust, "huber")
  expect_equal(model2$se_adjust, "arellano")
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

test_that("optic_model accepts additional arguments", {
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
})

test_that("optic_model fails with missing required arguments", {
  expect_error(
    optic_model(
      type = "reg",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    )
  )

  expect_error(
    optic_model(
      name = "test",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    )
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
    )
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
    )
  )
})

test_that("optic_model fails with incompatible type and call", {
  expect_error(
    optic_model(
      name = "test",
      type = "autoeffect",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    )
  )

  expect_error(
    optic_model(
      name = "test",
      type = "multisynth",
      call = "lm",
      formula = outcome ~ treatment,
      se_adjust = "none"
    )
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
})

test_that("optic_model creates list structure", {
  model <- optic_model(
    name = "test",
    type = "reg",
    call = "lm",
    formula = outcome ~ treatment,
    se_adjust = "none"
  )

  expect_true(is.list(model))
  expect_true(all(c("name", "type", "model_call", "model_formula", "se_adjust") %in% names(model)))
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
