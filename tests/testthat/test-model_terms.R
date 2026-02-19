#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test model_terms function

test_that("model_terms extracts LHS and RHS correctly for simple formula", {
  form <- formula(outcome ~ treatment + confounder)
  result <- model_terms(form)

  expect_equal(result$lhs, "outcome")
  expect_equal(result$rhs, c("treatment", "confounder"))
})

test_that("model_terms handles formula with single predictor", {
  form <- formula(y ~ x)
  result <- model_terms(form)

  expect_equal(result$lhs, "y")
  expect_equal(result$rhs, "x")
})

test_that("model_terms handles formula with factor terms", {
  form <- formula(crude.rate ~ as.factor(year) + as.factor(state) + treatment_level)
  result <- model_terms(form)

  expect_equal(result$lhs, "crude.rate")
  expect_true("year" %in% result$rhs)
  expect_true("state" %in% result$rhs)
  expect_true("treatment_level" %in% result$rhs)
})

test_that("model_terms handles formula with interaction terms", {
  form <- formula(y ~ x1 + x2 + x1:x2)
  result <- model_terms(form)

  expect_equal(result$lhs, "y")
  expect_true(all(c("x1", "x2") %in% result$rhs))
})

test_that("model_terms handles formula with polynomial terms", {
  form <- formula(y ~ x + I(x^2) + I(x^3))
  result <- model_terms(form)

  expect_equal(result$lhs, "y")
  expect_true("x" %in% result$rhs)
})

test_that("model_terms returns list with correct names", {
  form <- formula(outcome ~ predictor)
  result <- model_terms(form)

  expect_true(is.list(result))
  expect_equal(names(result), c("lhs", "rhs"))
})

test_that("model_terms fails with non-formula input", {
  expect_error(model_terms("not a formula"))
  expect_error(model_terms(NULL))
  expect_error(model_terms(123))
})

test_that("model_terms handles complex formula from real use case", {
  form <- formula(crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment_level)
  result <- model_terms(form)

  expect_equal(result$lhs, "crude.rate")
  expect_true(all(c("unemploymentrate", "year", "state", "treatment_level") %in% result$rhs))
})
