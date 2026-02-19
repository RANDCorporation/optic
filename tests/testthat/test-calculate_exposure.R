#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test calculate_exposure function

test_that("calculate_exposure returns correct vector length", {
  result <- calculate_exposure(month = 7, n_years = 10)
  expect_equal(length(result), 11)
})

test_that("calculate_exposure works with January start", {
  result <- calculate_exposure(month = 1, n_years = 5)
  expect_equal(length(result), 6)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

test_that("calculate_exposure works with December start", {
  result <- calculate_exposure(month = 12, n_years = 3)
  expect_equal(length(result), 4)
  expect_true(all(result >= 0))
})

test_that("calculate_exposure works with mid-year start (July)", {
  result <- calculate_exposure(month = 7, n_years = 10)
  expect_equal(length(result), 11)
  expect_true(result[1] < result[2])
  expect_true(result[length(result) - 1] < result[length(result)])
})

test_that("calculate_exposure produces monotonically increasing values", {
  result <- calculate_exposure(month = 7, n_years = 10)
  for (i in 2:length(result)) {
    expect_true(result[i] >= result[i-1])
  }
})

test_that("calculate_exposure handles single year implementation", {
  result <- calculate_exposure(month = 6, n_years = 1)
  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

test_that("calculate_exposure with custom monthly effect", {
  result <- calculate_exposure(month = 7, n_years = 5, monthly_effect = 0.02)
  expect_equal(length(result), 6)
  expect_true(all(is.finite(result)))
})

test_that("calculate_exposure matches expected exposure pattern", {
  result <- calculate_exposure(month = 7, n_years = 10)
  expect_true(result[1] < 0.1)
  expect_true(result[length(result)] > 0.9)
})
