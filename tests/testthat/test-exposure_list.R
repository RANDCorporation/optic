#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Test exposure_list function

test_that("exposure_list returns correct structure with instant policy", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 10,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_true(is.list(result))
  expect_equal(names(result), c("policy_years", "policy_month", "exposure"))
  expect_equal(result$policy_month, 7)
})

test_that("exposure_list instant policy produces correct years", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 5,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_equal(result$policy_years, 1:5)
})

test_that("exposure_list instant policy has correct first year exposure", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 5,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_equal(result$exposure[1], (12 - 7 + 1) / 12)
})

test_that("exposure_list instant policy full exposure after first year", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 5,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_true(all(result$exposure[-1] == 1))
})

test_that("exposure_list slow policy returns correct structure", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 10,
    policy_speed = "slow",
    n_implementation_periods = 5
  )

  expect_true(is.list(result))
  expect_equal(names(result), c("policy_years", "policy_month", "exposure"))
})

test_that("exposure_list slow policy has gradual increase", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 10,
    policy_speed = "slow",
    n_implementation_periods = 5
  )

  expect_true(result$exposure[1] < result$exposure[2])
  expect_true(result$exposure[2] < result$exposure[3])
})

test_that("exposure_list slow policy reaches full effect eventually", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 10,
    policy_speed = "slow",
    n_implementation_periods = 3
  )

  expect_true(any(result$exposure >= 0.99))
})

test_that("exposure_list handles January start", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 1,
    available_periods = 5,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_equal(result$exposure[1], 1)
})

test_that("exposure_list handles December start", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 12,
    available_periods = 5,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_equal(result$exposure[1], 1/12)
})

test_that("exposure_list handles late period start", {
  result <- exposure_list(
    sampled_time_period = 8,
    mo = 6,
    available_periods = 10,
    policy_speed = "instant",
    n_implementation_periods = 1
  )

  expect_equal(result$policy_years, 8:10)
  expect_equal(length(result$exposure), 3)
})

test_that("exposure_list slow policy with short available periods", {
  result <- exposure_list(
    sampled_time_period = 1,
    mo = 7,
    available_periods = 3,
    policy_speed = "slow",
    n_implementation_periods = 5
  )

  expect_equal(length(result$policy_years), 3)
  expect_equal(length(result$exposure), 3)
  expect_true(all(result$exposure < 1))
})

test_that("exposure_list exposure vector matches policy years length", {
  result <- exposure_list(
    sampled_time_period = 2,
    mo = 8,
    available_periods = 7,
    policy_speed = "slow",
    n_implementation_periods = 3
  )

  expect_equal(length(result$exposure), length(result$policy_years))
})
