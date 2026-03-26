test_that("sim_rejection_rate computes proportion below alpha", {
  expect_equal(sim_rejection_rate(c(0.01, 0.04, 0.06, 0.10), 0.05), 0.5)
  expect_equal(sim_rejection_rate(c(0.01, 0.02, 0.03), 0.05), 1.0)
  expect_equal(sim_rejection_rate(c(0.10, 0.20, 0.30), 0.05), 0.0)
  expect_equal(sim_rejection_rate(c(0.01, NA, 0.10), 0.05), 0.5)
})

test_that("sim_bias returns estimate minus true effect", {
  expect_equal(sim_bias(c(1, 2, 3), 2), c(-1, 0, 1))
  expect_equal(sim_bias(c(-5, -4), -5), c(0, 1))
  expect_equal(sim_bias(c(0, 0, 0), 0), c(0, 0, 0))
})

test_that("sim_mse returns squared deviations", {
  expect_equal(sim_mse(c(1, 2, 3), 2), c(1, 0, 1))
  expect_equal(sim_mse(c(0, 0), 1), c(1, 1))
})

test_that("sim_correction_factor produces sensible values", {
  set.seed(1)
  # Well-calibrated t-stats from N(0,1): CF should be near 1
  well_calibrated <- rnorm(10000)
  cf <- sim_correction_factor(well_calibrated)
  expect_true(cf > 0.9 && cf < 1.1)

  # Inflated t-stats (SEs too small): CF should be > 1
  inflated <- rnorm(10000, sd = 2)
  cf_inflated <- sim_correction_factor(inflated)
  expect_true(cf_inflated > 1.5)

  # NAs are removed
  expect_equal(
    sim_correction_factor(c(well_calibrated, NA, NA)),
    cf
  )
})

test_that("sim_coverage computes CI containment rate", {
  # All CIs contain true effect
  est <- c(0.1, -0.1, 0.05)
  ses <- c(1, 1, 1)
  expect_equal(sim_coverage(est, ses, true_effect = 0), 1.0)

  # No CIs contain true effect (true_effect far away)
  expect_equal(sim_coverage(est, ses, true_effect = 100), 0.0)

  # Correction factor widens CIs
  narrow_ses <- c(0.01, 0.01, 0.01)
  expect_equal(sim_coverage(est, narrow_ses, true_effect = 0), 0.0)
  expect_equal(
    sim_coverage(est, narrow_ses, true_effect = 0, correction_factor = 100),
    1.0
  )
})

test_that("sim_type_s_error returns NA for null, rate for non-null", {
  expect_true(is.na(sim_type_s_error(c(1, -1), c(0.01, 0.01), true_effect = 0)))

  # Positive true effect, all significant estimates are positive: no sign error
  expect_equal(
    sim_type_s_error(c(1, 2, 3), c(0.01, 0.01, 0.01), true_effect = 1),
    0
  )

  # Positive true effect, one significant estimate is negative: 1/3 sign error
  expect_equal(
    sim_type_s_error(c(1, 2, -1), c(0.01, 0.01, 0.01), true_effect = 1),
    1 / 3
  )

  # No significant results: 0

  expect_equal(
    sim_type_s_error(c(1, 2), c(0.5, 0.5), true_effect = 1),
    0
  )
})

test_that("sim_correct_rejection_rate works for null and non-null", {
  # Null effect: CI contains zero -> correct
  est <- c(0.1, -0.1, 0.05)
  ses <- c(1, 1, 1)
  expect_equal(sim_correct_rejection_rate(est, ses, true_effect = 0), 1.0)

  # Positive effect, CI excludes zero AND positive estimate -> correct
  est_pos <- c(5, 6, 7)
  ses_small <- c(0.1, 0.1, 0.1)
  expect_equal(sim_correct_rejection_rate(est_pos, ses_small, true_effect = 5), 1.0)

  # Positive effect, negative estimate -> incorrect even if CI excludes zero
  est_neg <- c(-5, -6, -7)
  expect_equal(sim_correct_rejection_rate(est_neg, ses_small, true_effect = 5), 0.0)
})

test_that("summarize_simulation returns complete summary", {
  set.seed(42)
  n <- 100
  results <- data.frame(
    estimate = rnorm(n, mean = 0, sd = 0.5),
    se = rep(0.5, n),
    p_value = runif(n),
    t_stat = rnorm(n)
  )

  summary <- summarize_simulation(results, true_effect = 0)

  expect_true(is.data.frame(summary))
  expect_equal(nrow(summary), 1)
  expected_cols <- c(
    "mean_estimate", "mean_bias", "mean_se", "mean_variance",
    "simulated_variance", "mse", "rmse", "rejection_rate",
    "correction_factor", "coverage", "type_s_error",
    "correct_rejection_rate", "n_valid"
  )
  expect_equal(names(summary), expected_cols)
  expect_equal(summary$n_valid, n)
  expect_true(is.na(summary$type_s_error))  # null effect
})

test_that("summarize_simulation handles custom column names", {
  results <- data.frame(
    beta = c(1, 2, 3),
    std_err = c(0.5, 0.5, 0.5),
    pval = c(0.01, 0.5, 0.8),
    tval = c(2, 1, 0.5)
  )

  summary <- summarize_simulation(
    results, true_effect = 2,
    estimate_col = "beta", se_col = "std_err",
    p_value_col = "pval", t_stat_col = "tval"
  )

  expect_equal(summary$n_valid, 3)
  expect_equal(summary$mean_estimate, 2)
  expect_equal(summary$mean_bias, 0)
})
