# Performance statistics for Monte Carlo simulation results
#
# Functions for computing bias, MSE, coverage, Type I error, Type S error,
# and related metrics from simulation output.

#' Rejection rate (Type I error under the null)
#'
#' Proportion of p-values below a significance threshold.
#'
#' @param p_values Numeric vector of p-values.
#' @param alpha Significance level (default 0.05).
#' @return Scalar: proportion of p-values < alpha.
#' @family simulation-performance
#' @export
sim_rejection_rate <- function(p_values, alpha = 0.05) {
  p_values <- p_values[!is.na(p_values)]
  mean(p_values < alpha)
}

#' Bias of point estimates
#'
#' Computes the difference between each estimate and the true effect.
#' Pass the signed true effect directly (negative for negative effects).
#'
#' @param estimates Numeric vector of point estimates.
#' @param true_effect Scalar true effect value.
#' @return Numeric vector of biases (estimate - true_effect).
#' @family simulation-performance
#' @export
sim_bias <- function(estimates, true_effect) {
  estimates - true_effect
}

#' Mean squared error
#'
#' Computes squared deviations of estimates from the true effect.
#'
#' @param estimates Numeric vector of point estimates.
#' @param true_effect Scalar true effect value.
#' @return Numeric vector of squared errors.
#' @family simulation-performance
#' @export
sim_mse <- function(estimates, true_effect) {
  (estimates - true_effect)^2
}

#' Empirical correction factor
#'
#' Computes the ratio of the empirical 95th percentile of squared t-statistics
#' to the theoretical chi-squared(1) 95th percentile. Used to calibrate
#' standard errors for coverage computation.
#'
#' @param test_stats Numeric vector of t-statistics.
#' @return Scalar correction factor. Values > 1 indicate model SEs are too small.
#' @family simulation-performance
#' @export
sim_correction_factor <- function(test_stats) {
  test_stats <- test_stats[!is.na(test_stats)]
  f_stats <- sort(test_stats^2)
  femp95 <- f_stats[floor(0.95 * length(f_stats))]
  freal <- stats::qf(0.95, 1, Inf)
  sqrt(femp95 / freal)
}

#' Coverage of confidence intervals
#'
#' Proportion of confidence intervals that contain the true effect.
#' Optionally applies an empirical correction factor to the standard errors
#' before constructing intervals.
#'
#' @param estimates Numeric vector of point estimates.
#' @param ses Numeric vector of standard errors.
#' @param true_effect Scalar true effect value (default 0).
#' @param correction_factor Scalar multiplier for SEs (default 1, no correction).
#' @param alpha Significance level for CI width (default 0.05 for 95\% CI).
#' @return Scalar coverage rate.
#' @family simulation-performance
#' @export
sim_coverage <- function(estimates, ses, true_effect = 0,
                         correction_factor = 1, alpha = 0.05) {
  adj_ses <- ses * correction_factor
  z <- stats::qnorm(1 - alpha / 2)
  low <- estimates - z * adj_ses
  high <- estimates + z * adj_ses
  mean(true_effect > low & true_effect < high, na.rm = TRUE)
}

#' Type S (sign) error rate
#'
#' Among statistically significant results, the proportion that have the
#' wrong sign relative to the true effect. Returns NA when true_effect is 0.
#'
#' @param estimates Numeric vector of point estimates.
#' @param p_values Numeric vector of p-values.
#' @param true_effect Scalar true effect value.
#' @param alpha Significance level (default 0.05).
#' @return Scalar Type S error rate, or NA for null effects.
#' @family simulation-performance
#' @export
sim_type_s_error <- function(estimates, p_values, true_effect, alpha = 0.05) {
  if (true_effect == 0) return(NA_real_)
  sig <- which(!is.na(p_values) & p_values < alpha)
  if (length(sig) == 0) return(0)
  if (true_effect > 0) {
    mean(estimates[sig] < 0)
  } else {
    mean(estimates[sig] > 0)
  }
}

#' Correct rejection rate
#'
#' For null effects: proportion of CIs that correctly contain zero.
#' For non-null effects: proportion of CIs that exclude zero AND have the
#' correct sign.
#'
#' @param estimates Numeric vector of point estimates.
#' @param ses Numeric vector of standard errors.
#' @param true_effect Scalar true effect value.
#' @param correction_factor Scalar multiplier for SEs (default 1).
#' @param alpha Significance level (default 0.05).
#' @return Scalar correct rejection rate.
#' @family simulation-performance
#' @export
sim_correct_rejection_rate <- function(estimates, ses, true_effect,
                                       correction_factor = 1, alpha = 0.05) {
  adj_ses <- ses * correction_factor
  z <- stats::qnorm(1 - alpha / 2)
  low <- estimates - z * adj_ses
  high <- estimates + z * adj_ses
  contains_zero <- (low < 0) & (high > 0)

  if (true_effect == 0) {
    mean(contains_zero, na.rm = TRUE)
  } else if (true_effect > 0) {
    correct <- (!contains_zero) & (estimates > 0)
    mean(correct, na.rm = TRUE)
  } else {
    correct <- (!contains_zero) & (estimates < 0)
    mean(correct, na.rm = TRUE)
  }
}

#' Summarize simulation results
#'
#' Computes all performance metrics from a data frame of simulation iteration
#' results. Returns a single-row data frame with bias, variance, MSE, rejection
#' rate, coverage, Type S error, and correct rejection rate.
#'
#' @param results Data frame with one row per simulation iteration.
#' @param true_effect Scalar true effect value (signed).
#' @param estimate_col Column name for point estimates (default "estimate").
#' @param se_col Column name for standard errors (default "se").
#' @param p_value_col Column name for p-values (default "p_value").
#' @param t_stat_col Column name for t-statistics (default "t_stat").
#' @return Single-row data frame with columns: mean_estimate, mean_bias,
#'   mean_se, mean_variance, simulated_variance, mse, rmse, rejection_rate,
#'   correction_factor, coverage, type_s_error, correct_rejection_rate, n_valid.
#' @family simulation-performance
#' @export
summarize_simulation <- function(results, true_effect,
                                 estimate_col = "estimate",
                                 se_col = "se",
                                 p_value_col = "p_value",
                                 t_stat_col = "t_stat") {
  est <- results[[estimate_col]]
  se <- results[[se_col]]
  pval <- results[[p_value_col]]
  tstat <- results[[t_stat_col]]

  valid <- !is.na(est)
  est_valid <- est[valid]
  se_valid <- se[valid]

  cf <- sim_correction_factor(tstat)

  data.frame(
    mean_estimate = mean(est_valid),
    mean_bias = mean(sim_bias(est_valid, true_effect)),
    mean_se = mean(se_valid),
    mean_variance = mean(se_valid^2),
    simulated_variance = stats::var(est_valid),
    mse = mean(sim_mse(est_valid, true_effect)),
    rmse = sqrt(mean(sim_mse(est_valid, true_effect))),
    rejection_rate = sim_rejection_rate(pval),
    correction_factor = cf,
    coverage = sim_coverage(est_valid, se_valid, true_effect, cf),
    type_s_error = sim_type_s_error(est_valid, pval[valid], true_effect),
    correct_rejection_rate = sim_correct_rejection_rate(
      est_valid, se_valid, true_effect, cf
    ),
    n_valid = sum(valid)
  )
}
