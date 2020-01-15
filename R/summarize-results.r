summarize_results <- function(ConfigObject) {
  stopifnot("OpticConfig" %in% class(ConfigObject))
  UseMethod("summarize_results", ConfigObject)
}

# NULL
# (perform grouped by se_adjustment)
# TYPE 1 ERROR
# - mean of logical vector of pvalues that are above/below 0.05 threshhold
# SIMPLE MEAN SUMMARIES
# - mean of estimate, se, variance, t_stat
# MSE
# - mse of estimate

# CORRECTION FACTOR FROM NULL MODEL
# different version for GEE (method by model call)
#  - add Wald stat implementation for correction_factor
# - pass test statistic for model (e.g., t-stat for linear) to correction
#   factor method
# - group by se_adjustment

# CALCULATE BIAS
# - needs to be done differently by model, but remain bias in the regression coefficient
# - vector of regression estimates minus true effect on appropriate scale, mean of that
# - keep standardized bias (develop/use outside package)

# COVERAGE INTERVALS, TYPE-S, CORRECT REJECTION RATE
# power - correct_rejection_rate (ensure standard errors not variance)
# type-s error - need to pass in adjusted p values
# coverage - currently adjusts for correction factor, do we need it here though?
#   - implement flag to use_correction_factor default to FALSE, cf=NULL


#' @importFrom magrittr %>%
summarize_results.null <- function(ConfigObject) {
  all_iters <- ConfigObject$iter_results
  
  all_iters <- all_iters %>%
    dplyr::group_by(se_adjustment) %>%
    dplyr::summarize(
      type1_error = mean(pval_flag(p_value))
    )
}