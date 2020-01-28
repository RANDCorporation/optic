#' @export
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
#' @export
summarize_results.lm <- function(ConfigObject) {
  if (ConfigObject$effect_direction == "null") {
    te <- effect_magnitude(ConfigObject)
    all_iters <- ConfigObject$iter_results
    
    # general summary information
    summary_info <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::summarize(
        type1_error = mean(pval_flag(p_value)),
        estimate = mean(estimate),
        se = mean(se),
        variance = mean(variance),
        # TODO: change this to depend on method call, grab correct stat column to summarize
        t_stat = mean(t_stat),
        mse = mean(mse),
        bias = mean(estimate - te)
      )
    
    # correction factors from null model
    correction_factors <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::summarize(correction_factor = correction_factor(t_stat))
    
    # extra summary information
    power_info <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::group_modify(~as.data.frame(correct_rejection_rate(
        .x$estimate,
        .x$se,
        correction_factors$correction_factor[correction_factors$se_adjustment == .y$se_adjustment],
        ConfigObject$effect_direction))
      )
    names(power_info) <- c("se_adjustment", "power")
    
    typeS_error <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::group_modify(~as.data.frame(type_s_error(
        .x$estimate,
        .x$p_value,
        ConfigObject$effect_direction
      )))
    names(typeS_error) <- c("se_adjustment", "type_s_error")
    
    summary_info <- dplyr::left_join(summary_info, power_info, by="se_adjustment")
    summary_info <- dplyr::left_join(summary_info, typeS_error, by="se_adjustment")
    
    return(summary_info)
  }
}

#' @importFrom magrittr %>%
#' @export
summarize_results.glm.nb <- function(ConfigObject) {
  if (ConfigObject$effect_direction == "null") {
    te <- effect_magnitude(ConfigObject)
    all_iters <- ConfigObject$iter_results
    
    # general summary information
    summary_info <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::summarize(
        type1_error = mean(pval_flag(p_value)),
        estimate = mean(estimate),
        se = mean(se),
        variance = mean(variance),
        # TODO: change this to depend on method call, grab correct stat column to summarize
        z_stat = mean(z_stat),
        mse = mean(mse),
        bias = mean(estimate - te)
      )
    
    # correction factors from null model
    correction_factors <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::summarize(correction_factor = correction_factor(z_stat))
    
    # extra summary information
    power_info <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::group_modify(~as.data.frame(correct_rejection_rate(
        .x$estimate,
        .x$se,
        correction_factors$correction_factor[correction_factors$se_adjustment == .y$se_adjustment],
        ConfigObject$effect_direction))
      )
    names(power_info) <- c("se_adjustment", "power")
    
    typeS_error <- all_iters %>%
      dplyr::group_by(se_adjustment) %>%
      dplyr::group_modify(~as.data.frame(type_s_error(
        .x$estimate,
        .x$p_value,
        ConfigObject$effect_direction
      )))
    names(typeS_error) <- c("se_adjustment", "type_s_error")
    
    summary_info <- dplyr::left_join(summary_info, power_info, by="se_adjustment")
    summary_info <- dplyr::left_join(summary_info, typeS_error, by="se_adjustment")
    
    return(summary_info)
  }
}

