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
  all_iters <- ConfigObject$iter_results
  if (ConfigObject$concurrent) {
    one <- names(all_iters)[grepl(".*?1$", names(all_iters))]
    two <- names(all_iters)[grepl(".*?2$", names(all_iters))]
    joint <- names(all_iters)[grepl("^joint", names(all_iters))]
    one <- all_iters[, c("se_adjustment", one)]
    names(one) <- c("se_adjustment", "estimate", "se", "variance", "t_stat", "p_value")
    one$joint_model <- "concurrent-1"
    two <- all_iters[, c("se_adjustment", two)]
    names(two) <- c("se_adjustment", "estimate", "se", "variance", "t_stat", "p_value")
    two$joint_model <- "concurrent-2"
    joint <- all_iters[, c("se_adjustment", joint)]
    names(joint) <- c("se_adjustment", "estimate", "se","variance", "t_stat", "p_value")
    joint$joint_model <- "joint"
    all_iters <- dplyr::bind_rows(one, two, joint)
    
    grouping_vars <- c("se_adjustment", "joint_model")
  } else {
    grouping_vars <- "se_adjustment"
  }
  
  groups <- rlang::syms(grouping_vars)
  
  te <- effect_magnitude(ConfigObject)
  
  # general summary information
  summary_info <- all_iters %>%
    dplyr::group_by(!!!groups) %>%
    dplyr::summarize(
      type1_error = mean(pval_flag(p_value)),
      estimate = mean(estimate),
      se = mean(se),
      variance = mean(variance),
      # TODO: change this to depend on method call, grab correct stat column to summarize
      t_stat = mean(t_stat),
      #for concurrent - we need to have joint.te = 2*te; how do we adjust?
      mse = mean((estimate - te)^2),
      bias = mean(estimate - te) 
    )
  
  if (ConfigObject$concurrent) {
  summary_info_joint <- all_iters %>%
    dplyr::group_by(!!!groups) %>%
    dplyr::summarize(
      type1_error = mean(pval_flag(p_value)),
      estimate = mean(estimate),
      se = mean(se),
      variance = mean(variance),
      # TODO: change this to depend on method call, grab correct stat column to summarize
      t_stat = mean(t_stat),
      #for concurrent - we need to have joint.te = 2*te; how do we adjust?
      mse = mean((estimate - 2*te)^2),
      bias = mean(estimate - 2*te) 
    )
  
  summary_info[summary_info$joint_model=="joint",]=summary_info_joint[summary_info_joint$joint_model=="joint",]
  }
  
  if (ConfigObject$effect_direction == "null") {
    # correction factors from null model
    correction_factors <- all_iters %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::summarize(correction_factor = correction_factor(t_stat))
    
    # assign to object
    ConfigObject$correction_factors <- correction_factors
  } else {
    if (is.null(ConfigObject$correction_factors)) {
      stop("Must provide correction factors when effect direction is 'pos' or 'neg'")
    }
    
    correction_factors <- ConfigObject$correction_factors
    # remove type 1 error from summary
    summary_info$type1_error <- NULL
  }
  
  # correct rejections rate (power)
 power_info <- dplyr::left_join(all_iters, correction_factors, by=grouping_vars)
   power_info <- power_info %>%
    dplyr::group_by(!!!groups) %>%
    dplyr::group_modify(~as.data.frame(correct_rejection_rate(
      coeffs=.x$estimate,
      ses=.x$se,
      cf=.x$correction_factor[1],
      effect_direction=ConfigObject$effect_direction))
    )
  names(power_info) <- c(grouping_vars, "power")
  
  typeS_error <- all_iters %>%
    dplyr::group_by(!!!groups) %>%
    dplyr::group_modify(~as.data.frame(type_s_error(
      .x$estimate,
      .x$p_value,
      ConfigObject$effect_direction
    )))
  names(typeS_error) <- c(grouping_vars, "type_s_error")
  
  summary_info <- dplyr::left_join(summary_info, power_info, by=grouping_vars)
  summary_info <- dplyr::left_join(summary_info, typeS_error, by=grouping_vars)
  
  if (ConfigObject$effect_direction != "null") {
    coverage <- dplyr::left_join(all_iters, correction_factors, by=grouping_vars)
    coverage <- coverage %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::group_modify(~as.data.frame(coverage(
        .x$estimate,
        .x$se,
        te,
        cf=.x$correction_factor[1],
        use_cf=TRUE
      )))
    names(coverage) <- c(grouping_vars, "coverage")
    summary_info <- dplyr::left_join(summary_info, coverage, by=grouping_vars)
  }
  
  return(summary_info)
}

#' @importFrom magrittr %>%
#' @export
summarize_results.glm.nb <- function(ConfigObject) {
  all_iters <- ConfigObject$iter_results
  if (ConfigObject$concurrent) {
    one <- names(all_iters)[grepl(".*?1$", names(all_iters))]
    two <- names(all_iters)[grepl(".*?2$", names(all_iters))]
    joint <- names(all_iters)[grepl("^joint", names(all_iters))]
    one <- all_iters[, c("se_adjustment", one)]
    names(one) <- c("se_adjustment", "estimate", "se", "variance", "t_stat", "p_value")
    one$joint_model <- "concurrent-1"
    two <- all_iters[, c("se_adjustment", two)]
    names(two) <- c("se_adjustment", "estimate", "se", "variance", "t_stat", "p_value")
    two$joint_model <- "concurrent-2"
    joint <- all_iters[, c("se_adjustment", joint)]
    names(joint) <- c("se_adjustment", "estimate", "se", "p_value")
    joint$joint_model <- "joint"
    all_iters <- dplyr::bind_rows(one, two, joint)
    
    grouping_vars <- c("se_adjustment", "joint_model")
  } else {
    grouping_vars <- "se_adjustment"
  }
  
  groups <- rlang::syms(grouping_vars)
  
  te <- effect_magnitude(ConfigObject)
  
  # general summary information
  summary_info <- all_iters %>%
    dplyr::group_by(!!!groups) %>%
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
  
  
  if (ConfigObject$effect_direction == "null") {
    # correction factors from null model
    correction_factors <- all_iters %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::summarize(correction_factor = correction_factor(t_stat))
    
    # assign to object
    ConfigObject$correction_factors <- correction_factors
  } else {
    if (is.null(ConfigObject$correction_factors)) {
      stop("Must provide correction factors when effect direction is 'pos' or 'neg'")
    }
    
    correction_factors <- ConfigObject$correction_factors
    # remove type 1 error from summary
    summary_info$type1_error <- NULL
  }
  
  # correct rejections rate (power)
  power_info <- dplyr::left_join(all_iters, correction_factors, by=grouping_vars)
  power_info <- power_info %>%
    dplyr::group_by(!!!groups) %>%
    dplyr::group_modify(~as.data.frame(correct_rejection_rate(
      .x$estimate,
      .x$se,
      .x$correction_factor[1],
      ConfigObject$effect_direction))
    )
  names(power_info) <- c(grouping_vars, "power")
  
  typeS_error <- all_iters %>%
    dplyr::group_by(!!!groups) %>%
    dplyr::group_modify(~as.data.frame(type_s_error(
      .x$estimate,
      .x$p_value,
      ConfigObject$effect_direction
    )))
  names(typeS_error) <- c(grouping_vars, "type_s_error")
  
  summary_info <- dplyr::left_join(summary_info, power_info, by=grouping_vars)
  summary_info <- dplyr::left_join(summary_info, typeS_error, by=grouping_vars)
  
  if (ConfigObject$effect_direction != "null") {
    coverage <- dplyr::left_join(all_iters, correction_factors, by=grouping_vars)
    coverage <- coverage %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::group_modify(~as.data.frame(coverage(
        .x$estimate,
        .x$se,
        te,
        cf=.x$correction_factor[1],
        use_cf=TRUE
      )))
    names(coverage) <- c(grouping_vars, "coverage")
    summary_info <- dplyr::left_join(summary_info, coverage, by=grouping_vars)
  }
  
  return(summary_info)
}

