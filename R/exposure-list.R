

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Applies a time-varying treatment effect 
#' 
#' @description Simulates a time-varying treatment effect that starts at zero in time period zero, then linearly increases to a 'full treatment' effect, based on
#' analyst-provided choices concerning time until full treatment effect and 'speed'
#'
#' @param sampled_time_period Year that treatment is first enacted
#' @param mo Month that treatment is first enacted
#' @param available_periods Maximum number of time periods in the data (e.g. if policy is between 1950-2000, then available_periods == 50)
#' @param policy_speed A string which is either "instant" for the policy going into immediate effect or "slow"
#'     for the policy effect phasing in linearly across n_implement_periods 
#' @param n_implementation_periods Number of periods until full treatment effect is applied. Only used if policy_speed is 'slow'.
#' @returns A list, containing a vector of policy years of implementation, an integer of the starting policy implementation month,
#'          and the effect of treatment within a given implementation year (as a fraction of the total policy effect)
#' @examples
#' # Set up a policy that starts in first-year of data, in July and takes 
#' # 2 years for full implementation:
#' exposure_list(1, 7, 3, policy_speed = 'slow', n_implementation_periods = 2)
#' 
#' # Same scenario but effect happens instantaneously:
#' exposure_list(1, 7, 3, policy_speed = 'instant')
#' @export
exposure_list <- function(sampled_time_period, mo, available_periods, policy_speed, n_implementation_periods) {
  if (policy_speed == "instant") {
    l <- list(
      policy_years = sampled_time_period:max(available_periods, na.rm=TRUE),
      policy_month = mo,
      exposure = c((12 - mo + 1)/12, rep(1, length((sampled_time_period + 1):max(available_periods, na.rm=TRUE))))
    )
  } else if (policy_speed == "slow") {
    l <- list(
      policy_years = sampled_time_period:max(available_periods, na.rm=TRUE),
      policy_month = mo,
      exposure = optic::calculate_exposure(mo, n_implementation_periods)
    )
    
    n <- length(l[["policy_years"]])
    exposure <- l[["exposure"]]
    if (n < length(exposure)) {
      l[["exposure"]] <- exposure[1:n]
    } else {
      n_more_years <- n - length(exposure)
      l[["exposure"]] <- c(exposure, rep(1, n_more_years))
    }
  }
  
  return(l)
}
