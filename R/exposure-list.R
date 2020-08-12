#' create exposure values across time periods for sampled treated units
#'
#' @param sampled_time_period year that policy is first enacted
#' @param mo month that policy is first enacted
#' @available_periods all available time periods in the data
#' @policy_speed either "instant" for the policy going into immediate effect or "slow"
#'     for the policy effect phasing in over time
#' @n_implementation_periods used if policy_speed is slow, number of time periods over
#'     which to phase in full policy effect
#'
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
      exposure = calculate_exposure(mo, n_implementation_periods)
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
