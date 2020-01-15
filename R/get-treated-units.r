#' get treated units
#' 
#' @importFrom magrittr %>%
#' 
#' @export
get_treated_units <- function(ConfigObject, policy_speed) {
  # randomly sample the states
  states <- sample(unique(ConfigObject$data$state), ConfigObject$n_states, replace=FALSE)
  
  # randomly sample the year and month the policy will take effect for each state
  treated <- list()
  for (state in states) {
    if (is.null(ConfigObject$time_periods)) {
      available_years <- unique(ConfigObject$data$year)
    } else {
      available_years <- unique(
        ConfigObject$data %>%
          dplyr::filter(year %in% ConfigObject$time_periods) %>%
          dplyr::pull(year)
      )
    }
    
    yr <- sample(available_years, 1)
    mo <- sample(1:12, 1)
    
    if (policy_speed == "slow") {
      treated[[state]] <- list(
        policy_years = yr:max(ConfigObject$data$year, na.rm=TRUE),
        policy_month = mo,
        exposure = calculate_exposure(mo, ConfigObject$number_implementation_years)
      )
      
      n <- length(treated[[state]][["policy_years"]])
      exposure <- treated[[state]][["exposure"]]
      if (n < length(exposure)) {
        treated[[state]][["exposure"]] <- exposure[1:n]
      } else {
        n_more_years <- n - length(exposure)
        treated[[state]][["exposure"]] <- c(exposure, rep(1, n_more_years))
      }
    } else if (policy_speed == "instant") {
      treated[[state]] <- list(
        policy_years = yr:max(ConfigObject$data$year, na.rm=TRUE),
        policy_month = mo,
        exposure = c((12 - mo + 1)/12, rep(1, length((yr + 1):max(ConfigObject$data$year, na.rm=TRUE))))
      )
    }
  }
  
  return(treated)
}
