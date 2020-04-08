#' TODO: docstring
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by, mutate, lag
run_iteration <- function(single_simulation) {
  # sample treated units and get years and exposure based on policy speed
  treated_units <- get_treated_units(
    x=single_simulation$data,
    n=single_simulation$n_units,
    unit_var=single_simulation$unit_var,
    time_var=single_simulation$time_var,
    policy_speed=single_simulation$policy_speed,
    n_implementation_periods=single_simulation$n_implementation_periods,
    concurrent=single_simulation$concurrent,
    time_period_restriction=single_simulation$time_period_restriction
  )
  
  # apply exposure (treatment) to data
}

run_iteration <- function(ConfigObject) {
  
  treated_units <- get_treated_units(ConfigObject, ConfigObject$policy_speed)
  
  # apply exposure (treatment) information to data
  apply_exposure(treated_units, ConfigObject)
  
  # apply treatment effect
  te <- effect_magnitude(ConfigObject)
  apply_treatment_effect(ConfigObject, te)
  
  # add lagged outcome if needed
  if (ConfigObject$lag_outcome) {
    # include lag for outcome
    ConfigObject$data <- ConfigObject$data %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(lag_outcome = dplyr::lag(crude_adjusted_outcome, n=1, order_by=year))
  }
  
  # run the model
  m <- run_model(ConfigObject)
  
  # get iteration results
  results <- iter_results(m, ConfigObject)
  
  return(results)
}



