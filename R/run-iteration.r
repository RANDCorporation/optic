#' TODO: docstring
#' 
#' @importFrom magrittr %>%
#' 
#' @export
run_iteration <- function(ConfigObject) {
  # sample treated units and get years and exposure based on policy speed
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



