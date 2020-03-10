#' TODO: docstring
#' 
#' @importFrom magrittr %>%
#' 
#' @export
run_iteration_concurrent <- function(ConfigObject,rho) {
  library(MASS) 
  # sample treated units and get years and exposure based on policy speed
  treated_units <- get_treated_units_concurrent(ConfigObject, ConfigObject$policy_speed,rho)
  
  # apply exposure (treatment) information to data
  apply_exposure_concurrent(treated_units, ConfigObject)
  
  # apply treatment effect 
  #for now let's assume both laws have same effect
  te <- effect_magnitude(ConfigObject)
  
  #we need to expand treatment effect to allow for two te's from the two laws
  apply_treatment_effect_concurrent(ConfigObject, te)
  
  # add lagged outcome if needed
  if (ConfigObject$lag_outcome) {
    # include lag for outcome
    ConfigObject$data <- ConfigObject$data %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(lag_outcome = dplyr::lag(crude_adjusted_outcome, n=1, order_by=year))
   # dplyr::mutate(lag_outcome = dplyr::lag(crude_adjusted_rate, n=1, order_by=year))
  }

  
  # run the model
  #we will also need to update the model so it includes predictors for both policies
  #here I updated in code since not dealth with inside function so we could use the same
  #run_model (at least for now)
  m <- run_model(ConfigObject)
  
  # get iteration results - need to expand results so includes 
  #regression details for both sets of laws
  results <- iter_results_concurrent_wjointeff(m, ConfigObject)
  
  return(results)
}




