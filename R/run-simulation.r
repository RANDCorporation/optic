#' TODO: docstring
#' 
run_simulation <- function(ConfigObject) {
  # THIS IS ONE ITERATION OF SIM
  
  # sample treated units and get years and exposure based on policy speed
  treated_units <- get_treated_units(ConfigObject, ConfigObject$policy_speed)
  
  # apply exposure (treatment) information to data
  apply_exposure(treated_units, ConfigObject)
  
  # apply treatment effect
  te <- effect_magnitude(ConfigObject)
  apply_treatment_effect(ConfigObject, te)
  
}



