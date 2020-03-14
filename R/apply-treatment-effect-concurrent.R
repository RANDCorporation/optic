#' apply specified treatment effect and transform outcome to match model type
#' 
#' Calls appropriate method depending on model type (class) of OpticConfig object
#' 
#' @param ConfigObject R6Class object of class "OpticConfig"
#' @param te output of effect_magnitude, the true effect
#' 
#' @export
apply_treatment_effect_concurrent <- function(ConfigObject, te) {
  stopifnot("OpticConfig" %in% class(ConfigObject))
  UseMethod("apply_treatment_effect_concurrent", ConfigObject)
}


#' apply treatment effect for linear model
apply_treatment_effect_concurrent.linear <- function(ConfigObject, te) {
  if (ConfigObject$effect_direction != "null") {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]] + (te * ConfigObject$data$treatment1)+ (te * ConfigObject$data$treatment2)
    ConfigObject$data$crude_adjusted_outcome <- ConfigObject$data$outcome
  } else {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]]
    ConfigObject$data$crude_adjusted_outcome <- ConfigObject$data$outcome
  }
}


#' apply treatment effect for log model
apply_treatment_effect_concurrent.log <- function(ConfigObject, te) {
  if (ConfigObject$effect_direction != "null") {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]] +
                                   ConfigObject$data[[ConfigObject$outcome]] *
                                   (te - 1) * ConfigObject$data$treatment1*(te - 1) * ConfigObject$data$treatment2
    ConfigObject$data$outcome <- round2(ConfigObject$data$outcome, 0)
    ConfigObject$data$crude_adjusted_rate <- (ConfigObject$data$outcome * 100000) / ConfigObject$data$population
  }  else {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]]
    ConfigObject$data$crude_adjusted_rate <- (ConfigObject$data$outcome * 100000) / ConfigObject$data$population
  }
}


#' apply treatment effect for log-linear model
apply_treatment_effect_concurrent.loglinear <- function(ConfigObject, te) {
  if (ConfigObject$effect_direction != "null") {
    ConfigObject$data$outcome <- log(ConfigObject$data[[ConfigObject$outcome]] + 
                                       ConfigObject$data[[ConfigObject$outcome]] * 
                                       (te - 1) * ConfigObject$data$treatment1+(te - 1) * ConfigObject$data$treatment2)
    ConfigObject$data$crude_adjusted_rate <- exp(ConfigObject$data$outcome)
  }  else {
    ConfigObject$data$outcome <- log(ConfigObject$data[[ConfigObject$outcome]])
    ConfigObject$data$crude_adjusted_rate <- exp(ConfigObject$data$outcome)
  }
}
