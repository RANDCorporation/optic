apply_treatment_effect <- function(ConfigObject, te) {
  stopifnot("OpticConfig" %in% class(ConfigObject))
  UseMethod("apply_treatment_effect", ConfigObject)
}

apply_treatment_effect.linear <- function(ConfigObject, te) {
  if (ConfigObject$effect_direction != "null") {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]] + (te * ConfigObject$data$treatment)
  } else {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]]
  }
}

apply_treatment_effect.log <- function(ConfigObject, te) {
  if (ConfigObject$effect_direction != "null") {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]] +
                                   ConfigObject$data[[ConfigObject$outcome]] *
                                   (te - 1) * ConfigObject$data$treatment
    #TODO: rounded???
    ConfigObject$data$outcome <- round(ConfigObject$data$outcome)
    ConfigObject$data$outcome <- (ConfigObject$data$outcome * 100000) / ConfigObject$data$population
  }  else {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]]
  }
}

apply_treatment_effect.loglinear <- function(ConfigObject, te) {
  if (ConfigObject$effect_direction != "null") {
    #TODO: confirm outcome is exp() piece
    ConfigObject$data$outcome <- log(ConfigObject$data[[ConfigObject$outcome]] + 
                                       ConfigObject$data[[ConfigObject$outcome]] * 
                                       (te - 1) * ConfigObject$data$treatment)
    ConfigObject$data$outcome <- exp(ConfigObject$data$outcome)
  }  else {
    ConfigObject$data$outcome <- ConfigObject$data[[ConfigObject$outcome]]
  }
}