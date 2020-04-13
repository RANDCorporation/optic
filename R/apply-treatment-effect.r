#' apply specified treatment effect and transform outcome to match model type
#' 
#' Calls appropriate method depending on model type (class) of OpticConfig object
#' 
#' @param ConfigObject R6Class object of class "OpticConfig"
#' @param te output of effect_magnitude, the true effect
apply_treatment_effect <- function(x, model_call, model_formula, te, effect_direction, concurrent) {
  outcome <- model_terms(model_formula)[["lhs"]]
  
  if (effect_direction == "null") {
    return(x)
  } else {
    if (effect_direction == "neg") {
      te <- -1 * te
    }
    if (model_call == "lm") {
      if (concurrent) {
        x[[outcome]] <- x[[outcome]] + (te * x[["treatment1"]]) + (te * x[["treatment2"]])
      } else if (!concurrent) {
        x[[outcome]] <- x[[outcome]] + (te * x[["treatment"]])
      }
    } else if (model_call %in% c("glm.nb")) {
      if (concurrent) {
        x[[outcome]] <- x[[outcome]] + (x[[outcome]] * te * x[["treatment1"]]) + (x[[outcome]] * te * x[["treatment2"]])
      } else if (!concurrent) {
        x[[outcome]] <- x[[outcome]] + (x[[outcome]] * te * x[["treatment"]])
      }
    }
  
    return(x)
  }
}