#' apply specified treatment effect (percent change) to outcome
#' 
#' Calls appropriate method depending on model type (class) of OpticConfig object
#' 
#' @param x data
#' @param model_formula formula for sim run used to identify outcome
#' @param effect_magnitude effect as proportion of change (e.g., 0.05 = 5%)
#' @param effect_direction "null", "pos", or "neg"
#' @param concurrent bool for whether this is concurrent run or not
apply_treatment_effect <- function(x, model_formula, effect_magnitude, effect_direction, concurrent) {
  outcome <- model_terms(model_formula)[["lhs"]]
  
  if (effect_direction == "null") {
    return(x)
  } else {
    if (effect_direction == "neg") {
      effect_magnitude <- -1 * effect_magnitude
    }
    if (concurrent) {
      x[[outcome]] <- x[[outcome]] + (x[[outcome]] * effect_magnitude[1] * x[["treatment1"]]) + (x[[outcome]] * effect_magnitude[2] * x[["treatment2"]])
    } else {
      x[[outcome]] <- x[[outcome]] + (x[[outcome]] * effect_magnitude * x[["treatment"]])
    }
    return(x)
  }
}