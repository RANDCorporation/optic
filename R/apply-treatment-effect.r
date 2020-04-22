#' apply specified treatment effect (percent change) to outcome
#' 
#' @description TODO
#' 
#' @param x data
#' @param model_formula formula for sim run used to identify outcome
#' @param te true effect as proportion of change (e.g., 0.05 = 5%)
#' @param effect_direction "null", "pos", or "neg"
#' @param concurrent bool for whether this is concurrent run or not
apply_treatment_effect <- function(x, model_formula, te, effect_direction, concurrent) {
  outcome <- model_terms(model_formula)[["lhs"]]
  
  if (effect_direction == "null") {
    return(x)
  } else {
    if (effect_direction == "neg") {
      te <- -1 * te
    }
    if (concurrent) {
      x[[outcome]] <- x[[outcome]] + (te[1] * x[["treatment1"]]) + (te[2] * x[["treatment2"]])
    } else {
      x[[outcome]] <- x[[outcome]] + (te * x[["treatment"]])
    }
    return(x)
  }
}