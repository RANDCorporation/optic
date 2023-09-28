

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' apply specified treatment effect (percent change) to outcome
#' 
#' 
#' @param x data
#' @param model_formula formula for sim run used to identify outcome
#' @param te true effect as proportion of change (e.g., 0.05 = 5%)
#' @param effect_direction "null", "pos", or "neg"
#' @param concurrent bool for whether this is concurrent run or not
#' 
#' @noRd
apply_treatment_effect <- function(x, model_formula, outcome, model_call, te, effect_direction, concurrent) {
  
  # identify outcome
  outcome <- model_terms(model_formula)[["lhs"]]
  
  # identify additive or multiplicative modification of outcome required
  if (model_call != "glm.nb") {
    modifier <- "additive"
  } else if (model_call == "glm.nb") {
    modifier <- "multiplicative"
  } 
  
  # apply true effect
  if (effect_direction == "null") {
    return(x)
  } else {
    if (effect_direction == "neg") {
      te <- -1 * te
    }
    if (modifier == "additive") {
      if (concurrent) {
        x[[outcome]] <- x[[outcome]] + (te[1] * x[["treatment1"]]) + (te[2] * x[["treatment2"]])
      } else {
        x[[outcome]] <- x[[outcome]] + (te * x[["treatment"]])
      }
    } else if (modifier == "multiplicative") {
      if (concurrent) {
        x[[outcome]] <- x[[outcome]] + (x[[outcome]] * te[1] * x[["treatment1"]]) + (x[[outcome]] * te[2] * x[["treatment2"]])
      } else {
        x[[outcome]] <- x[[outcome]] + (x[[outcome]] * te * x[["treatment"]])
      }
      
      #l
      # x[[outcome]] <- round2(x[[outcome]], 0)
    } 
    
    return(x)
  }
}
