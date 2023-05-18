

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Parse a formula object into its left-hand-side and right-hand-side components
#' 
#' @param x Formula to parse
#' 
#' @return list with named elements "lhs" and "rhs", containing variables on each respective side of the equation
#' @examples 
#' # Set up a hypothetical function, then decompose into left-hand and 
#' # right-hand sides
#' form <- formula(outcome ~ treatment + confounder + unit + time)
#' model_terms(form)
#' @export
model_terms <- function(x) {
  
  stopifnot(class(x) == "formula")
  
  lhs <- all.vars(x)[1]
  rhs <- all.vars(x)[-1]
  
  return(list(lhs=lhs, rhs=rhs))
  
}