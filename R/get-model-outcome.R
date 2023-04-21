

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Get model outcome of a particular optic_model
#'
#' @param model optic_model object
#'
#' @return char(1) corresponding to the model outcome
#' @noRd
get_model_outcome = function(model){
  if(model$type != "drdid"){
    outcome <- model_terms(model[["model_formula"]])[["lhs"]]
  }else{
    outcome <- as.character(model$model_args$yname)
  }
  return(outcome)
}
