

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
  get_behavior(model$type)$get_outcome(model)
}
