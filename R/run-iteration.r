

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' TODO: docstring
#' 
#' @noRd
run_iteration <- function(single_simulation) {
  # sampling occurs outside modeling loop so same sample is used for each model
  single_simulation <- single_simulation$method_sample(single_simulation)
  
  # After sampling, loop over models, single_simulation is initial input, but 
  # then model specific object is used, output from method_results are added to
  # list by model name and results object is returned
  results <- list()
  for (mname in names(single_simulation$models)) {
    # create new object for this model and limit models element to current model
    model_simulation <- single_simulation
    model_simulation$models <- single_simulation$models[[mname]]
    
    if (!is.null(single_simulation$method_pre_model)) {
      model_simulation <- single_simulation$method_pre_model(model_simulation)
    }
    
    if (!is.null(single_simulation$method_post_model)) {

      model_simulation <- single_simulation$method_model(model_simulation)
      
      r <- single_simulation$method_post_model(model_simulation)
    } else {
      r <- single_simulation$method_model(model_simulation)
    }
    
    results[[mname]] <- r
    rm(model_simulation, r)
  }
  
  return(single_simulation$method_results(results))
}
