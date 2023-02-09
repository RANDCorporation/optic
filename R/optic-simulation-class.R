

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Configuration object for OPTIC Simulation project
#' 
#' @importFrom R6 R6Class
#' @importFrom tidyr crossing
#' @importFrom methods new
#' @importFrom stats as.formula pnorm sd vcov
#' 
#' @noRd
OpticSim <- R6::R6Class(
  "OpticSim",
  
  ###
  # PUBLIC
  ###
  public = list(
    
    initialize = function(
      data, models,
      method_sample, method_pre_model, method_model, method_post_model, method_results,
      iters, params, globals) {
      
      # create matrix of all combinations of iterable params
      simulation_params <- purrr::cross(params)
      simulation_params <- lapply(simulation_params, unlist)
      simulation_params <- data.frame(do.call("rbind", simulation_params), stringsAsFactors = FALSE)
      simulation_params <- type.convert(simulation_params, as.is=TRUE)
      
      private$.data <- data
      private$.models <- models
      private$.iters <- iters
      private$.params <- params
      private$.globals <- globals
      private$.simulation_params <- simulation_params
      
      private$.method_sample <- method_sample
      private$.method_pre_model <- method_pre_model
      private$.method_model <- method_model
      private$.method_post_model <- method_post_model
      private$.method_results <- method_results
    },
    
    setup_single_simulation = function(i) {
      params <- as.list(self$simulation_params[i, ])
      params$data <- self$data
      params$models <- self$models
      params$iters <- self$iters
      params$method_sample <- self$method_sample
      params$method_pre_model <- self$method_pre_model
      params$method_model <- self$method_model
      params$method_post_model <- self$method_post_model
      params$method_results <- self$method_results
      params$globals <- self$globals
      
      return(params)
    },
    
    print = function(...) {
      cat(paste("Number of Simulations:", nrow(self$simulation_params)))
      cat(paste("\nNumber of Models:", length(self$models)))
      cat(paste("\nIteration per Simulation :", self$iters))
      cat(paste("\nTotal number of Iterations to Run:", nrow(self$simulation_params)*length(self$models)*self$iters))
      cat("\n")
    }
  ),
  
  ###
  # PRIVATE
  ###
  private = list(
    .data=NULL,
    .models=NULL,
    .method_sample=NULL,
    .method_pre_model=NULL,
    .method_model=NA,
    .method_post_model=NULL,
    .method_results=NULL,
    .iters=NULL,
    .params=NULL,
    .globals=NULL,
    .simulation_params=NULL
  ),
  
  ###
  # ACTIVE
  ###
  active = list(
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        stop("`$data` is read-only", call.=FALSE)
      }
    },
    models = function(value) {
      if (missing(value)) {
        private$.models
      } else {
        stop("`$models` is read-only", call.=FALSE)
      }
    },
    iters = function(value) {
      if (missing(value)) {
        private$.iters
      } else {
        stop("`$iters` is read-only", call.=FALSE)
      }
    },
    params = function(value) {
      if (missing(value)) {
        private$.params
      } else {
        stop("`$params` is read-only", call.=FALSE)
      }
    },
    globals = function(value) {
      if (missing(value)) {
        private$.globals
      } else {
        stop("`$globals` is read-only", call.=FALSE)
      }
    },
    simulation_params = function(value) {
      if (missing(value)) {
        private$.simulation_params
      } else {
        stop("`$simulation_params` is read-only", call.=FALSE)
      }
    },
    method_sample = function(value) {
      if (missing(value)) {
        private$.method_sample
      } else {
        stop("`$method_sample` is read-only", call.=FALSE)
      }
    },
    method_pre_model = function(value) {
      if (missing(value)) {
        private$.method_pre_model
      } else {
        stop("`$method_pre_model` is read-only", call.=FALSE)
      }
    },
    method_model = function(value) {
      if (missing(value)) {
        private$.method_model
      } else {
        stop("`$method_model` is read-only", call.=FALSE)
      }
    },
    method_post_model = function(value) {
      if (missing(value)) {
        private$.method_post_model
      } else {
        stop("`$method_post_model` is read-only", call.=FALSE)
      }
    },
    method_results = function(value) {
      if (missing(value)) {
        private$.method_results
      } else {
        stop("`$method_results` is read-only", call.=FALSE)
      }
    }
  )
)
