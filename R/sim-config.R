#' Configuration object for OPTIC Simulation project
#' 
#' @importFrom R6 R6Class
#' @importFrom tidyr crossing
SimConfig <- R6::R6Class(
  "SimConfig",
  
  ###
  # PUBLIC
  ###
  public = list(
    initialize = function(
      data, models,
      method_class, method_sample, method_te, method_pre_model,
      method_model, method_post_model, method_results,
      iters, params) {
      
      # create matrix of all combinations of iterable params
      simulation_params <- purrr::cross(params)
      simulation_params <- lapply(simulation_params, unlist)
      simulation_params <- data.frame(do.call("rbind", simulation_params), stringsAsFactors = FALSE)
      simulation_params <- type.convert(simulation_params, as.is=TRUE)
      
      private$.data <- data
      private$.models <- models
      private$.iters <- iters
      private$.params <- params
      private$.simulation_params <- simulation_params
      
      private$.method_class <- method_class
      private$.method_sample <- method_sample
      private$.method_te <- method_te
      private$.method_pre_model <- method_pre_model
      private$.method_model <- method_model
      private$.method_post_model <- method_post_model
      private$.method_results <- method_results
      
      # dispatch to correct S3 methods
      class(self) <- c(class(self), self$method_class)
    },
    
    setup_single_simulation = function(i) {
      params <- as.list(self$simulation_params[i, ])
      params$data <- self$data
      params$models <- self$models
      params$iters <- self$iters
      params$method_class <- self$method_class
      params$method_sample <- self$method_sample
      params$method_te <- self$method_te
      params$method_pre_model <- self$method_pre_model
      params$method_model <- self$method_model
      params$method_post_model <- self$method_post_model
      params$method_results <- self$method_results
      
      class(params) <- c(class(params), self$method_class)
      
      return(params)
    },
    
    print = function(...) {
      cat(paste("Number of Simulations:", nrow(self$simulation_params)))
      cat(paste("\nIteration per Simulation :", self$iters))
      cat(paste("\nTotal number of Iterations to Run:", nrow(self$simulation_params)*self$iters))
      cat("\n")
    }
  ),
  
  ###
  # PRIVATE
  ###
  private = list(
    .data=NULL,
    .models=NULL,
    .method_class=NULL,
    .method_sample=NULL,
    .method_te=NULL,
    .method_pre_model=NULL,
    .method_model=NA,
    .method_post_model=NULL,
    .method_results=NULL,
    .iters=NULL,
    .params=NULL,
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
    simulation_params = function(value) {
      if (missing(value)) {
        private$.simulation_params
      } else {
        stop("`$simulation_params` is read-only", call.=FALSE)
      }
    },
    method_class = function(value) {
      if (missing(value)) {
        private$.method_class
      } else {
        stop("`$method_class` is read-only", call.=FALSE)
      }
    },
    method_sample = function(value) {
      if (missing(value)) {
        private$.method_sample
      } else {
        stop("`$method_sample` is read-only", call.=FALSE)
      }
    },
    method_te = function(value) {
      if (missing(value)) {
        private$.method_te
      } else {
        stop("`$method_te` is read-only", call.=FALSE)
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
