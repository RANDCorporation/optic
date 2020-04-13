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
      data, unit_var, time_var, model_call, model_args, model_formula, iters,
      effect_direction, effect_magnitude, n_units, policy_speed, se_adjust,
      concurrent, lag_outcome, n_implementation_periods,
      time_period_restriction, correction_factors, rhos, change_code_treatment) {
      
      # create matrix of all combinations of iterable args
      model_index <- 1:length(model_call)
      combination_args <- tidyr::crossing(
        model_index,
        n_units,
        effect_direction,
        policy_speed
      )
      if(concurrent) {
        combination_args <- tidyr::crossing(
          combination_args,
          rhos
        )
      }
      
      private$.data <- data
      private$.unit_var <- unit_var
      private$.time_var <- time_var
      private$.model_call <- model_call
      private$.model_args <- model_args
      private$.model_formula <- model_formula
      private$.iters <- iters
      private$.effect_direction <- effect_direction
      private$.effect_magnitude <- effect_magnitude
      private$.n_units <- n_units
      private$.policy_speed <- policy_speed
      private$.n_implementation_periods <- n_implementation_periods
      private$.time_period_restriction <- time_period_restriction
      private$.lag_outcome <- lag_outcome
      private$.se_adjust <- se_adjust
      private$.concurrent <- concurrent
      private$.correction_factors <- correction_factors
      private$.rhos <- rhos
      private$.change_code_treatment <- change_code_treatment
      private$.combination_args <- combination_args
      
      # dispatch to correct S3 methods
      class(self) <- c(class(self), self$model_call)
    },
    
    setup_single_simulation = function(combination) {
      params <- as.list(self$combination_args[combination, ])
      params$model_call <- self$model_call[[params$model_index]]
      params$model_formula <- self$model_formula[[params$model_index]]
      params$effect_magnitude <- self$effect_magnitude[[params$model_index]]
      if (!is.null(self$model_args)) {
        params$model_args <- self$model_args[[params$model_index]]
      } else {
        params$model_args <- NULL
      }
      params$data <- self$data
      params$unit_var <- self$unit_var
      params$time_var <- self$time_var
      params$iters <- self$iters
      params$n_implementation_periods <- self$n_implementation_periods
      params$time_period_restriction <- self$time_period_restriction
      params$lag_outcome <- self$lag_outcome
      params$se_adjust <- self$se_adjust
      params$concurrent <- self$concurrent
      params$correction_factors <- self$correction_factors
      params$change_code_treatment <- self$change_code_treatment
      params$combination_args <- self$combination_args
      
      return(params)
    },
    
    print = function(...) {
      cat(paste("Number of Simulations:", nrow(self$combination_args)))
      cat(paste("\nIteration per Simulation :", self$iters))
      cat(paste("\nTotal number of Iterations to Run:", nrow(self$conbination_args)*self$iters))
    }
  ),
  
  ###
  # PRIVATE
  ###
  private = list(
    .data=NULL,
    .unit_var=NULL,
    .time_var=NULL,
    .model_call=NULL,
    .model_formula=NULL,
    .model_args=NULL,
    .iters=NA,
    .effect_direction=NULL,
    .effect_magnitude=NULL,
    .n_units=NULL,
    .policy_speed=NULL,
    .n_implementation_periods=NULL,
    .time_period_restriction=NULL,
    .lag_outcome=NA,
    .se_adjust=NULL,
    .concurrent=NA,
    .correction_factors=NULL,
    .rhos=NULL,
    .change_code_treatment=NULL,
    .combination_args=NULL
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
    unit_var = function(value) {
      if (missing(value)) {
        private$.unit_var
      } else {
        stop("`$unit_var` is read-only", call.=FALSE)
      }
    },
    time_var = function(value) {
      if (missing(value)) {
        private$.time_var
      } else {
        stop("`$time_var` is read-only", call.=FALSE)
      }
    },
    model_call = function(value) {
      if (missing(value)) {
        private$.model_call
      } else {
        stop("`$model_call` is read-only", call.=FALSE)
      }
    },
    model_formula = function(value) {
      if (missing(value)) {
        private$.model_formula
      } else {
        stop("`$model_formula` is read-only", call.=FALSE)
      }
    },
    model_args = function(value) {
      if (missing(value)) {
        private$.model_args
      } else {
        stop("`$model_args` is read-only", call.=FALSE)
      }
    },
    iters = function(value) {
      if (missing(value)) {
        private$.iters
      } else {
        stop("`$iters` is read-only", call.=FALSE)
      }
    },
    effect_direction = function(value) {
      if (missing(value)) {
        private$.effect_direction
      } else {
        stop("`$effect_direction` is read-only", call.=FALSE)
      }
    },
    effect_magnitude = function(value) {
      if (missing(value)) {
        private$.effect_magnitude
      } else {
        stop("`$effect_magnitude` is read-only", call.=FALSE)
      }
    },
    n_units = function(value) {
      if (missing(value)) {
        private$.n_units
      } else {
        stop("`$n_units` is read-only", call.=FALSE)
      }
    },
    policy_speed = function(value) {
      if (missing(value)) {
        private$.policy_speed
      } else {
        stop("`$policy_speed` is read-only", call.=FALSE)
      }
    },
    n_implementation_periods = function(value) {
      if (missing(value)) {
        private$.n_implementation_periods
      } else {
        stop("`$n_implementation_periods` is read-only", call.=FALSE)
      }
    },
    time_period_restriction = function(value) {
      if (missing(value)) {
        private$.time_period_restriction
      } else {
        stop("`$time_period_restriction` is read-only", call.=FALSE)
      }
    },
    lag_outcome = function(value) {
      if (missing(value)) {
        private$.lag_outcome
      } else {
        stop("`$lag_outcome` is read-only", call.=FALSE)
      }
    },
    se_adjust = function(value) {
      if (missing(value)) {
        private$.se_adjust
      } else {
        stop("`$se_adjust` is read-only", call.=FALSE)
      }
    },
    concurrent = function(value) {
      if (missing(value)) {
        private$.concurrent
      } else {
        stop("`$iters` is read-only", call.=FALSE)
      }
    },
    correction_factors = function(value) {
      if (missing(value)) {
        private$.correction_factors
      } else {
        stop("`$iters` is read-only", call.=FALSE)
      }
    },
    rhos = function(value) {
      if (missing(value)) {
        private$.rhos
      } else {
        stop("`$rhos` is read-only", call.=FALSE)
      }
    },
    change_code_treatment = function(value) {
      if (missing(value)) {
        private$.change_code_treatment
      } else {
        stop("`$change_code_treatment` is read-only", call.=FALSE)
      }
    },
    combination_args = function(value) {
      if (missing(value)) {
        private$.combination_args
      } else {
        stop("`$combination_args` is read-only", call.=FALSE)
      }
    }
  )
)
