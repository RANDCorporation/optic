#' Configuration object for OPTIC Simulation project
#' 
#' @importFrom R6 R6Class
#' 
#' @export
SimConfig <- R6::R6Class(
  "SimConfig",
  
  ###
  # PUBLIC
  ###
  public = list(
    initialize = function(
      data, outcome, unit_var, time_var, model_call, model_args, model_formula, iters,
      effect_direction, true_effect, n_units, policy_speed, se_adjust,
      concurrent, lag_outcome, n_implementation_periods, set_seed,
      time_period_restriction, correction_factors, rhos, change_code_treatment) {
      
      private$.data <- data
      private$.outcome <- outcome
      private$.unit_var <- unit_var
      private$.time_var <- time_var
      private$.model_call <- model_call
      private$.model_args <- model_args
      private$.model_formula <- model_formula
      private$.iters <- iters
      private$.effect_direction <- effect_direction
      private$.true_effect <- true_effect
      private$.n_units <- n_units
      private$.policy_speed <- policy_speed
      private$.n_implementation_periods <- n_implementation_periods
      private$.set_seed <- set_seed
      private$.time_period_restriction <- time_period_restriction
      private$.lag_outcome <- lag_outcome
      private$.se_adjust <- se_adjust
      private$.concurrent <- concurrent
      private$.correction_factors <- correction_factors
      private$.rhos <- rhos
      private$.change_code_treatment <- change_code_treatment
      
      # dispatch to correct S3 methods
      class(self) <- c(class(self), self$model_call)
    },
    
    print = function(...) {
      cat(paste("MODEL:", private$.model_call))
      cat(paste("\nOUTCOME:", private$.outcome))
      cat(paste("\nTRUE EFFECT:", private$.true_effect))
      cat(paste("\nEFFECT DIRECTION:", paste(priviate$.effect_direction, collapse=", ")))
      cat(paste("\nITERATIONS:", private$.iters))
    }
  ),
  
  ###
  # PRIVATE
  ###
  private = list(
    .data=NULL,
    .outcome=NULL,
    .unit_var=NULL,
    .time_var=NULL,
    .model_call=NULL,
    .model_formula=NULL,
    .model_args=NULL,
    .iters=NA,
    .effect_direction=NULL,
    .true_effect=NULL,
    .n_units=NULL,
    .policy_speed=NULL,
    .n_implementation_periods=NULL,
    .set_seed=NA,
    .time_period_restriction=NULL,
    .lag_outcome=NA,
    .se_adjust=NULL,
    .concurrent=NA,
    .correction_factors=NULL,
    .rhos=NULL,
    .change_code_treatment=NULL
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
    outcome = function(value) {
      if (missing(value)) {
        private$.outcome
      } else {
        stop("`$outcome` is read-only", call.=FALSE)
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
    true_effect = function(value) {
      if (missing(value)) {
        private$.true_effect
      } else {
        stop("`$true_effect` is read-only", call.=FALSE)
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
    set_seed = function(value) {
      if (missing(value)) {
        private$.set_seed
      } else {
        stop("`$set_seed` is read-only", call.=FALSE)
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
    }
  )
)
