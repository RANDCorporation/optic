require(R6)
require(tidyverse)


OpticConfig <- R6Class("OpticConfig", public=list(
  data=NULL,
  model_type=NULL,
  outcome=NULL,
  iters=NA,
  target_deaths=NA,
  effect_direction=NULL,
  n_states=NA,
  policy_speed=NULL,
  total_population=NA,
  total_deaths=NA,
  number_implementation_years=NA,
  seed=NA,
  results=NULL,
  
  initialize = function(data, model_type, outcome, iters, target_deaths, effect_direction, n_states,
                        policy_speed, number_implementation_years, set_seed=FALSE) {
    # input/data validation
    datafields <- c("population", "deaths", "year", "state")
    stopifnot(is.data.frame(data) | is_tibble(data))
    stopifnot(all(datafields %in% names(data)))
    stopifnot(is.character(model_type), length(model_type) == 1)
    stopifnot(is.character(outcome), length(outcome) == 1)
    stopifnot(is.numeric(iters))
    stopifnot(is.numeric(target_deaths))
    stopifnot(effect_direction %in% c("null", "pos", "neg"))
    stopifnot(is.numeric(n_states))
    stopifnot(policy_speed %in% c("instant", "slow"))
    stopifnot(is.numeric(number_implementation_years))
    
    self$data <- data
    self$model_type <- model_type
    self$outcome <- outcome
    self$iters <- iters
    self$target_deaths <- target_deaths
    self$effect_direction <- effect_direction
    self$n_states <- n_states
    self$policy_speed <- policy_speed
    self$number_implementation_years <- number_implementation_years
    self$total_population <- sum(data$population, na.rm=TRUE)
    self$total_deaths <- sum(data$deaths, na.rm=TRUE)
    
    if (set_seed) {
      self$seed <- sample(999:99999999, 1)
    }
    
    # dispatch to correct S3 methods
    class(self) <- c(class(self), self$model_type)
  },
  
  print = function(...) {
    # TODO: print method
    cat("TODO: print method.")
  })
)

# example
Sim1 <- OpticConfig$new(
  data=x,
  model_type="linear",
  iters=500,
  target_deaths=3500,
  effect_direction="pos",
  n_states=5,
  policy_speed="instant",
  number_implementation_years=3
)
