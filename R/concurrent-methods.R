#######################
### SAMPLING METHOD ###
#######################

#' perform sampling and coding of treatment for concurrent policy simulations
#' 
#' @description samples the treated units (e.g., states) randomly and then generates enacted dates for two policies with a prespeficied average mean difference in length between enactment dates of the two; also computes the needed levels and change levels coding for the time-varying vectors of the treatment/exposure variables representing those two policies 
#'
#' @param single_simulation object created from SimConfig$setup_single_simulation()
#' @export
#' 
concurrent_sample <- function(single_simulation) {
  
  ##########################################
  ### PULL DATA AND PARAMETERS/VARIABLES ###
  ##########################################
  x <- single_simulation$data
  n <- single_simulation$n_units
  unit_var <- single_simulation$unit_var
  time_var <- single_simulation$time_var
  policy_speed <- single_simulation$policy_speed
  n_implementation_periods <- single_simulation$n_implementation_periods
  rho <- single_simulation$rho
  time_period_restriction <- single_simulation$time_period_restriction
  years_apart <- single_simulation$years_apart
  ordered <- single_simulation$ordered
  
  ###############################################
  ### IDENTIFY TREATED UNITS AND TIME PERIODS ###
  ###############################################
  # sample treated units
  sampled_units <- sample(unique(x[[unit_var]]), n, replace=FALSE)
  
  # initialize list to store treated units
  treated <- list()
  # perform sampling and exposure coding
  for (sunit in sampled_units) {
    # randomly sample the time period, check if there is a restriction
    if (!is.null(time_period_restriction)) {
      available_periods <- x[[time_var]][x[[unit_var]] == sunit & x[[time_var]] %in% time_period_restriction]
    } else {
      available_periods <- unique(x[[time_var]])
    }
    
    #      sample on months; perhaps look into abstracting for any units of time
    available_periods1 = min(available_periods):(max(available_periods)-(years_apart))
    sampled_time_period <- sample(available_periods1, 1) #this becomes the mean
    
    # for second argument in mu, add the ability for the user how far those means should be on average.
    sampled_time_period_yearsapart = sampled_time_period + years_apart
    
    data = MASS::mvrnorm(n=200, mu=c(sampled_time_period, sampled_time_period_yearsapart), Sigma=matrix(c(1, rho, rho, 1), nrow=2), empirical=TRUE) #odd - can't set n = 1 so have to sample two
    # if order matters:
    if(ordered == "yes"){
      sampled_time_period1 = min(data[1,1], data[1,2])
      sampled_time_period2 = max(data[1,1], data[1,2])
    } else{ # if order doesn't matter:
      draw = sample(c(1,2),1)
      if(draw == 2){draw2 <- 1}else{draw2 <- 2}
      sampled_time_period1 = data[1, draw]  # standard normal (mu=yr, sd=1)
      sampled_time_period2 = data[1, draw2]  # standard normal (mu=yr, sd=1)
    }
    
    #group to nearest month 
    mo1<-sampled_time_period1-floor(sampled_time_period1)
    mo2<-sampled_time_period2-floor(sampled_time_period2)
    mo1=round(12*mo1)
    mo2=round(12*mo2)
    if(mo1==0) {
      mo1=1
    }
    if(mo2==0) {
      mo2=1
    }
    
    #setting years
    sampled_time_period1=floor(sampled_time_period1)
    sampled_time_period2=floor(sampled_time_period2)
    
    # exposure coding
    l1 <- optic::exposure_list(sampled_time_period1, mo1, available_periods, policy_speed, n_implementation_periods)
    names(l1) <- paste0(names(l1), "1")
    l2 <- optic::exposure_list(sampled_time_period2, mo2, available_periods, policy_speed, n_implementation_periods)
    names(l2) <- paste0(names(l2), "2")
    treated[[sunit]] <- c(l1, l2)
  }
  
  # calulate the distance (in days) between the two policy start dates
  policy_distances <- get_distance_avgs(treated)
  
  #############################
  ### ADD TREATMENT TO DATA ###
  #############################
  # apply treatment to data
  x$treatment1 <- 0
  x$treatment2 <- 0
  
  for (sunit in names(treated)) {
    for (i in 1:length(treated[[sunit]][["policy_years1"]])) {
      time_period <- treated[[sunit]][["policy_years1"]][i]
      exposure <- treated[[sunit]][["exposure1"]][i]
      
      x$treatment1 <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment1
      )
    }
    for (i in 1:length(treated[[sunit]][["policy_years2"]])) {
      time_period <- treated[[sunit]][["policy_years2"]][i]
      exposure <- treated[[sunit]][["exposure2"]][i]
      
      x$treatment2 <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment2
      )
    }
  }
  
  #########################################
  ### LEVEL AND CHANGE CODING TREATMENT ###
  #########################################
  # ensure both treatment levels and treatment change code are coded
  unit_sym <- dplyr::sym(unit_var)
  time_sym <- dplyr::sym(time_var)
  
  x <- x %>%
    dplyr::arrange(!!unit_sym, !!time_sym) %>%
    dplyr::group_by(!!unit_sym) %>%
    dplyr::mutate(temp_lag1 = dplyr::lag(treatment1, n=1L)) %>%
    dplyr::mutate(temp_lag2 = dplyr::lag(treatment2, n=1L)) %>%
    dplyr::mutate(treatment1_change = treatment1 - temp_lag1) %>%
    dplyr::mutate(treatment2_change = treatment2 - temp_lag2) %>%
    dplyr::ungroup() %>%
    dplyr::select(-temp_lag1, -temp_lag2) %>%
    # need to save original treatment1 and treatment2 since they are used
    # for treatment effect application
    dplyr::mutate(treatment1_level=treatment1, treatment2_level=treatment2)
  
  
  # assign back to config object
  single_simulation$data <- x
  single_simulation$policy_distances <- policy_distances
  
  return(single_simulation)
}


########################
### PRE MODEL METHOD ###
########################

#' pre-modeling method to apply to config object/data
#' 
#' @description since there are different data transformations needed for different
#'     model approaches (e.g., linear, count, log-linear), the treatment effect application and other data prep steps
#'     are run here before modeling in the model-specific config object rather 
#'     than in the sampling step 
#' 
#' @export
concurrent_premodel <- function(model_simulation) {
  ##########################################
  ### PULL DATA AND PARAMETERS/VARIABLES ###
  ##########################################
  x <- model_simulation$data
  model <- model_simulation$models
  outcome <- model_terms(model$model_formula)[["lhs"]]
  
  ##############################
  ### APPLY TREATMENT EFFECT ###
  ##############################
  # custom method that works for concurrent and regular single policy eval
  x <- apply_treatment_effect(
    x=x,
    model_formula=model$model_formula,
    model_call=model$model_call,
    te=c(model_simulation$effect_magnitude1, model_simulation$effect_magnitude2),
    effect_direction=model_simulation$effect_direction,
    concurrent=TRUE
  )
  
  # if autoregressive, need to add lag for crude rate
  # when outcome is deaths, derive new crude rate from modified outcome
  if (model$type == "autoreg") {
    if (outcome == "deaths") {
      x$crude.rate <- (x$deaths * 100000)/ x$population
    }
    
    # get lag of crude rate and add it to the model
    unit_sym <- dplyr::sym(model_simulation$unit_var)
    time_sym <- dplyr::sym(model_simulation$time_var)
    
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(lag_crude.rate = dplyr::lag(crude.rate, n=1L)) %>%
      dplyr::ungroup()
    
    model_simulation$models$model_formula <- update.formula(model_simulation$models$model_formula, ~ . + lag_crude.rate)
  }
  
  # modified data back into object
  model_simulation$data <- x
  
  return(model_simulation)
}


####################
### MODEL METHOD ###
####################
#' run model and store results
#' 
#' @description runs the model against the prepared data along with
#'     any provided arguments. Stores the model object in the input
#'     list, new element named "model_result" and returns full list
#'
#' @export
concurrent_model <- function(model_simulation) {
  model <- model_simulation$models
  x <- model_simulation$data
  
  args = c(list(data=x, formula=model[["model_formula"]]), model[["model_args"]])
  
  m <- do.call(
    model[["model_call"]],
    args
  )
  
  model_simulation$model_result <- m
  
  return(model_simulation)
}


#########################
### POST_MODEL METHOD ###
#########################

#' process results from model(s) 
#' 
#' @description summarizes the statistical performance of the model(s) being compared by computing summary information on the model fit, estimated effects and standard errors 
#' 
#' @export
concurrent_postmodel <- function(model_simulation) {
  model <- model_simulation$models
  
  # check for one or both treatments in model
  rhs <- model_terms(model$model_formula)[["rhs"]]
  if (sum(grepl("^treatment", rhs)) == 2) {
    results <- iter_results_concurrent_wjointeff(model_simulation)
  } else if (sum(grepl("^treatment", rhs)) == 1) {
    results <- iter_results(model_simulation)
    results <- results %>%
      dplyr::rename(
        estimate1=estimate,
        se1=se,
        variance1=variance,
        test_stat1=t_stat,
        p_value1=p_value
      ) %>%
      dplyr::select(-mse)
  }
  
  results$effect_direction <- model_simulation$effect_direction
  results$effect_magnitude1 <- model_simulation$effect_magnitude1
  results$effect_magnitude2 <- model_simulation$effect_magnitude2
  results$policy_speed <- model_simulation$policy_speed
  
  # Max edit: Argument below was originally called "model_simulat$rhos". I changed to call it
  # "rho" to ensure this matches the output from results. This allows merging
  # on a sim_id column in a new results step.
  
  results$rho <- model_simulation$rho
  results$years_apart <- model_simulation$years_apart
  results$ordered <- model_simulation$ordered
  results$n_units <- model_simulation$n_units
  results$mean_distance <- mean(model_simulation$policy_distances)
  results$min_distance <- min(model_simulation$policy_distances)
  results$max_distance <- max(model_simulation$policy_distances)
  results$n_implementation_periods <- model_simulation$n_implementation_periods
  results$model_name <- model_simulation$models$name
  results$model_type <- model_simulation$models$type
  results$model_call <- model_simulation$models$model_call
  results$model_formula <- paste(trimws(deparse(model_simulation$models$model_formula)), collapse=" ")
  
  return(results)
}


######################
### RESULTS METHOD ###
######################

#' compiles the final results 
#' 
#' @description compiles the results into one table for all permutations of the simulation
#' 
#' @export
concurrent_results <- function(r) {
  return(dplyr::bind_rows(r))
}



