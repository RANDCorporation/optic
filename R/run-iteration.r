#' TODO: docstring
#' 
#' @importFrom magrittr %>%
#' @import dplyr
run_iteration <- function(single_simulation) {
  # sample treated units and get years and exposure based on policy speed
  treated_units <- get_treated_units(
    x=single_simulation$data,
    n=single_simulation$n_units,
    unit_var=single_simulation$unit_var,
    time_var=single_simulation$time_var,
    policy_speed=single_simulation$policy_speed,
    n_implementation_periods=single_simulation$n_implementation_periods,
    concurrent=single_simulation$concurrent,
    time_period_restriction=single_simulation$time_period_restriction,
    rho=single_simulation$rhos
  )
  
  if (single_simulation$concurrent) {
    distances <- get_distance_avgs(treated_units)
  }
  
  # apply exposure (treatment) to data
  single_simulation$data <- apply_exposure(
    x=single_simulation$data,
    unit_var=single_simulation$unit_var,
    time_var=single_simulation$time_var,
    treated_units=treated_units,
    concurrent=single_simulation$concurrent
  )
  
  # apply treatment effect
  single_simulation$data <- apply_treatment_effect(
    x=single_simulation$data,
    model_formula=single_simulation$model_formula,
    model_call=single_simulation$model_call,
    te=single_simulation$effect_magnitude,
    effect_direction=single_simulation$effect_direction,
    concurrent=single_simulation$concurrent
  )
  
  # change code treatment if required
  if (single_simulation$change_code_treatment) {
    single_simulation$data <- change_code_treatment(
      x=single_simulation$data,
      unit_var=single_simulation$unit_var,
      time_var=single_simulation$time_var,
      concurrent=single_simulation$concurrent
    )
  }
  
  # add lagged variable to model if provided
  if (!is.null(single_simulation$add_lag)) {
    lag_var <- sym(single_simulation$add_lag)
    unit_sym <- dplyr::sym(single_simulation$unit_var)
    time_sym <- dplyr::sym(single_simulation$time_var)
    
    single_simulation$data <- single_simulation$data %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(lag_variable = dplyr::lag(!!lag_var, n=1)) %>%
      dplyr::ungroup()
    
    names(single_simulation$data)[which(names(single_simulation$data) == "lag_variable")] <- paste0("lag_", single_simulation$add_lag)
    
    new_formula <- as.formula(
      paste(all_terms$lhs, "~", paste(c(all_terms$rhs, paste0("lag_", single_simulation$add_lag)), collapse=" + "))
    )
    
    single_simulation$model_formula <- new_formula
  }
  
  # run model
  m <- run_model(single_simulation)
  
  # get iteration results
  if (single_simulation$concurrent) {
    # check for one or both treatments in model
    rhs <- model_terms(single_simulation$model_formula)[["rhs"]]
    if (sum(grepl("^treatment", rhs)) == 2) {
      results <- iter_results_concurrent_wjointeff(m, single_simulation)
    } else if (sum(grepl("^treatment", rhs)) == 1) {
      results <- iter_results(m, single_simulation)
    }
    results$mean_distance <- mean(distances)
    results$min_distance <- min(distances)
    results$max_distance <- max(distances)
  } else if (!single_simulation$concurrent) {
    results <- iter_results(m, single_simulation)
  }
  
  return(results)
}

