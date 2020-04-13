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
  
  # apply exposure (treatment) to data
  single_simulation$data <- apply_exposure(
    x=single_simulation$data,
    unit_var=single_simulation$unit_var,
    time_var=single_simulation$time_var,
    treated_units=treated_units,
    change_code_treatment=single_simulation$change_code_treatment,
    concurrent=single_simulation$concurrent
  )
  
  # apply treatment effect
  single_simulation$data <- apply_treatment_effect(
    x=single_simulation$data,
    model_call=single_simulation$model_call,
    model_formula=single_simulation$model_formula,
    te=single_simulation$effect_magnitude,
    effect_direction=single_simulation$effect_direction,
    concurrent=single_simulation$concurrent
  )
  
  # add lag outcome to model if required
  if (single_simulation$lag_outcome) {
    all_terms <- model_terms(single_simulation$model_formula)
    unit_sym <- dplyr::sym(single_simulation$unit_var)
    time_sym <- dplyr::sym(single_simulation$time_var)
    outcome_sym <- dplyr::sym(all_terms$lhs)
    
    single_simulation$data <- single_simulation$data %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(lag_outcome = dplyr::lag(!!outcome_sym, n=1)) %>%
      dplyr::ungroup()
    
    new_formula <- as.formula(
      paste(all_terms$lhs, "~", paste(c(all_terms$rhs, "lag_outcome"), collapse=" + "))
    )
    
    single_simulation$model_formula <- new_formula
  }
  
  # run model
  m <- run_model(single_simulation)
  
  # get iteration results
  class(single_simulation) <- c(class(single_simulation), single_simulation$model_call)
  if (single_simulation$concurrent) {
    results <- iter_results_concurrent_wjointeff(m, single_simulation)
  } else if (!single_simulation$concurrent) {
    results <- iter_results(m, single_simulation)
  }
  
  return(results)
}

