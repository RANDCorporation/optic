#' TODO: docstring
#' @param single_simulation SimConfig object created using 'create_config'

#' @noRd
run_tuning <- function(single_simulation) {
  # sampling occurs outside modeling loop so same sample is used for each model
  single_simulation <- single_simulation$method_sample(single_simulation)
  
  # After sampling, loop over models, single_simulation is initial input, but 
  # then model specific object is used, output from method_results are added to
  # list by model name and results object is returned
  results <- data.frame()
  mname = names(single_simulation$models)
  
  # create new object for this model and limit models element to current model
  model_simulation <- single_simulation
  model_simulation$models <- single_simulation$models[[mname]]
  
  if (!is.null(single_simulation$method_pre_model)) {
    model_simulation <- single_simulation$method_pre_model(model_simulation)
  }
  outcome <- model_terms(model_simulation$models[["model_formula"]])[["lhs"]]
  bias_vals <- model_simulation$globals[["bias_vals"]][[model_simulation$bias_type]][[model_simulation$prior_control]][[model_simulation$bias_size]]
  # get run metadata to merge in after
  meta_data <- data.frame(
    model_name = model_simulation$models$name,
    model_call = model_simulation$models[["model_call"]],
    outcome = outcome,
    model_formula = Reduce(paste, trimws(deparse(model_simulation$models[["model_formula"]]))),
    policy_speed = model_simulation$policy_speed,
    n_implementation_years = model_simulation$n_implementation_periods,
    prior_control = model_simulation$prior_control,
    bias_type = model_simulation$bias_type,
    bias_size = model_simulation$bias_size,
    b0 = bias_vals["b0"],
    b1 = bias_vals["b1"],
    b2 = bias_vals["b2"],
    b3 = bias_vals["b3"],
    b4 = bias_vals["b4"],
    b5 = bias_vals["b5"],
    a1 = bias_vals["a1"],
    a2 = bias_vals["a2"],
    a3 = bias_vals["a3"],
    a4 = bias_vals["a4"],
    a5 = bias_vals["a5"]
  )
  
  combine_results = cbind(meta_data, model_simulation$balance_statistics)
  return(combine_results)
}

# # Tune Parameters:
# for(j in 1:300){
#   for(i in 1:6){
#     example_single_sim <- linear_fe_config$setup_single_simulation(i)
#     if(i==1){
#       tuning_results = tuning(example_single_sim)
#     } else{
#       tuning_results = rbind(tuning_results, tuning(example_single_sim))
#     }
#   }
# }




  
  
  