#' TODO: docstring
run_iteration <- function(single_simulation) {
  single_simulation <- single_simulation$method_sample(single_simulation)
  
  single_simulation <- single_simulation$method_te(single_simulation)
  
  if (!is.null(single_simulation$method_pre_model)) {
    single_simulation <- single_simulation$method_pre_model(single_simulation)
  }
  
  single_simulation <- single_simulation$method_model(single_simulation)
  
  if (!is.null(single_simulation$method_post_model)) {
    single_simulation <- single_simulation$method_post_model(single_simulation)
  }
  
  return(single_simulation$method_results(single_simulation))
}
