#' run model
#' @export
run_model <- function(single_simulation) {
  # required for negative binary and GEE models
  if (single_simulation$model_call == "glm.nb") {
    require(MASS)
  }
  if (single_simulation$model_call == "geeglm") {
    require(geepack)
  }
  
  m <- do.call(
    single_simulation$model_call,
    c(list(data=single_simulation$data, formula=single_simulation$model_formula),
      single_simulation$model_args))
  
  return(m)
}