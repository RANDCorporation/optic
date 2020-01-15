#' run model
#' @export
#' 
run_model <- function(ConfigObject) {
  # required for negative binary and GEE models
  if (ConfigObject$method_call == "glm.nb") {
    require(MASS)
  }
  if (ConfigObject$method_call == "geeglm") {
    require(geepack)
  }
  
  m <- do.call(ConfigObject$method_call, c(ConfigObject$model_params, list(data=ConfigObject$data)))
  
  return(m)
}