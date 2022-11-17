
# Optic Model (optic_model) class constructor:

# following the constructor, helper, validator convention:
# https://adv-r.hadley.nz/s3.html

# MaxTODO: Document this function

#' Optic Model
#'
#' @param name describe
#' @param type describe
#' @param call describe
#' @param formula describe
#' @param args describe
#' @param se_adjust describe
#'
#' @return optic_model (optic_model) object
#' @export
#'
optic_model <- function(name, type,call, formula, args, se_adjust) {
  
  # TODO: perform any needed type conversion here
  validate_optic_model(new_optic_model(name, type, call, formula, args, se_adjust))

}


# Constructor:
new_optic_model <- function(
                  name= c("fixedeff_linear", "autoreg_linear"),
                  type= c("reg", "autoreg"),
                  call= c("lm"),
                  formula,
                  args=list(weights=as.name('population')),
                  se_adjust=c("none", "cluster")
                  ){
  
  # model object
  m <- list()
  
  # basic checks and model assigments
  m$name <- match.arg(name)
  m$type <- match.arg(type)
  
  stopifnot(rlang::is_formula(formula))
  m$model_formula <- formula
  
  m$model_call <- call
  
  stopifnot(is.list(args))
  m$model_args <- args
  
  # TODO: What check to apply to se_adjust?
  m$se_adjust <- se_adjust
  
  class(m) <- "optic_model"
  
  return(m)

}

# Validator
validate_optic_model <- function(x) {
  
  # implement validation logic
  return(x)
  
}
