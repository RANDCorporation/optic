#' Calculate type S error - how often model gets direction of effect wrong
#' 
#' number of significant betas of certain direction divided by all significant betas
#' 
#' @param betas vector of regression coefficients
#' @param pvals vector of p values for betas
#' @param effect_direction direction of true effect
type_s_error <- function(betas, pvals, effect_direction) {
  if (length(betas[pvals < 0.05]) != 0) {
    if (effect.direction == "neg") {
      s_error <- length(betas[betas > 0 & pvals < 0.05]) / length(betas[pvals < 0.05])
    }else{
      s_error <- length(betas[betas < 0 & pvals < 0.05]) / length(betas[pvals < 0.05])
    }
  }else{
    s_error <- 0
  }
  return(s_error)
}
