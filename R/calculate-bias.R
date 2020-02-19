#' @export
calculate_bias <- function(estimate, te) {
  return(mean(estimate - te))
}
