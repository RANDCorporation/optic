calculate_type1_error <- function(p_values) {
  return(mean(pval_flag(p_value)))
}
