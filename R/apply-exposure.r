#' apply exposure wrapper
apply_exposure <- function(x, unit_var, time_var, treated_units, concurrent) {
  if (concurrent) {
    apply_exposure_concurrent(x=x, unit_var=unit_var, time_var=time_var, treated_units=treated_units)
  } else if (!concurrent) {
    apply_exposure_single(x=x, unit_var=unit_var, time_var=time_var, treated_units=treated_units)
  }
}


#' apply exposure not concurrent
#' 
#' @importFrom magrittr %>%
#' @import dplyr
apply_exposure_single <- function(x, unit_var, time_var, treated_units) {
  x$treatment <- 0
  for (sunit in names(treated_units)) {
    for (i in 1:length(treated_units[[sunit]][["policy_years"]])) {
      time_period <- treated_units[[sunit]][["policy_years"]][i]
      exposure <- treated_units[[sunit]][["exposure"]][i]
      
      x$treatment <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment
      )
    }
  }
  
  return(x)
}

#' apply exposure for concurrent
#' 
#' @importFrom magrittr %>%
#' @import dplyr
apply_exposure_concurrent <- function(x, unit_var, time_var, treated_units) {
  x$treatment1 <- 0
  x$treatment2 <- 0
  
  for (sunit in names(treated_units)) {
    for (i in 1:length(treated_units[[sunit]][["policy_years1"]])) {
      time_period <- treated_units[[sunit]][["policy_years1"]][i]
      exposure <- treated_units[[sunit]][["exposure1"]][i]
      
      x$treatment1 <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment1
      )
    }
    for (i in 1:length(treated_units[[sunit]][["policy_years2"]])) {
      time_period <- treated_units[[sunit]][["policy_years2"]][i]
      exposure <- treated_units[[sunit]][["exposure2"]][i]
      
      x$treatment2 <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment2
      )
    }
  }
  
  return(x)
}
