#' apply exposure wrapper
apply_exposure <- function(x, unit_var, time_var, treated_units, change_code_treatment, concurrent) {
  if (concurrent) {
    apply_exposure_concurrent(x=x, unit_var=unit_var, time_var=time_var, treated_units=treated_units,
                              change_code_treatment=change_code_treatment)
  } else if (!concurrent) {
    apply_exposure_single(x=x, unit_var=unit_var, time_var=time_var, treated_units=treated_units,
                          change_code_treatment=change_code_treatment)
  }
}


#' apply exposure not concurrent
#' 
#' @importFrom magrittr %>%
#' @import dplyr
apply_exposure_single <- function(x, unit_var, time_var, treated_units, change_code_treatment) {
  unit_sym <- dplyr::sym(unit_var)
  time_sym <- dplyr::sym(time_var)
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
  
  if (change_code_treatment) {
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(temp_lag = dplyr::lag(treatment, n=1L)) %>%
      dplyr::mutate(treatment_change = treatment - temp_lag) %>%
      dplyr::ungroup() %>%
      dplyr::select(-temp_lag, treatment) %>%
      dplyr::rename(treatment=treatment_change)
  }
  
  return(x)
}

#' apply exposure for concurrent
#' 
#' @importFrom magrittr %>%
#' @import dplyr
apply_exposure_concurrent <- function(x, unit_var, time_var, treated_units, change_code_treatment) {
  unit_sym <- dplyr::sym(unit_var)
  time_sym <- dplyr::sym(time_var)
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
  
  if (change_code_treatment) {
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(temp_lag1 = dplyr::lag(treatment1, n=1L)) %>%
      dplyr::mutate(temp_lag2 = dplyr::lag(treatment2, n=1L)) %>%
      dplyr::mutate(treatment_change1 = treatment1 - temp_lag1) %>%
      dplyr::mutate(treatment_change2 = treatment2 - temp_lag2) %>%
      dplyr::ungroup() %>%
      dplyr::select(-temp_lag1, -temp_lag2, -treatment1, -treatment2) %>%
      dplyr::rename(treatment1=treatment_change1, treatment2=treatment_change2)
  }
  
  return(x)
}
