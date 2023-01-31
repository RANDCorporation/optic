#' @importFrom magrittr %>%
#' @import dplyr
change_code_treatment <- function(x, unit_var, time_var, concurrent) {
  unit_sym <- dplyr::sym(unit_var)
  time_sym <- dplyr::sym(time_var)
  
  if (concurrent) {
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
    
  } else {
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(temp_lag = dplyr::lag(treatment, n=1L)) %>%
      dplyr::mutate(treatment_change = treatment - temp_lag) %>%
      dplyr::ungroup() %>%
      dplyr::select(-temp_lag, -treatment) %>%
      dplyr::rename(treatment=treatment_change)
  }
  
  return(x)
}
