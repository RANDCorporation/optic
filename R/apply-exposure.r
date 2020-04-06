#' apply exposure
#' 
#' @importFrom magrittr %>%
#' 
#' @export
apply_exposure <- function(treated_units, ConfigObject) {
  ConfigObject$data$treatment <- 0
  for (t_state in names(treated_units)) {
    for (i in 1:length(treated_units[[t_state]][["policy_years"]])) {
      yr <- treated_units[[t_state]][["policy_years"]][i]
      exposure <- treated_units[[t_state]][["exposure"]][i]
      
      ConfigObject$data <- ConfigObject$data %>%
        dplyr::mutate(treatment = ifelse(state == t_state & year == yr, exposure, treatment))
    }
  }
  
  # add change level coding
  ConfigObject$data <- ConfigObject$data %>%
    dplyr::arrange(state, year) %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(temp_lag = lag(treatment, n=1L)) %>%
    dplyr::mutate(treatment_change = treatment - temp_lag) %>%
    dplyr::ungroup() %>%
    dplyr::select(-temp_lag)
}
