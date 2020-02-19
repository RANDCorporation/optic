#' apply exposure
#' 
#' @importFrom magrittr %>%
#' 
#' @export
apply_exposure_concurrent <- function(treated_units, ConfigObject) {
  ConfigObject$data$treatment1 <- 0
  ConfigObject$data$treatment2 <- 0
  for (t_state in names(treated_units)) {
    for (i in 1:length(treated_units[[t_state]][["policy1_years"]])) {
      yr <- treated_units[[t_state]][["policy1_years"]][i]
      exposure <- treated_units[[t_state]][["exposure1"]][i]
      
      #not sure how to fix this
      ConfigObject$data <- ConfigObject$data %>%
        dplyr::mutate(treatment1 = ifelse(state == t_state & year == yr, exposure, treatment1))
    }
      for (i in 1:length(treated_units[[t_state]][["policy2_years"]])) {
        yr <- treated_units[[t_state]][["policy2_years"]][i]
        exposure <- treated_units[[t_state]][["exposure2"]][i]
        
        #not sure how to fix this
        ConfigObject$data <- ConfigObject$data %>%
          dplyr::mutate(treatment2 = ifelse(state == t_state & year == yr, exposure, treatment2))
      }
  }
}
