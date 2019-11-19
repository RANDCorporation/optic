#' calculate the effect magnitude for log model
#' 
#' @param target_deaths target number of deaths per 100,000 for true effect
#' @param total_deaths total number of deaths over entire time period (sum across years)
#' @param number_years number of years to observe effect
#' @param effect_direction "pos" (default) or "neg"
loglinear_effect_magnitudes <- function(target_deaths, total_deaths, number_years, effect_direction) {
  ave_per_yr <- total_deaths / number_years
  percent_change <- target_deaths / ave_per_yr
  if (effect_direction == "neg") {
    true_effect <- 1 - percent_change
  } else {
    true_effect <- 1 + percent_change
  }
  return(true_effect)
}