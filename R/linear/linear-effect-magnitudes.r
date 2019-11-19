#' calculate the effect magnitude for linear model
#' 
#' @param target_deaths target number of deaths per 100,000 for true effect
#' @param total_population total population over entire time period (sum across years)
#' @param number_years number of years to observe effect
#' @param effect_direction "pos" (default) or "neg"
linear_effect_magnitudes <- function(target_deaths, total_population, number_years, effect_direction) {
  ave_pop_100K <- (total_population / number_years) / 100000
  true_effect <- target_deaths / ave_pop_100K
  if (effect_direction == "neg") {
    true_effect <- -1 * true_effect
  }
  return(true_effect)
}