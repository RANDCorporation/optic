effect_magnitude <- function(ConfigObject) {
  stopifnot("OpticConfig" %in% class(ConfigObject))
  UseMethod("effect_magnitude", ConfigObject)
}


effect_magnitude.linear <- function(ConfigObject) {
  target_deaths <- ConfigObject$target_deaths
  total_population <- ConfigObject$total_population
  number_years <- ConfigObject$number_implementation_years
  effect_direction <- ConfigObject$effect_direction
  ave_pop_100K <- (total_population / number_years) / 100000
  true_effect <- target_deaths / ave_pop_100K
  if (effect_direction == "neg") {
    true_effect <- -1 * true_effect
  }
  return(true_effect)
}


effect_magnitude.log <- function(ConfigObject) {
  target_deaths <- ConfigObject$target_deaths
  total_deaths <- ConfigObject$total_deaths
  number_years <- ConfigObject$number_implementation_years
  effect_direction <- ConfigObject$effect_direction
  ave_per_yr <- total_deaths / number_years
  percent_change <- target_deaths / ave_per_yr
  if (effect_direction == "neg") {
    true_effect <- 1 - percent_change
  } else {
    true_effect <- 1 + percent_change
  }
  return(true_effect)
}


effect_magnitude.loglinear <- function(ConfigObject) {
  target_deaths <- ConfigObject$target_deaths
  total_deaths <- ConfigObject$total_deaths
  number_years <- ConfigObject$number_implementation_years
  effect_direction <- ConfigObject$effect_direction
  ave_per_yr <- total_deaths / number_years
  percent_change <- target_deaths / ave_per_yr
  if (effect_direction == "neg") {
    true_effect <- 1 - percent_change
  } else {
    true_effect <- 1 + percent_change
  }
  return(true_effect)
}
