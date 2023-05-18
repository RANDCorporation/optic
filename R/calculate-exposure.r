

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Calculates the exposure rate applied to each year provided month
#' of policy implementation and number of years to full implementation
#' 
#' @param month month of year (as integer) that policy takes effect
#' @param n_years number of months until full implementation in effect
#' @param monthly_effect increment of exposure to apply each month; default is
#'     ((1/n_years) / 12) (constant over the period)
#' @returns A vector of percentages, indicating change in exposure by year (relative to start month)
#' @examples 
#' 
#' # Calculate uniform increase in policy effect which ramps up across 10 years
#' 
#' # Assume policy starts in July of the first year, then continues for 10 years
#' starting_month <- 7
#' implementation_years <- 10
#' 
#' # Assume some policy effect (which is the target effect for simulations)
#' policy_effect <- 2
#' 
#' exposure_by_year <- calculate_exposure(starting_month, implementation_years)
#' 
#' # Based on exposure by year, calculate policy effect by year:
#' plot(policy_effect*exposure_by_year)
#' 
#' @export

calculate_exposure <- function(month, n_years, monthly_effect=(1/n_years)/12) {
  
  top <- n_years - 1
  total_times <- 1:top
  
  # compute year 1 average effect first
  fraction_year_enacted <- (13 - month) / 12
  average_effect_while_enacted <- 0.5 * fraction_year_enacted * (1 / n_years)
  average_effect_over_year1 <- fraction_year_enacted * average_effect_while_enacted
  
  # mid years
  mid_years_values <- average_effect_over_year1 + total_times * (1 / n_years)
  
  # final year
  final_year_value <- ((13 - month) * 1 + (month - 1) - calc_constant(month - 1) * monthly_effect) / 12
  
  return(c(average_effect_over_year1, mid_years_values, final_year_value))
  
}