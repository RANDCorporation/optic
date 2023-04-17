

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' get treated units
#' 
#' @importFrom magrittr %>%
#' @noRd
get_treated_units <- function(
  x, n, unit_var, time_var, policy_speed, n_implementation_periods, concurrent, rho, time_period_restriction) {
  # randomly sample units
  sampled_units <- sample(unique(x[[unit_var]]), n, replace=FALSE)
  
  # initialize list to store treated units
  treated <- list()
  # perform sampling and exposure coding
  for (sunit in sampled_units) {
    # randomly sample the time period, check if there is a restriction
    if (!is.null(time_period_restriction)) {
      available_periods <- x[[time_var]][x[[unit_var]] == sunit & x[[time_var]] %in% time_period_restriction]
    } else {
      available_periods <- unique(x[[time_var]])
    }
    
    # This assumes time_var is at the year level.
    if (concurrent) {
      sampled_time_period <- sample(available_periods, 1) #this becomes the mean
      data = MASS::mvrnorm(n=200, mu=c(sampled_time_period, sampled_time_period), Sigma=matrix(c(1, rho, rho, 1), nrow=2), empirical=TRUE) #odd - can't set n = 1 so have to sample two
      # standard normal (mu=yr, sd=1)
      sampled_time_period1 = data[1, 1]
      sampled_time_period2 = data[1, 2]
      #cor(yr1,yr2) #if increasen n to 200; can confirm that correlation = rho with large samples
      
      # months are grouped to nearest month
      mo1<-sampled_time_period1-floor(sampled_time_period1)
      mo2<-sampled_time_period2-floor(sampled_time_period2)
      mo1=round(12*mo1)
      mo2=round(12*mo2)
      if(mo1==0) {
        mo1=1
      }
      if(mo2==0) {
        mo2=1
      }
      
      #setting years
      sampled_time_period1=floor(sampled_time_period1)
      sampled_time_period2=floor(sampled_time_period2)
      
    } else {
      sampled_time_period <- sample(available_periods, 1)
      mo <- sample(1:12, 1)
    }
    
    # exposure coding
    if (concurrent) {
      l1 <- optic::exposure_list(sampled_time_period1, mo1, available_periods, policy_speed, n_implementation_periods)
      names(l1) <- paste0(names(l1), "1")
      l2 <- optic::exposure_list(sampled_time_period2, mo2, available_periods, policy_speed, n_implementation_periods)
      names(l2) <- paste0(names(l2), "2")
      treated[[sunit]] <- c(l1, l2)
    } else {
      treated[[sunit]] <- optic::exposure_list(sampled_time_period, mo, available_periods, policy_speed, n_implementation_periods)
    }
  }
  
  return(treated)
}