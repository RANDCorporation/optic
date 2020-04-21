#' get treated units
#' 
#' @importFrom magrittr %>%
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
    
    #TODO: this is currently specific to time_var being year and wanting to
    #      sample on months; need to abstract this functionality so it's not
    #      hardocded
    if (concurrent) {
      sampled_time_period <- sample(available_periods, 1) #this becomes the mean
      data = MASS::mvrnorm(n=200, mu=c(sampled_time_period, sampled_time_period), Sigma=matrix(c(1, rho, rho, 1), nrow=2), empirical=TRUE) #odd - can't set n = 1 so have to sample two
      sampled_time_period1 = data[1, 1]  # standard normal (mu=yr, sd=1)
      sampled_time_period2 = data[1, 2]  # standard normal (mu=yr, sd=1)
      #cor(yr1,yr2) #if increasen n to 200; can confirm that correlation = rho with large samples
      
      #TODO:
      #now we have continuous enactment dates - would be nice if we could just work with these in our slow and instant coding
      #ask Geoff if he could work on that code
      
      #for now - I will just group to nearest month - very blunt approach
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
      l1 <- exposure_list(sampled_time_period1, mo1, available_periods, policy_speed, n_implementation_periods)
      names(l1) <- paste0(names(l1), "1")
      l2 <- exposure_list(sampled_time_period2, mo2, available_periods, policy_speed, n_implementation_periods)
      names(l2) <- paste0(names(l2), "2")
      treated[[sunit]] <- c(l1, l2)
    } else {
      treated[[sunit]] <- exposure_list(sampled_time_period, mo, available_periods, policy_speed, n_implementation_periods)
    }
  }
  
  return(treated)
}

exposure_list <- function(sampled_time_period, mo, available_periods, policy_speed, n_implementation_periods) {
  if (policy_speed == "instant") {
    l <- list(
      policy_years = sampled_time_period:max(available_periods, na.rm=TRUE),
      policy_month = mo,
      exposure = c((12 - mo + 1)/12, rep(1, length((sampled_time_period + 1):max(available_periods, na.rm=TRUE))))
    )
  } else if (policy_speed == "slow") {
    l <- list(
      policy_years = sampled_time_period:max(available_periods, na.rm=TRUE),
      policy_month = mo,
      exposure = calculate_exposure(mo, n_implementation_periods)
    )
    
    n <- length(l[["policy_years"]])
    exposure <- l[["exposure"]]
    if (n < length(exposure)) {
      l[["exposure"]] <- exposure[1:n]
    } else {
      n_more_years <- n - length(exposure)
      l[["exposure"]] <- c(exposure, rep(1, n_more_years))
    }
  }
  
  return(l)
}


