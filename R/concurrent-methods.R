#######################
### SAMPLING METHOD ###
#######################

#' perform sampling and coding of treatment for concurrent policy simulations
#' 
#' @description todo
#'
#' @param single_simulation object created from SimConfig$setup_single_simulation()
concurrent_sample <- function(single_simulation) {
  x <- single_simulation$data
  n <- single_simulation$n_units
  unit_var <- single_simulation$unit_var
  time_var <- single_simulation$time_var
  n_implementation_periods <- single_simulation$n_implementation_periods
  rho <- single_simulation$rho
  time_period_restriction <- single_simulation$time_period_restriction
  
  # sample treated units
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
    #      sample on months; perhaps look into abstracting for any units of time
    sampled_time_period <- sample(available_periods, 1) #this becomes the mean
    data = MASS::mvrnorm(n=200, mu=c(sampled_time_period, sampled_time_period), Sigma=matrix(c(1, rho, rho, 1), nrow=2), empirical=TRUE) #odd - can't set n = 1 so have to sample two
    sampled_time_period1 = data[1, 1]  # standard normal (mu=yr, sd=1)
    sampled_time_period2 = data[1, 2]  # standard normal (mu=yr, sd=1)
    #cor(yr1,yr2) #if increasen n to 200; can confirm that correlation = rho with large samples
    
    #TODO:
    #now we have continuous enactment dates - would be nice if we could just work with these in our slow and instant coding
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
  
    # exposure coding
    l1 <- exposure_list(sampled_time_period1, mo1, available_periods, policy_speed, n_implementation_periods)
    names(l1) <- paste0(names(l1), "1")
    l2 <- exposure_list(sampled_time_period2, mo2, available_periods, policy_speed, n_implementation_periods)
    names(l2) <- paste0(names(l2), "2")
    treated[[sunit]] <- c(l1, l2)
  }
  
  # calulate the distance (in days) between the two policy start dates
  policy_distances <- get_distance_avgs(treated)
  
  # apply treatment to data
  x$treatment1 <- 0
  x$treatment2 <- 0
  
  for (sunit in names(treated)) {
    for (i in 1:length(treated[[sunit]][["policy_years1"]])) {
      time_period <- treated[[sunit]][["policy_years1"]][i]
      exposure <- treated[[sunit]][["exposure1"]][i]
      
      x$treatment1 <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment1
      )
    }
    for (i in 1:length(treated[[sunit]][["policy_years2"]])) {
      time_period <- treated[[sunit]][["policy_years2"]][i]
      exposure <- treated[[sunit]][["exposure2"]][i]
      
      x$treatment2 <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment2
      )
    }
  }
  
  single_simulation$data <- x
  single_simulation$policy_distances <- policy_distances
  
  return(single_simulation)
}
  
