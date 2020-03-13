#' @export

get_treated_units_concurrent <- function(ConfigObject, policy_speed,rho) {
  #ConfigObject
  #policy_speed=ConfigObject$policy_speed
  
  # randomly sample the states
  states <- sample(unique(ConfigObject$data$state), ConfigObject$n_states, replace=FALSE)
  
  # randomly sample the year and month the policy will take effect for each state
  treated <- list()
  for (state in states) {
    #need to comment this part out because it won't run for me
    #    if (is.null(ConfigObject$time_periods)) {
    #      available_years <- unique(ConfigObject$data$year)
    #    } else {
    #      available_years <- unique(
    #        ConfigObject$data %>%
    #          dplyr::filter(year %in% ConfigObject$time_periods) %>%
    #          dplyr::pull(year)
    #      )
    #    }
    #instead I just set the available years
    #j=1
    #state=states[j]
  
    available_years=2003:2013  
    #generating two years of data; can update to 2017/2018
    
    #rho=0.83 #note - we will explore a range of correlations = 0, 0.25, 0.5, 0.75, 0.9, 1.0
    
    yr <- sample(available_years, 1) #this becomes the mean
    data = mvrnorm(n=200, mu=c(yr, yr), Sigma=matrix(c(1, rho, rho, 1), nrow=2), empirical=TRUE) #odd - can't set n = 1 so have to sample two
    yr1 = data[1, 1]  # standard normal (mu=yr, sd=1)
    yr2 = data[1, 2]  # standard normal (mu=yr, sd=1)
    #cor(yr1,yr2) #if increasen n to 200; can confirm that correlation = rho with large samples
    
    #now we have continuous enactment dates - would be nice if we could just work with these in our slow and instant coding
    #ask Geoff if he could work on that code
    
    #for now - I will just group to nearest month - very blunt approach
    mo1<-yr1-floor(yr1)
    mo2<-yr2-floor(yr2)
    
    mo1=round(12*mo1)
    mo2=round(12*mo2)
    
    if(mo1==0)
    {
      mo1=1
    }
    if(mo2==0)
    {
      mo2=1
    }
    
    #setting years
    yr1=floor(yr1)
    yr2=floor(yr2)
    
    if (policy_speed == "slow") {
      treated[[state]] <- list(
        policy1_years = yr1:max(ConfigObject$data$year, na.rm=TRUE),
        policy2_years = yr2:max(ConfigObject$data$year, na.rm=TRUE),
        policy1_month = mo1,
        policy2_month = mo2,
        exposure1 = calculate_exposure(mo1, ConfigObject$number_implementation_years),
        exposure2 = calculate_exposure(mo2, ConfigObject$number_implementation_years)
      )
      
      n1 <- length(treated[[state]][["policy1_years"]])
      exposure1 <- treated[[state]][["exposure1"]]
      n2 <- length(treated[[state]][["policy2_years"]])
      exposure2 <- treated[[state]][["exposure2"]]
      if (n1 < length(exposure1)) {
        treated[[state]][["exposure1"]] <- exposure1[1:n1]
      } else {
        n1_more_years <- n1 - length(exposure1)
        treated[[state]][["exposure1"]] <- c(exposure1, rep(1, n1_more_years))
      }
      if (n2 < length(exposure2)) {
        treated[[state]][["exposure2"]] <- exposure2[1:n2]
      } else {
        n2_more_years <- n2 - length(exposure2)
        treated[[state]][["exposure2"]] <- c(exposure2, rep(1, n2_more_years))
      }
    } else if (policy_speed == "instant") {
      treated[[state]] <- list(
        policy1_years = yr1:max(ConfigObject$data$year, na.rm=TRUE),
        policy2_years = yr2:max(ConfigObject$data$year, na.rm=TRUE),
        policy1_month = mo1,
        policy2_month = mo2,
        exposure1 = c((12 - mo1 + 1)/12, rep(1, length((yr1 + 1):max(ConfigObject$data$year, na.rm=TRUE)))),
        exposure2 = c((12 - mo2 + 1)/12, rep(1, length((yr2 + 1):max(ConfigObject$data$year, na.rm=TRUE))))
      )
    }
  }
  
  return(treated)
}

#notes from call on 1/27/2020
#outcome - simple additive; still an argument for positive/negative; maybe do additive but then disentangle
#one harmful and one helpful; interactive
#of interest - joint estimate of laws might be ok
#maybe additive outcome model - not so good

#of interest - model choice - huge thing to study - control for one versus control for both; many times may miss something

#concrete paper 1 idea: model specification - omitted key policies that have been implemented - exploration of model misspecification (could do one paper just on this)
#synergistic effects; indepent effects; negative effects; here is performance when don't control
#here is performance when you simultaneously control
#then maybe shed light on the issue of joint modeling of these

#option 2 - 

#use a different model approach if laws are adopted together; post treatment effect would be 

#GOALS - what are the errors in this literature?

#picking best model performance + classic two ways; simplified to optimal model + two way DID
#choose one model and just stick with new question for main manuscript and then put others in the appendix

#this is about varying the distance between law 1 and law 2; time-varying policy effects then you will have a time-varying policy effect
#data generating - wouldn't there be a time-varying policy estimate

#use of event study to help might be useful to explore

#need to write


