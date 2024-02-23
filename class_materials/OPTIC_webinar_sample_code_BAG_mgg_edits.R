# OPTIC Webinar Code

setwd("C:/Users/griswold/Documents/Github/optic/class_materials/")

#code to use for reading in data set
#setwd("FILL IN FOLDER LOCATION WHERE SAVED CASE STUDY DATASET")

OPTIC_casestudy_data<-read.table("OPTIC_casestudy_data.csv",sep=",",h=T)

# Uses OPTIC_casestudy_data
dta <- OPTIC_casestudy_data

#installation notes for new packages
#when its the first time using many of these library
#use install.packages("library.name") 
#for augsynth - you will need to type
#install.packages("devtools")
#devtools::install_github("ebenmichael/augsynth")

################################################################################
################################################################################

# Case Study Example 1: Callaway & Sant'Anna staggered difference-in-differences

# Excludes 2010 (Illinois) and 2013 (North Carolina) cohorts because of small cohort size
dta1 <- subset(dta, !(state %in% c("Illinois", "North Carolina")))

#Max - do you have the code to create the needed policy indicator for CSA to work has to be coded as 0 for control states & the year of adoption for treated states? 
#it is already saved in the dataset but I think we should show folks how to do this!!!

# max -> Beth Ann: Here's some code to do it using "date_nal_protocol_standing" variable which indicated the
# policy's implementation date. I used only base R functions to generate this variable and demonstrate
# some analyst choices, though this would be 1-2 lines of code using a package like dplyr or data.table

# Transform the character variable indicating the policy's first treatment period into
# year and month variables using the as.date and format functions. Make sure to
# set the variable to a numeric type.

# I am letting R infer the right date format. If the date looked differently, then
# an additional argument would be needed to specify the date type
dta1$implementation_date <- as.Date(dta1$date_nal_protocol_standing)

# Extract the year and month from the date variable
dta1$cohort_year <- as.numeric(format(dta1$implementation_date, "%Y"))
dta1$cohort_month <- as.numeric(format(dta1$implementation_date, "%m"))

# If the cohort-year is missing, set year to zero so that CSA works correctly:
dta1[is.na(dta1$cohort_year),]$cohort_year <- 0
dta1[is.na(dta1$cohort_month),]$cohort_month <- 0

# For treated units, set the cohort year to the next year, if treatment began occurring after June:
dta1[dta1$cohort_month > 6,]$cohort_year <-  dta1[dta1$cohort_month > 6,]$cohort_year + 1

# If the implementation date is greater than the observed date in the dataset, set 
# cohort year to zero (since for the purposes of this dataset, the unit was untreated
# during the study period)

dta1[dta1$cohort_year > max(dta1$year),]$cohort_year <- 0

# Now, check that the new cohort_year variable equals the previous
# "standing_order_cohort" variable. 

dta1[(dta1$standing_order_cohort != dta1$cohort_year)]

# No rows were printed indicating we have agreement with the existing cohort-year coding!

library(did)

# Estimate group-time average effects
csa_attgt <- att_gt(  
  yname = "OD_rate",
	tname = "year",
  idname = "state_id", #this idname has to be numeric not a character
	gname = "standing_order_cohort", #very importantly - the policy indicator for CSA to work has to be coded as 0 for control states & the year of adoption for treated states
  xformla = ~unemployment_rate,  
  data = dta1,
  allow_unbalanced_panel = TRUE,
  control_group = c("nevertreated"),
  est_method = "dr"
)# Display and graph cohort specific treatment effects 
summary(csa_attgt)
ggdid(csa_attgt)

# Average effects by length of exposure to treatment 
csa_agg_adj_balance <- aggte(csa_attgt, type = "dynamic")
summary(csa_agg_adj_balance)
ggdid(csa_agg_adj_balance)

# Average effects by length of exposure to treatment with balanced panel (same number of pre- and post-periods from each cohort)
csa_agg_adj_balance <- aggte(csa_attgt, type = "dynamic", min_e = -5, balance_e=0)
summary(csa_agg_adj_balance)
ggdid(csa_agg_adj_balance)

################################################################################
################################################################################

# Case Study Example 2: Augmented synthetic control 

# Include only Illinois (2010) and never treated control states
dta2 <- dta[dta$standing_order_cohort == 0 | dta$standing_order_cohort == 2010, ]

library(magrittr) 
library(dplyr) 
library(augsynth) 

# Augsynth analysis 
syn <-  augsynth(    
  OD_rate ~ standing_order_policy | unemployment_rate,     #here make sure your policy variable is only 0 and 1s
  unit = state_id,    #this idname has to be numeric not a character
  time = year,   
  data = dta2,    
  progfunc = "ridge",  
  scm = TRUE,          
  fixedeff = TRUE      
)

# Effect estimates: year-specific ATTs
summary(syn)

# Plot time-specific ATTs with 95% confidence interval
plot(syn) 

################################################################################
################################################################################

# Case Study Example 3: Synthetic control for multiple units

# Include full sample

library(magrittr) 
library(dplyr) 
library(augsynth) 

# Multisynth analysis
multi_syn <- multisynth(
  OD_rate ~ standing_order_policy | unemployment_rate, #policy variable has to be 0/1
  unit = state_id, 
  time = year, 
  data = dta,
  n_leads=8,
  time_cohort = TRUE)

# Effect estimates: average effect by length of exposure to treatment 
summary(multi_syn)

# Plot dynamic ATTs (based on length of exposure to treatment) by treatment cohort
plot(multi_syn)

# Plot dynamic ATTs (based on length of exposure to treatment) by treatment cohort
plot(multi_syn, levels = "Average")


################################################################################
################################################################################

# Case study Example 4: Auto-regressive model 

# Include full sample

library(fixest)

# Generate lagged outcome variable and lagged treatment indicator
dta4 <- dta %>%
  arrange(state_id, year) %>% 
  group_by(state_id) %>%
  mutate(lag1=dplyr::lag(OD_rate,n=1,default=NA),
         lag1.trt=dplyr::lag(standing_order_policy,n=1,default=NA))

# Drop first year in time series which will have lag as an NA
dta4=dta4[dta4$year>1999,]

# Compute policy change code using lagged treatment variable
dta4$policy_change = dta4$standing_order_policy - dta4$lag1.trt

# Fitting the linear AR model
ar=lm(OD_rate ~ policy_change + lag1 + as.factor(year) + unemployment_rate,
      data=dta4)
summary(ar)

# AR model with time-varying effects
# Code first treatment period
dta4 <- dta4 %>%
  group_by(state_id) %>%
  mutate(treat_year = ifelse(max(standing_order_policy == 1), 1 + max(year ^ (1 - standing_order_policy)), -Inf))

# Now, use treatment year to calculate time-since-treatment variables
dta4 <- dta4 %>%
  mutate(tst = year - treat_year)

# Fitting the linear AR model with event study specification
form_ar_es <- formula(OD_rate ~ unemployment_rate + lag1 + i(tst, ref = c(-1, Inf)) | as.factor(year))
ar_es <- feols(form_ar_es, dta4) 
att_ar_es <- aggregate(ar_es, c("ATT" = "tst::([[:digit:]]+)"))
summary(ar)
att_ar_es 


################################################################################
################################################################################

# Case study example 5: Interrupted time series

# Include only 2014 cohort
dta5 <- dta[dta$standing_order_cohort == 2014 ,]

# Generate variable of time since policy implementation (2014)
dta5$time_post <-0
dta5$time_post[dta5$year == 2015] <- 1
dta5$time_post[dta5$year == 2016] <- 2
dta5$time_post[dta5$year == 2017] <- 3

# Time series regression 
#Max - two questions here
#1 - is this your favorite way to fit an ITS?
#2 - can you remind me how to code up cluster adjusted SEs here by state_id?

# Max -> Beth Ann: For an ITS design, I would also use the LM function since this
# makes the approach more obvious than using a package and it's a simple design.

# Also, maybe worth noting, depending on the audience: you could use the same specification within a glm framework to 
# add additional model structure, e.g. poisson, logit (though you would of course need to interpret coefficients
# inversing the link). This is also true for all the event/study DiD estimators, though I rarely see economists
# do this (besides Jeffrey Wooldridge).

ts <- lm(OD_rate ~ year + standing_order_policy + time_post + as.factor(state_id) + unemployment_rate, data=dta5)
summary(ts)

# Max -> Beth Ann: For the below plots, why not display counterfactuals using estimated
# effects from the ITS model, rather than using the observed data to construct
# an empirical ATE? (e.g. first line == intercept term + year term, 
# second line == intercept + year term + post-treatment term)

# I think this goes back to a request you made a few months ago about
# implementing a similar procedure as the one below within the OPTIC package 
# (at the time, I didn't understand the 
# distinction you were trying to make between simulating an ATE using the observed
# data v. using the estimated coefficients to construct ATE)

# Max -> Beth Ann: Alternative plot using only model coefficients:

tsc <- coef(ts)

# Observed data for untreated units
plot(dta5$year,
     dta5$OD_rate, bty = 'n', xaxt='n',
     xlab = "Year", ylab = "OD rate",  
     ylim = c(0, 40), xlim = c(2000, 2019),
     col = rgb(0.5, 0.5, 0.5, 0.5), pch = 19)

axis(side = 1, at = seq(2000, 2018, 2))

# Post-treatment year
abline(v = 2014, col = rgb(1, 0.5, 0, 0.8), lw = 2, lty = 2)

# E[Y*|X = 0 & year < post]
segments(y0 = tsc[["(Intercept)"]] + 2000*tsc[["year"]],
         y1 = tsc[["(Intercept)"]] + 2014*tsc[["year"]], 
         x0 = 2000, x1 = 2014,
         col = rgb(0.65, 0.7, 0.95), lwd = 3)

# E[Y* = 0 & year > post]
segments(y0 = tsc[["(Intercept)"]] + 2014*tsc[["year"]],
         y1 = tsc[["(Intercept)"]] + 2018*tsc[["year"]], 
         x0 = 2014, x1 = 2018,
         col = rgb(1, 0.5, 0, 0.8), lwd = 3)

# E[Y*|X = 1 & year > post]
segments(y0 = tsc[["(Intercept)"]] + 2014*tsc[["year"]] + tsc[["standing_order_policy"]] + tsc[["time_post"]],
         y1 = tsc[["(Intercept)"]] + 2018*tsc[["year"]]+  tsc[["standing_order_policy"]] + 3*tsc[["time_post"]], 
         x0 = 2014, x1 = 2018,
         col = rgb(0.65, 0.7, 0.95), lwd = 3)

# ATE for three years post-treatment:
# E[Y*|X = 1 & year > post + 3]] - E[Y*|X = 1 & year > post + 3]
arrows(x0 = 2018.5, x1 = 2018.5, 
       y1 = tsc[["(Intercept)"]] + 2018*tsc[["year"]]+  tsc[["standing_order_policy"]] + 3*tsc[["time_post"]],
       y0 = tsc[["(Intercept)"]] + 2018*tsc[["year"]],
       angle = 90, code = 3, length = 0.05, col = rgb(0.2, 0.2, 0.2, 0.7), 
       lwd = 2)

# Generate predicted values for pre-treatment and post-treatment based on regression results and average at each time point
# Pre-treatment
dta5_1 <- dta5[dta5$standing_order_policy==0,]
pred1 <- predict(ts, newdata=dta5_1)
mean_OD1 <- aggregate(pred1, by = list(dta5_1$year), FUN = mean)

# Post-treatment
dta5_2 <- dta5[dta5$standing_order_policy==1,]
pred2 <- predict(ts, newdata=dta5_2)
mean_OD2 <- aggregate(pred2, by = list(dta5_2$year), FUN = mean)

# Graph results
plot(dta5$year, dta5$OD_rate, xlab="Year", ylab= "OD rate")
lines(mean_OD1$Group.1, mean_OD1$x, col="red", lw=2)
lines(mean_OD2$Group.1, mean_OD2$x, col="red", lw=2)
abline(v=2014, col="black", lw=2, lty=2)


################################################################################
################################################################################

# Case study example 6: Comparative interrupted time series

# Include only 2014 cohort and never treated group 
dta6 <- dta[dta$standing_order_cohort == 0 | dta$standing_order_cohort == 2014, ]

# Generate variable of time since policy implementation (2014)
dta6$time_post <-0
dta6$time_post[dta6$year == 2015] <- 1
dta6$time_post[dta6$year == 2016] <- 2
dta6$time_post[dta6$year == 2017] <- 3

# Generate indicator for treatment states (1) vs. control states (0)
dta6$treated_state<-0
dta6$treated_state[dta6$standing_order_cohort > 0] <- 1

# Generate indicator for pre (0) vs. post (1) policy implementation  
dta6$standing_order_policy[dta6$year < 2014] <- 0
dta6$standing_order_policy[dta6$year > 2013] <- 1

# Generate interaction terms with treatment state indicator
dta6$treated_stateXyear <- dta6$treated_state * dta6$year
dta6$treated_stateXpolicy <- dta6$treated_state * dta6$standing_order_policy
dta6$treated_stateXtime_post <- dta6$treated_state * dta6$time_post

# Comparative ITS regression
#Max - same two questions here
#1 - is this your favorite way to fit an CITS?
#2 - can you remind me how to code up cluster adjusted SEs here by state_id?

# Max -> Beth Ann: One thing I might change would be including the interactions
# within the specification to reduce the amount of code. But that's neither here nor
# there. More substantively, I was under the impression CITS differed from DiD by omitting period &
# unit fixed effect terms? I did a quick lit review and I was surprised by the number
# of specifications different authors are using for this design so not it's not clear
# to me if there's a consistent standard.

# Also, for this model, why include both a post-treat variable & time-since-treat?
# (I don't understand myself why you would need both; the model I made below
# does not include the post-treat variable and results look very similar, only 
# small movements in group averages)

# "*" command in R formula adds both primary and interaction terms for variables while
# ":" only adds the interaction term

# Changing variables names to make the equation a little easier to read:
dta6$ever_treat <- dta6$treated_state
dta6$post_treat <- dta6$standing_order_policy
dta6$state <- as.factor(dta6$state)

cts_max <- lm(OD_rate ~ ever_treat*year + ever_treat*time_post + state + unemployment_rate, data = dta6)

# Max -> Beth Ann: We use the sandwich package in OPTIC, which is what I would personally
# use (if you want to be didactic for the class, I can send you code for calculating
# a sandwich estimator using only model outputs)

library(sandwich)
library(lmtest)

cluster_adjust_se <- function(model, cluster) {
  
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse.cov <- dfc * sandwich::sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- lmtest::coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
  
}

cluster_adjust_se(cts_max, dta6$state)

#Max -> Beth Ann: Rather than do the above, I think we could simply calculate
#clustered standard errors using the below line of code (this produces
# the same se as applying the above function)
coeftest(cts_max, vcovCL(x = cts_max, cluster = dta6$state, type = "HC1"))

cts <- lm(OD_rate ~ treated_state + year + treated_stateXyear+ standing_order_policy + treated_stateXpolicy + time_post + treated_stateXtime_post + as.factor(state_id) + unemployment_rate, data=dta6)
summary(cts)

# Generate predicted values for pre/post-treatment for treated and control groups based on regression results and average at each time point
# Pre-treatment, treated states
dta61 <- dta6[dta6$standing_order_policy==0 & dta6$treated_state==1,]
pred1 <- predict(cts, newdata=dta61)
mean_OD1 <- aggregate(pred1, by = list(dta61$year), FUN = mean)

# Post-treatment, treated states
dta62 <- dta6[dta6$standing_order_policy==1 & dta6$treated_state==1,]
pred2 <- predict(cts, newdata=dta62)
mean_OD2 <- aggregate(pred2, by = list(dta62$year), FUN = mean)

# Pre-treatment, control states
dta63 <- dta6[dta6$standing_order_policy==0 & dta6$treated_state==0,]
pred3 <- predict(cts, newdata=dta63)
mean_OD3 <- aggregate(pred3, by = list(dta63$year), FUN = mean)

# Post-treatment, control states
dta64 <- dta6[dta6$standing_order_policy==1 & dta6$treated_state==0,]
pred4 <- predict(cts, newdata=dta64)
mean_OD4 <- aggregate(pred4, by = list(dta64$year), FUN = mean)

# Graph results
plot(dta6$year, dta6$OD_rate, xlab="Year", ylab= "OD rate")
lines(mean_OD1$Group.1, mean_OD1$x, col="red", lw=2)
lines(mean_OD2$Group.1, mean_OD2$x, col="red", lw=2)
lines(mean_OD3$Group.1, mean_OD3$x, col="blue", lw=2)
lines(mean_OD4$Group.1, mean_OD4$x, col="blue", lw=2)
abline(v=2014, col="black", lw=2, lty=2)