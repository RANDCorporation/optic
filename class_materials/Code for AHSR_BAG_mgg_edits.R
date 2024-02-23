#AHSR Example Code
library(haven)
library(dplyr)
library(fixest)

#AR model 

dta <- read_dta("C:/users/griswold/Documents/GitHub/optic/class_materials/AHSR-naloxone-case-study.dta")
# All states

#Generate lagged outcome and treatment indicator
dta <- 
  dta %>%
  arrange(state_id, year) %>% 
  group_by(state_id) %>%
  mutate(lag1=dplyr::lag(OD_rate,n=1,default=NA),lag1.trt=dplyr::lag(standing_order_policy,n=1,default=NA))

#drop first year in time series which will have lag as an NA
dta=dta[dta$year>1999,]

#Compute change code for policy indicator
dta$policy_change=dta$standing_order_policy - dta$lag1.trt

#Fitting the linear AR model
ar=lm(OD_rate ~ policy_change+lag1+as.factor(year)+unemployment_rate,
      data=dta)
summary(ar)

##for max - need to create time to treat variable so this model works from our sim runs. what is easiest way to do that?!?

#Max edits: I find it easiest to calculate a time-to-treat variable directly in the data. It looks like I can
#use "standing_order_cohort" to calculate the first year of treatment. But I assuming I only had the binary treatment
# variable, I would do the following:

# Code first treatment period. For a given state, see if that state
# is treated. If so, set treatment year to the minimum across treatment years.
# If not, set treatment year to Inf.
dta <- dta %>%
       group_by(state_id) %>%
       mutate(treat_year = ifelse(max(standing_order_policy == 1), 1 + max(year ^ (1 - standing_order_policy)), -Inf))

# Now, use treatment year to calculate time-since-treatment variables:
dta <- dta %>%
       mutate(tst = year - treat_year)

# Max edits: I modified the form to match the earlier specification.
form_ar_es <- formula(OD_rate ~ unemployment_rate + lag1 + i(tst, ref = c(-1, Inf)) | as.factor(year))
ar_es <- feols(form_ar_es, dta) 

# Max edits: Compare AR model's effect to aggregated effect from event study:
att_ar    <- summary(ar)$coefficients["policy_change",]
att_ar_es <- aggregate(ar_es, c("ATT" = "tst::([[:digit:]]+)"))