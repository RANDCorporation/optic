# Show example panel data inference models using a single simulated datset.

library(dplyr)
library(did)
library(augsynth)
library(did2s)
library(didimputation)
library(fixest)
library(tidyr)

setwd("./data/did_sim/")

# For each time-varying method, check to see if it works on the test dataset:
df <- read.csv("test_sim.csv")
unit <- "state"
time <- "year"
n_implementation_periods <- 6

df <- df %>% mutate(treatment = ifelse(treatment > 0, 1, 0))

# Event study:
form_twfe <- formula(crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1, Inf)) | state + year)
twfe <- feols(form_twfe, df, cluster = unit)

# Event study: AR(1)
form_ar <- formula(crude.rate ~ unemploymentrate + lag_outcome + i(time_to_treat, ref = c(-1, Inf)) | year)
ar <- feols(form_twfe, df, cluster = unit)

# Event study: Sun and Abraham
df <- df %>% mutate(treatment_year = ifelse(treatment_year == 0, Inf, treatment_year))

form_sa <- formula(crude.rate ~ unemploymentrate + sunab(treatment_year, year, ref.p = c(-1, .F)) | state + year)
sa <- feols(form_sa, df, cluster = unit)

# CSA
df <- df %>% mutate(treatment_year = ifelse(treatment_year == 0, NA, treatment_year),
                    state = as.numeric(as.factor(state)))

csa <- att_gt(yname = "crude.rate", 
             tname = "year", 
             idname = "state", 
             gname = "treatment_year", 
             xformla = ~ unemploymentrate,
             data = df)

csa <- aggte(csa, type = "dynamic", na.rm = T)

# Multisynth
form_ms <- formula(crude.rate ~ treatment | unemploymentrate)
ms <- multisynth(form = form_ms, state, year, df, n_leads = n_implementation_periods)

# Borusyak, Jaravel, and Spiess
df <- df %>% mutate(treatment_year = ifelse(is.na(treatment_year), 0, treatment_year))

bjs <- did_imputation(data = df, yname = "crude.rate", gname = "treatment_year",
                      tname = "year", idname = "state", horizon = T)

# Gardner
g <- did2s(df, yname = "crude.rate", first_stage = ~ 0 | state + year,
           second_stage = ~i(time_to_treat, ref = c(-1, Inf)), treatment = "treatment",
           cluster_var = "state")