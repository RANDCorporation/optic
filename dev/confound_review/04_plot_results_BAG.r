# plot-results-confounding.R ----------------------------------------------
# 
# Plot summarized results of the selection bias simulations
# 
# Pedro Nascimento de Lima, Beth Ann Griffin, Max Griswold

# load libraries ----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(glue)
library(tidyr)
library(yaml)
library(randplot)

setwd("C:/Users/griswold/Documents/github/optic/dev/confound_review/")

# source relevant scripts -------------------------------------------------
source("plots_and_tables.R")

w <- 9
h <- 5.25
text_size <- 20

# Define statistics to plot
stats <- c("Bias (effect size scale)","Bias (original scale)", "Variance","Empirical variance", "RMSE", "Coverage") 

# define output path
outpath <- 'C:/Users/griswold/Documents/github/optic/dev/confound_review'

# set a datestamp
datestamp <- Sys.Date()

# If save folder does not exist, make folder:
if (!dir.exists(glue('{outpath}/{datestamp}-figures'))){
  dir.create(glue('{outpath}/{datestamp}-figures'))
}

# load the summarized data
sim_summaries_no_TE <- readRDS(glue('{outpath}/summarized_results_full_runs_noTE2022-11-16.Rds'))

#updating coverage to be correct
sim_summaries_no_TE$coverage <- 1 - sim_summaries_no_TE$type_1_error

# load the summarized data
sim_summaries_TE <- readRDS(glue('{outpath}/summarized_results_full_runs_TE2022-12-20.Rds'))

# Max edit: Recalculate coverage to see if this fixes missing coverage bars
# Max further edit: This did not fix that particular issue.

#updating coverage to be correct
sim_summaries_TE$coverage <- 1 - sim_summaries_TE$type_1_error

#notice we have variability in coverage here
sim_summaries_TE$coverage

################################################################

# And data sets:
data_sets <- list(no_TE = sim_summaries_no_TE, TE = sim_summaries_TE)

# Produce results for each dataset and statistic:

# remove prior figures:
# get all files in the directories, recursively

f <- list.files(glue("{outpath}/{datestamp}-figures"), include.dirs = F, full.names = T, recursive = T)

# remove the files
file.remove(f)

fig_n = 1
for (d in names(data_sets)) {
  
  adjust_coverage <- ifelse(d == "no_TE", T, F)
  
  for(stat in stats) {
    data_sets[[d]] %>%
      prep_plot_data(., adjust_coverage = adjust_coverage) %>%
      makeplot(., statistic = stat, ncol = 2)
    
    ggsave(glue('{outpath}/{datestamp}-figures/fig_{fig_n}_{d}_{stat}.eps'), width=w, height=h,units = "in", bg = "white", scale = 1.2)
    ggsave(glue('{outpath}/{datestamp}-figures/fig_{fig_n}_{d}_{stat}.pdf'), width=w, height=h,units = "in", bg = "white", scale = 1.2)
    
   fig_n <- fig_n + 1 
   
  }
}

