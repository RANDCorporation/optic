

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# dplyr is used in this package; this fixes some of the issues created by it:
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
#' To produce this list run the CRAN check, copy the undefined global functions or variables that appear in the check. Paste them into sublime, replace spaces with a new line dash n, then use command shift L to edit all lines and format approproately 
#' @name globalvariables definitions
#' @noRd
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @import dplyr
utils::globalVariables(c("treatment1", 
                         "treatment2", 
                         "temp_lag1", 
                         "temp_lag2", 
                         "treatment_change1", 
                         "treatment_change2", 
                         "treatment", 
                         "temp_lag", 
                         "treatment_change", 
                         "estimate", 
                         "se", 
                         "variance", 
                         "t_stat", 
                         "p_value",
                         "mse",
                         "update.formula",
                         ".",
                         ":=",
                         "One_minus_trt_pr",
                         "X1",
                         "X2",
                         "es",
                         "es_prior",
                         "es_conf",
                         "feols",
                         "isfirst",
                         "lag1",
                         "lag2",
                         "lag3",
                         "logits",
                         "mu0",
                         "mu0_prior",
                         "mu0_conf",
                         "mu1",
                         "mu1_prior",
                         "mu1_conf",
                         "n_trt",
                         "number_implementation_years",
                         "prior_control",
                         "prior_control_old",
                         "sd_prior",
                         "sd_conf",
                         "treatment_date",
                         "trt_ind",
                         "trt_ind_new",
                         "trt_pr",
                         "year"))



