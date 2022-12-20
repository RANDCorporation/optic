
# dplyr is used in this package; this fixes some of the issues created by it:
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
#' @importFrom utils globalVariables
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
                         "crude.rate", 
                         "update.formula"))
