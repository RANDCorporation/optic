

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


#' Compute prior control variables
#' 
#' Adds a three-period moving average (level) and a trend (computed as the difference between y_t-1 and y_t-3) to the data set.
#'
#' @param data a data.frame set provided by the user
#' @param unit_var char(1) The unit variable (i.e., state)
#' @param time_var char(1) The time variable (i.e., year)
#' @param outcome_var char(1) The outcome variable
#'
#' @return the same data.frame, with to additional variables called prior_control_level_OLD, and prior_control_trend_OLD
#' @noRd
compute_prior_controls <- function(data, unit_var, time_var, outcome_var) {
  
  # all variables must exist in the data
  stopifnot(all(c(unit_var, time_var, outcome_var) %in% names(data)))
  
  unit_sym <- dplyr::sym(unit_var)
  time_sym <- dplyr::sym(time_var)
  outcome_sym <- dplyr::sym(outcome_var)
  
  # This uses a three-time window moving average by default.
  new_data <- data %>%
    arrange(!!unit_sym, !!time_sym) %>%
    group_by(!!unit_sym) %>%
    mutate(lag1 = lag(!!outcome_sym, n=1L),
           lag2 = lag(!!outcome_sym, n=2L),
           lag3 = lag(!!outcome_sym, n=3L)) %>%
    ungroup() %>%
    rowwise() %>%
    # code in moving average and trend versions of prior control
    mutate(prior_control_level_OLD = mean(c(lag1, lag2, lag3)),
           prior_control_trend_OLD = lag1 - lag3) %>%
    ungroup() %>%
    dplyr::select(-lag1, -lag2, -lag3) %>%
    mutate("{{unit_sym}}" := factor(as.character(!!unit_sym)))
  
  return(new_data)
  
}
