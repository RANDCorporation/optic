#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Validate that input data has no NAs in columns optic must use
#'
#' Checks that the union of `unit_var`, `time_var`, each model's outcome
#' (formula LHS), and each model's covariates (formula RHS minus
#' treatment-construct terms) is NA-free. Treatment columns are simulated
#' by optic and are excluded from this check. Stops with an informative
#' message listing each offending column, its NA count, and a few example
#' (unit, time) pairs when the offending column is not the unit or time
#' variable itself.
#'
#' @param data Empirical data passed to `optic_simulation()`.
#' @param models List of `optic_model` objects.
#' @param unit_var Character. Unit identifier column name.
#' @param time_var Character. Time variable column name.
#' @return TRUE invisibly if all columns are NA-free; otherwise stops with
#'   an informative error.
#' @keywords internal
#' @noRd
.validate_optic_input_nas <- function(data, models, unit_var, time_var) {
  treatment_terms <- c("treatment_level", "treatment_change", "treatment",
                       "treatment1_level", "treatment2_level",
                       "treatment1_change", "treatment2_change",
                       "trt_ind")

  required_cols <- character(0)
  required_cols <- c(required_cols, unit_var, time_var)
  for (m in models) {
    outcome <- tryCatch(get_model_outcome(m), error = function(e) NULL)
    if (!is.null(outcome)) {
      required_cols <- c(required_cols, outcome)
    }
    fml <- m[["model_formula"]]
    if (inherits(fml, "formula")) {
      rhs <- model_terms(fml)$rhs
      rhs <- rhs[!rhs %in% treatment_terms]
      required_cols <- c(required_cols, rhs)
    }
  }
  required_cols <- unique(required_cols)
  required_cols <- required_cols[required_cols %in% colnames(data)]

  if (length(required_cols) == 0) return(invisible(TRUE))

  na_counts <- vapply(required_cols,
                      function(col) sum(is.na(data[[col]])),
                      integer(1))
  offenders <- required_cols[na_counts > 0]
  if (length(offenders) == 0) return(invisible(TRUE))

  stop(.format_optic_input_na_error(
    data, offenders, na_counts[offenders],
    unit_var = unit_var, time_var = time_var
  ), call. = FALSE)
}

#' Format an informative NA-input error message
#'
#' @keywords internal
#' @noRd
.format_optic_input_na_error <- function(data, offenders, counts,
                                         unit_var, time_var,
                                         max_examples = 3L) {
  lines <- c(
    "Input data passed to optic_simulation() contains NAs in required",
    "column(s). optic uses these columns directly to simulate treatment",
    "assignment, compute prior controls, and fit the supplied models.",
    "Treatment columns are simulated by optic and are not checked here.",
    "",
    "Columns with NAs:"
  )
  for (col in offenders) {
    n_na <- counts[[col]]
    examples <- character(0)
    if (col != unit_var && col != time_var &&
        unit_var %in% colnames(data) && time_var %in% colnames(data)) {
      bad_idx <- which(is.na(data[[col]]))
      if (length(bad_idx) > 0) {
        idx <- utils::head(bad_idx, max_examples)
        examples <- vapply(
          idx,
          function(i) sprintf("%s=%s, %s=%s",
                              unit_var, format(data[[unit_var]][i]),
                              time_var, format(data[[time_var]][i])),
          character(1)
        )
      }
    }
    msg <- sprintf("  - %s: %d NA values", col, n_na)
    if (length(examples) > 0) {
      msg <- paste0(
        msg, " (e.g., ",
        paste(examples, collapse = "; "),
        if (n_na > length(examples)) ", ..." else "",
        ")"
      )
    }
    lines <- c(lines, msg)
  }
  lines <- c(
    lines,
    "",
    "Remove or impute NA rows before passing the data to optic_simulation()."
  )
  paste(lines, collapse = "\n")
}
