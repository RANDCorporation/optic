#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Stop if input data has NAs in any column optic must use
#'
#' Validates the union of `unit_var`, `time_var`, each model's outcome,
#' and each model's covariates (formula RHS minus treatment-construct
#' terms). Treatment columns are simulated by optic and excluded.
#'
#' @keywords internal
#' @noRd
.validate_optic_input_nas <- function(data, models, unit_var, time_var) {
  trt_terms <- c("treatment_level", "treatment_change", "treatment",
                 "treatment1_level", "treatment2_level",
                 "treatment1_change", "treatment2_change", "trt_ind")

  cols <- c(unit_var, time_var)
  for (m in models) {
    cols <- c(cols, tryCatch(get_model_outcome(m), error = function(e) NULL))
    fml <- m[["model_formula"]]
    if (inherits(fml, "formula")) {
      rhs <- model_terms(fml)$rhs
      cols <- c(cols, rhs[!rhs %in% trt_terms])
    }
  }
  cols <- unique(cols)
  cols <- cols[cols %in% colnames(data)]
  if (length(cols) == 0) return(invisible(TRUE))

  na_counts <- vapply(cols, function(c) sum(is.na(data[[c]])), integer(1))
  offenders <- cols[na_counts > 0]
  if (length(offenders) == 0) return(invisible(TRUE))

  stop(.format_optic_input_na_error(data, offenders, na_counts[offenders],
                                    unit_var, time_var), call. = FALSE)
}

.format_optic_input_na_error <- function(data, offenders, counts,
                                         unit_var, time_var, max_ex = 3L) {
  lines <- c(
    "Input data passed to optic_simulation() contains NAs in required column(s).",
    "Treatment columns are simulated by optic and are not checked here.",
    "",
    "Columns with NAs:"
  )
  has_keys <- unit_var %in% colnames(data) && time_var %in% colnames(data)
  for (col in offenders) {
    msg <- sprintf("  - %s: %d NA values", col, counts[[col]])
    if (has_keys && !col %in% c(unit_var, time_var)) {
      idx <- utils::head(which(is.na(data[[col]])), max_ex)
      ex <- sprintf("%s=%s, %s=%s",
                    unit_var, format(data[[unit_var]][idx]),
                    time_var, format(data[[time_var]][idx]))
      more <- if (counts[[col]] > length(idx)) ", ..." else ""
      msg <- paste0(msg, " (e.g., ", paste(ex, collapse = "; "), more, ")")
    }
    lines <- c(lines, msg)
  }
  paste(c(lines, "",
          "Remove or impute NA rows before passing the data to optic_simulation()."),
        collapse = "\n")
}
