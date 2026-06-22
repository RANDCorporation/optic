#' Stop if input data has NAs in any column optic must use
#'
#' Validates the union of `unit_var`, `time_var`, each model's outcome,
#' and each model's covariates (formula RHS minus treatment-construct
#' terms). Treatment columns are simulated by optic and excluded.
#'
#' @keywords internal
#' @noRd
.validate_optic_input_nas <- function(data, models, unit_var, time_var) {
  trt <- c("treatment_level", "treatment_change", "treatment",
           "treatment1_level", "treatment2_level",
           "treatment1_change", "treatment2_change", "trt_ind")
  cols <- c(unit_var, time_var)
  for (m in models) {
    cols <- c(cols, tryCatch(get_model_outcome(m), error = function(e) NULL))
    if (inherits(m[["model_formula"]], "formula")) {
      rhs <- model_terms(m[["model_formula"]])$rhs
      cols <- c(cols, rhs[!rhs %in% trt])
    }
  }
  cols <- unique(cols); cols <- cols[cols %in% colnames(data)]
  if (!length(cols)) return(invisible(TRUE))
  n <- vapply(cols, function(c) sum(is.na(data[[c]])), integer(1))
  bad <- cols[n > 0]
  if (!length(bad)) return(invisible(TRUE))

  msgs <- vapply(bad, function(col) {
    head_msg <- sprintf("  - %s: %d NA values", col, n[[col]])
    if (col %in% c(unit_var, time_var) ||
        !all(c(unit_var, time_var) %in% colnames(data))) return(head_msg)
    idx <- utils::head(which(is.na(data[[col]])), 3L)
    ex <- sprintf("%s=%s, %s=%s",
                  unit_var, format(data[[unit_var]][idx]),
                  time_var, format(data[[time_var]][idx]))
    more <- if (n[[col]] > length(idx)) ", ..." else ""
    paste0(head_msg, " (e.g., ", paste(ex, collapse = "; "), more, ")")
  }, character(1))

  stop(paste(c(
    "Input data passed to optic_simulation() contains NAs in required column(s).",
    "Treatment columns are simulated by optic and are not checked here.",
    "", "Columns with NAs:", msgs, "",
    "Remove or impute NA rows before passing the data to optic_simulation()."
  ), collapse = "\n"), call. = FALSE)
}
