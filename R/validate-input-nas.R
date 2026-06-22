# Stop if input data has NAs in columns optic uses across all models.
.validate_optic_input_nas <- function(data, models, unit_var, time_var) {
  trt <- c("treatment_level", "treatment_change", "treatment", "trt_ind",
           "treatment1_level", "treatment2_level", "treatment1_change", "treatment2_change")
  cols <- c(unit_var, time_var)
  for (m in models) {
    cols <- c(cols, tryCatch(get_model_outcome(m), error = function(e) NULL))
    if (inherits(m[["model_formula"]], "formula")) {
      rhs <- model_terms(m[["model_formula"]])$rhs
      cols <- c(cols, rhs[!rhs %in% trt])
    }
  }
  cols <- intersect(unique(cols), colnames(data))
  n <- vapply(cols, function(c) sum(is.na(data[[c]])), integer(1))
  bad <- cols[n > 0]; if (!length(bad)) return(invisible(TRUE))
  has_keys <- all(c(unit_var, time_var) %in% colnames(data))
  msgs <- vapply(bad, function(col) {
    h <- sprintf("  - %s: %d NA values", col, n[[col]])
    if (!has_keys || col %in% c(unit_var, time_var)) return(h)
    idx <- utils::head(which(is.na(data[[col]])), 3L)
    ex <- sprintf("%s=%s, %s=%s", unit_var, format(data[[unit_var]][idx]),
                  time_var, format(data[[time_var]][idx]))
    paste0(h, " (e.g., ", paste(ex, collapse = "; "),
           if (n[[col]] > length(idx)) ", ..." else "", ")")
  }, character(1))
  stop(paste(c("optic_simulation() requires NA-free unit, time, outcome, and covariate columns.",
               "Treatment columns are simulated by optic and are not checked.", "",
               "Columns with NAs:", msgs, "", "Remove or impute NA rows."),
             collapse = "\n"), call. = FALSE)
}
