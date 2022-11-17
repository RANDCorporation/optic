#' parse a formula object into its left-hand-side and right-hand-side components
#' 
#' @param x formula to parse
#' 
#' @return list with named elements "lhs" and "rhs"
#' 
#' @noRd
model_terms <- function(x) {
  stopifnot(class(x) == "formula")
  # convert to string
  string_model <- paste(trimws(deparse(x)), collapse=" ")
  # replace all spaces
  string_model <- gsub(" ", "", string_model)
  lhs <- strsplit(string_model, "[~]")[[1]][1]
  rhs <- strsplit(string_model, "[~]")[[1]][2]
  rhs <- strsplit(rhs, "[+]")[[1]]
  
  return(list(lhs=lhs, rhs=rhs))
}