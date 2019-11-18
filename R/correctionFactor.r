#' calculate the correction factor
#' 
#' @param test_stats test statistics
#' @param type what type of test statistic is being supplied; one of "t", "wald"; default is "t"
correctionFactor <- function(test_stats, type="t") {
  if (type == "t") {
    f_stats <- (test_stats)^2
    f_stats <- sort(f_stats)
    high_cut <- 0.95 * length(test_stats)
    femp95 <- f_stats[high_cut]
    freal <- qf(0.95, 1, Inf)
    corr_factor <- sqrt(femp95 / freal)
  }
  
  return(corr_factor)
}

