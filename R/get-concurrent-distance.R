#' get distance between concurrent policies for each treated unit and summarize
get_distance_avgs <- function(treated_units, units="days") {
  distances <- c()
  for (treated in names(treated_units)) {
    t1_date <- paste(min(treated_units[[treated]][["policy_years1"]]),
                     treated_units[[treated]][["policy_month1"]],
                     "01", sep="-")
    t2_date <- paste(min(treated_units[[treated]][["policy_years2"]]),
                     treated_units[[treated]][["policy_month2"]],
                     "01", sep="-")
    t1_date <- as.Date(t1_date, "%Y-%m-%d")
    t2_date <- as.Date(t2_date, "%Y-%m-%d")
    
    dist <- abs(as.numeric(difftime(t1_date, t2_date, units=units)))
    distances <- c(distances, dist)
  }
  
  return(distances)
}