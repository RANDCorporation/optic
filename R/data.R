

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' OPTIC Overdoses example data.
#'
#' An example dataset for performing simulations using the OPTIC library,
#' consisting of state-year overdose data from the US Bureau of Labor Statistics, 
#' the Centers from Disease Control and Prevention, and IQVIA Xponent.
#'
#' @format A data frame with 969 rows and 7 variables:
#' \describe{
#'   \item{state}{US state}
#'   \item{year}{Year}
#'   \item{population}{Population estimate (Centers for Disease Control and Prevention, National Center for Health Statistics)}
#'   \item{unemploymentrate}{Average annual unemployment rate (US Bureau of Labor Statistics)}
#'   \item{opioid_rx}{Estimated number of annual opioid prescriptions dispensed per 100 residents (Centers for Disease Control and Prevention, IQVIA)}
#'   \item{deaths}{Annual number of drug-induced deaths (all drug overdose) (Centers for Disease Control and Prevention, National Center for Health Statistics)}
#'   \item{crude.rate}{Crude rate of drug-induced deaths (all drug overdose) per 100,000 residents (Centers for Disease Control and Prevention, National Center for Health Statistics)}
#' }
#' @source 
#' US Bureau of Labor Statistics. Local Area Unemployment Statistics April 2019 release. Accessed at https://www.bls.gov/lau/.
#' 
#' Centers for Disease Control and Prevention, National Center for Health Statistics. Multiple Cause of Death 1999-2019 on CDC WONDER Online Database, released in 2020. Data are from the Multiple Cause of Death Files, 1999-2019, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at \url{http://wonder.cdc.gov/mcd-icd10.html}.
#' 
#' Centers for Disease Control and Prevention, IQVIA Xponent 2006–2019. U.S. Opioid Dispensing Rate Maps. Accessed at https://www.cdc.gov/drugoverdose/rxrate-maps/index.html.
"overdoses"
