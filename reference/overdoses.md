# OPTIC Overdoses example data.

An example dataset for performing simulations using the OPTIC library,
consisting of state-year overdose data from the US Bureau of Labor
Statistics, the Centers from Disease Control and Prevention, and IQVIA
Xponent.

## Usage

``` r
overdoses
```

## Format

A data frame with 969 rows and 7 variables:

- state:

  US state

- year:

  Year

- population:

  Population estimate (Centers for Disease Control and Prevention,
  National Center for Health Statistics)

- unemploymentrate:

  Average annual unemployment rate (US Bureau of Labor Statistics)

- opioid_rx:

  Estimated number of annual opioid prescriptions dispensed per 100
  residents (Centers for Disease Control and Prevention, IQVIA)

- deaths:

  Annual number of drug-induced deaths (all drug overdose) (Centers for
  Disease Control and Prevention, National Center for Health Statistics)

- crude.rate:

  Crude rate of drug-induced deaths (all drug overdose) per 100,000
  residents (Centers for Disease Control and Prevention, National Center
  for Health Statistics)

## Source

US Bureau of Labor Statistics. Local Area Unemployment Statistics April
2019 release. Accessed at https://www.bls.gov/lau/.

Centers for Disease Control and Prevention, National Center for Health
Statistics. Multiple Cause of Death 1999-2019 on CDC WONDER Online
Database, released in 2020. Data are from the Multiple Cause of Death
Files, 1999-2019, as compiled from data provided by the 57 vital
statistics jurisdictions through the Vital Statistics Cooperative
Program. Accessed at
[http://wonder.cdc.gov/mcd-icd10.html](http://wonder.cdc.gov/mcd-icd10.md).

Centers for Disease Control and Prevention, IQVIA Xponent 2006–2019.
U.S. Opioid Dispensing Rate Maps. Accessed at
https://www.cdc.gov/drugoverdose/rxrate-maps/index.html.
