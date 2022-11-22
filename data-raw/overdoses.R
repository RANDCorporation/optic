## code to prepare `DATASET` dataset goes here


load("./data-raw/overdoses.rda")
overdoses <- example_data
usethis::use_data(overdoses, overwrite = TRUE)
