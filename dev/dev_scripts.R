

# ammend dependencies
attachment::att_amend_desc()

# document
devtools::document()

devtools::test()

devtools::build_manual()

# ascciicast 

library(asciicast)

src <- system.file("./")
cast <- asciicast::record("./dev/quick_tutorial_test.R")
svg <- tempfile(fileext = ".svg")
asciicast::write_svg(cast, "file.svg", window = TRUE)


asciicast::write_gif(cast, "file.gif")




# Load two packages
library(dplyr)
library(stringr)
# Count your lines of R code
list.files(path = "./R/", recursive = T, full.names = T) %>%
  str_subset("[.][R]$") %>%
  sapply(function(x) x %>% readLines() %>% length()) %>%
  sum()

