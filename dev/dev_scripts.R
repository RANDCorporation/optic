

# ammend dependencies
attachment::att_amend_desc()

# document
devtools::document()

devtools::build_manual()

# ascciicast 

library(asciicast)

src <- system.file("./")
cast <- asciicast::record("./dev/quick_tutorial_test.R")
svg <- tempfile(fileext = ".svg")
asciicast::write_svg(cast, "file.svg", window = TRUE)


asciicast::write_gif(cast, "file.gif")
