#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# ammend dependencies
attachment::att_amend_desc()

# document
devtools::document()

devtools::test()

devtools::build_manual()

devtools::build_vignettes()

# install with vignettes:
devtools::install(build_vignettes = TRUE)