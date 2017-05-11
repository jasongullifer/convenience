library(roxygen2)
library(devtools)

document()
devtools::build_vignettes()

devtools::check(document = FALSE)

