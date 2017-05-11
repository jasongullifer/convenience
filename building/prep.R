library(roxygen2)
library(devtools)
devtools::check(document = FALSE)

document()
devtools::build_vignettes()


