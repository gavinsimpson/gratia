# Run from the root of the gratia checkout.
options(device = function(...) grDevices::pdf(file = NULL))
stopifnot(file.exists("DESCRIPTION"), dir.exists("notes/gjrm-support"))
pkgload::load_all(".", quiet = TRUE)
source_file <- normalizePath("notes/gjrm-support/gjrm-examples.Rmd")
knitr::purl(source_file,
  output = "notes/gjrm-support/gjrm-examples.R", documentation = 1L, quiet = TRUE)
rmarkdown::render(source_file, quiet = TRUE, envir = globalenv())
