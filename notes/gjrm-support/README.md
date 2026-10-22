# GJRM and grouped-family examples

From the package root, regenerate the report and plots with:

```sh
Rscript --vanilla notes/gjrm-support/render-examples.R
```

This loads the source package, fits seven joint GJRM models and a grouped-family
GAM, and writes an HTML report, executable R examples, fitted model objects and
PNG figures in this directory. These generated files are ignored by Git.

The report checks all 22 GJRM smooths against basis matrices and the final
joint coefficients and covariance matrices. The CSV files record the numerical
comparisons. Warnings in the numerical checks stop rendering.

After rendering, run the report checks from the package root:

```r
options(device = function(...) grDevices::pdf(file = NULL))
pkgload::load_all(".", quiet = TRUE)
library("testthat")
testthat::test_file("tests/integration/test-gjrm-example-checks.R",
  env = globalenv(), stop_on_failure = TRUE, stop_on_warning = TRUE)
```

The report and its integration tests are excluded from the source package.
The installed package test suite covers the model adapters, diagnostics,
response selection and shared plotting methods independently.
