# Cache fitted integration fixtures within one test run. The explicit parent
# retains their raw data for formula evaluation and model-data recovery.
make_secondary_models <- function(envir) {
  force(envir)
  models <- NULL
  function() {
    testthat::skip_on_cran()
    if (is.null(models)) {
      fixtures <- new.env(parent = envir)
      sys.source(testthat::test_path("fixtures", "secondary-models.R"),
        envir = fixtures)
      models <<- fixtures
    }
    models
  }
}
