# Compare public entry points with the underlying GAM computation. Fixtures
# are shared through setup.R; this helper supplies only assertion machinery.
expect_gamm4_equivalent <- function(model, operation, ...) {
  fun <- get(operation, envir = asNamespace("gratia"))
  actual <- fun(model, ...)
  component <- model$gam
  component$residuals <- stats::residuals(model$mer, type = "working")
  expected <- fun(component, ...)
  # Captured calls/model expressions intentionally name different inputs.
  for (nm in c(".call", "model_name", "model")) {
    attr(actual, nm) <- attr(expected, nm) <- NULL
  }
  expect_equal(actual, expected, info = operation)
  invisible(actual)
}
