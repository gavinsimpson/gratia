test_that("plots retain the type supplied by a registered smooth evaluator", {
  model <- m_tiny_eg1
  original_class <- class(model$smooth[[1L]])
  extension_type <- "P spline (inc)"
  ns <- asNamespace("gratia")
  registerS3method("eval_smooth", "gratia_test_extension_smooth",
    function(smooth, model, ...) {
      class(smooth) <- original_class
      estimates <- eval_smooth(smooth, model, ...)
      estimates$.type <- extension_type
      estimates
    }, envir = ns)
  withr::defer(rm("eval_smooth.gratia_test_extension_smooth",
    envir = get(".__S3MethodsTable__.", envir = ns)))
  class(model$smooth[[1L]]) <- c("gratia_test_extension_smooth", "mgcv.smooth")

  reference <- smooth_estimates(m_tiny_eg1, select = 1L, n = 9L)
  estimates <- smooth_estimates(model, select = 1L, n = 9L)
  expect_identical(unique(estimates$.type), extension_type)
  expect_equal(estimates$.estimate, reference$.estimate)
  expect_equal(estimates$.se, reference$.se)

  assembled <- assemble(model, select = 1L, n = 9L)
  drawn <- draw(model, select = 1L, n = 9L, wrap = FALSE)
  for (panels in list(assembled, drawn)) {
    expect_length(panels, 1L)
    expect_identical(unique(panels[[1L]]$labels$caption),
      paste("Basis:", extension_type))
    expect_silent(ggplot2::ggplot_build(panels[[1L]]))
  }
})
