test_that("rootogram transformations are independent of axis labels", {
  rg <- tibble::tibble(
    .bin = 0:3, .observed = c(4, 16, 9, 0),
    .fitted = c(9, 9, 4, 1)
  )
  class(rg) <- c("rootogram", class(rg))
  attr(rg, "distribution") <- "Poisson"
  for (type in c("hanging", "standing", "suspended")) {
    for (use_sqrt in c(FALSE, TRUE)) {
      default <- draw(rg, type = type, sqrt = use_sqrt)
      custom <- draw(rg, type = type, sqrt = use_sqrt, ylab = "Custom label")
      expect_equal(custom$data, default$data)
      expect_equal(custom$labels$y, "Custom label")
      expected_obs <- if (use_sqrt) sqrt(rg$.observed) else rg$.observed
      expected_fit <- if (use_sqrt) sqrt(rg$.fitted) else rg$.fitted
      expect_equal(custom$data$.observed, expected_obs)
      expect_equal(custom$data$.fitted, expected_fit)
      expect_equal(
        ggplot2::ggplot_build(custom)$data,
        ggplot2::ggplot_build(default)$data
      )
    }
  }
})
