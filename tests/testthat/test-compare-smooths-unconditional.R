test_that("compare smooths unconditional works", {
  withr::local_seed(42)
  d <- data.frame(x = runif(200), z = runif(200))
  d$y <- sin(6 * d$x) + d$z^2 + rnorm(200, sd = 0.3)
  m1 <- gam(y ~ s(x, k = 8) + s(z, k = 8), data = d, method = "REML")
  m2 <- gam(y ~ s(x, k = 6) + s(z, k = 6), data = d, method = "REML")
  models <- list(m1 = m1, m2 = m2)
  nd <- data.frame(x = seq(0.1, 0.9, length.out = 20), z = 0.5)
  for (m in models) {
    expect_false(is.null(m$Vc))
    expect_gt(max(abs(m$Vp - m$Vc)), 1e-8)
  }
  for (u in c(FALSE, TRUE)) {
    got <- compare_smooths(m1, m2, data = nd, unconditional = u)
    for (i in seq_len(nrow(got))) {
      ref <- smooth_estimates(models[[got$.model[[i]]]],
        select = got$.smooth[[i]], data = nd,
        unconditional = u
      )
      expect_equal(got$data[[i]]$.se, ref$.se, tolerance = 1e-10)
    }
  }
  default <- compare_smooths(m1, m2, data = nd)
  conditional <- compare_smooths(m1, m2, data = nd, unconditional = FALSE)
  corrected <- compare_smooths(m1, m2, data = nd, unconditional = TRUE)
  expect_equal(default, conditional)
  for (i in seq_len(nrow(corrected))) {
    expect_gt(
      max(abs(corrected$data[[i]]$.se - conditional$data[[i]]$.se)),
      1e-8
    )
  }
})
