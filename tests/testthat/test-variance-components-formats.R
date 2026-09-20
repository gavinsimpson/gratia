test_that("variance_comp handles gam.vcomp return formats", {
  withr::local_seed(42)
  d <- data.frame(x = runif(200), z = runif(200))
  d$y <- sin(6 * d$x) + d$z^2 + rnorm(200, sd = 0.3)
  models <- list(
    gcv = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d),
    reml = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d, method = "REML"),
    ml = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d, method = "ML"),
    fixed = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d, sp = c(1, 2)),
    linked = gam(y ~ s(x, k = 8, id = 1) + s(z, k = 8, id = 1),
      data = d, method = "REML"
    )
  )
  for (m in models) {
    for (rescale in c(FALSE, TRUE)) {
      got <- variance_comp(m, rescale = rescale, coverage = 0.9)
      invisible(capture.output(ref <- gam.vcomp(m,
        rescale = rescale, conf.lev = 0.9
      )))
      if (is.list(ref) && !is.null(ref$vc)) ref <- ref$vc
      expect_s3_class(got, "variance_comp")
      if (is.null(ref)) {
        expect_equal(nrow(got), 0L)
      } else if (is.matrix(ref)) {
        expect_equal(got$.component, rownames(ref))
        expect_equal(got$.std_dev, unname(ref[, "std.dev"]))
        expect_equal(got$.lower_ci, unname(ref[, "lower"]))
        expect_equal(got$.upper_ci, unname(ref[, "upper"]))
      } else {
        expect_equal(got$.component, names(ref))
        expect_equal(got$.std_dev, as.numeric(ref))
        expect_true(all(is.na(got$.lower_ci)))
        expect_true(all(is.na(got$.upper_ci)))
      }
      expect_equal(got$.variance, got$.std_dev^2)
    }
  }
})
