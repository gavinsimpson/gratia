test_that("quantile residual diagnostics respect missing-row conventions", {
  withr::local_seed(42)
  d <- data.frame(x = runif(100), y = rpois(100, 3))
  d$x[7] <- NA
  d$y[19] <- NA
  keep <- complete.cases(d)
  for (fit in list(
    function(action) {
      gam(y ~ s(x, k = 6),
        family = poisson(), data = d,
        na.action = action, method = "REML"
      )
    },
    function(action) glm(y ~ x, family = poisson(), data = d, na.action = action)
  )) {
    excluded <- fit(na.exclude)
    omitted <- fit(na.omit)
    for (type in c("pit", "quantile")) {
      a <- quantile_residuals(excluded, type = type, seed = 123)
      b <- quantile_residuals(omitted, type = type, seed = 123)
      expect_length(a, nrow(d))
      expect_length(b, sum(keep))
      expect_identical(which(is.na(a)), which(!keep))
      expect_equal(unname(a[keep]), unname(b))
      expect_true(all(is.finite(b)))
      p <- residuals_linpred_plot(excluded, type = type, seed = 123)$data
      q <- residuals_linpred_plot(omitted, type = type, seed = 123)$data
      expect_equal(unname(p$residuals), unname(a))
      expect_equal(unname(p$eta[keep]), unname(q$eta))
      expect_identical(which(is.na(p$eta)), which(!keep))
      for (bins in c("sturges", "scott", "fd")) {
        expect_s3_class(residuals_hist_plot(excluded,
          type = type,
          n_bins = bins, seed = 123
        ), "ggplot")
      }
    }
  }
})
