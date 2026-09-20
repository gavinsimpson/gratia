test_that("overview reports coefficient-level parametric degrees of freedom", {
  withr::local_seed(42)
  for (nlevels in 2:4) {
    d <- data.frame(
      f = factor(rep(seq_len(nlevels), each = 50)),
      x = runif(50 * nlevels), z = rnorm(50 * nlevels)
    )
    d$y <- as.integer(d$f) + sin(6 * d$x) + d$z + rnorm(nrow(d))
    for (intercept in c(TRUE, FALSE)) {
      form <- if (intercept) {
        y ~ f + z + s(x, k = 6)
      } else {
        y ~ 0 + f + z + s(x, k = 6)
      }
      m <- gam(form, data = d, method = "REML")
      got <- overview(m)
      para <- got[got$type == "parametric", ]
      ref <- summary(m)$p.table
      expect_equal(
        para$term,
        sub("^\\(Intercept\\)$", "Intercept", rownames(ref))
      )
      expect_equal(para$statistic, unname(ref[, 3]))
      expect_equal(
        para$p.value,
        format.pval(unname(ref[, 4]), eps = 0.001, digits = 3)
      )
      expect_equal(para$edf, rep(1, nrow(ref)))
      expect_equal(para$ref.edf, rep(1, nrow(ref)))
      smooth_only <- overview(m, parametric = FALSE)
      expect_equal(got[got$type != "parametric", ], smooth_only)
    }
  }
})
