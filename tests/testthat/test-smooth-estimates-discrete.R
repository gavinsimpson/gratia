# Regression tests for discrete random-effect interactions (#387).

test_that("smooth_estimates matches discrete prediction for diamonds", {
  m <- bam(price ~ s(color, clarity, bs = "re"),
    data = ggplot2::diamonds, discrete = TRUE
  )
  # This case has no explicit margin permutation, but still needs tensor order.
  expect_null(m$smooth[[1L]]$rind)
  sm <- smooth_estimates(m)
  pr <- predict(m, newdata = sm, type = "terms", se.fit = TRUE)
  expect_equal(unname(sm$.estimate), unname(pr$fit[, 1L]))
  expect_equal(sm$.se, unname(pr$se.fit[, 1L]))
})

test_that("discrete random effects preserve margin order and by variables", {
  dat <- expand.grid(
    a = factor(letters[1:3]), b = factor(LETTERS[1:4]),
    c = factor(1:2), replicate = 1:8
  )
  dat <- withr::with_seed(387, {
    dat$x <- runif(nrow(dat))
    dat$z <- rnorm(nrow(dat))
    dat$g <- factor(rep(c("u", "v"), length.out = nrow(dat)))
    dat$y <- as.numeric(dat$a) * as.numeric(dat$b) + rnorm(nrow(dat))
    dat
  })
  formulas <- list(
    y ~ s(a, b, bs = "re"),
    y ~ s(b, a, bs = "re"),
    y ~ s(a, b, c, bs = "re"),
    y ~ s(x, a, b, bs = "re"),
    y ~ s(a, b, bs = "re", by = z),
    y ~ s(a, b, bs = "re", by = g)
  )
  for (f in formulas) {
    for (discrete in c(FALSE, TRUE)) {
      m <- bam(f, data = dat, discrete = discrete)
      original_smooths <- m$smooth
      for (label in smooths(m)) {
        sm <- smooth_estimates(m, select = label, data = dat)
        # Factor-by smooths retain only rows for their own factor level.
        pr <- predict(m, newdata = sm, type = "terms", se.fit = TRUE)
        expect_equal(unname(sm$.estimate), unname(pr$fit[, label]))
        expect_equal(sm$.se, unname(pr$se.fit[, label]))
      }
      expect_identical(m$smooth, original_smooths)
    }
  }
})

test_that("discrete smooth evaluation only needs the selected predictors", {
  dat <- withr::with_seed(387, {
    dat <- expand.grid(a = factor(letters[1:3]), b = factor(LETTERS[1:4]),
      replicate = 1:20
    )
    dat$x <- runif(nrow(dat))
    dat$y <- as.numeric(dat$a) * as.numeric(dat$b) + dat$x +
      rnorm(nrow(dat))
    dat
  })
  m <- bam(y ~ s(x) + s(a, b, bs = "re"), data = dat, discrete = TRUE)
  sm <- smooth_estimates(m, select = "s(a,b)")
  expect_false("x" %in% names(sm))
  pr <- predict(m, newdata = transform(sm, x = 0),
    type = "terms", se.fit = TRUE
  )
  expect_equal(unname(sm$.estimate), unname(pr$fit[, "s(a,b)"]))
  expect_equal(sm$.se, unname(pr$se.fit[, "s(a,b)"]))
})
