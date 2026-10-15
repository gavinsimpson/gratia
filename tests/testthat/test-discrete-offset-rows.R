test_that("discrete fitted intervals restore formula-offset missing rows once", {
  d <- withr::with_seed(26, {
    d <- data.frame(x = seq(.1, 2, length.out = 90), off = runif(90))
    d$y <- rpois(90, exp(.2 + sin(d$x) + d$off))
    d
  })
  d$x[20] <- NA; d$off[35] <- NA; d$y[50] <- NA
  for (action in c("na.omit", "na.exclude")) {
    m <- mgcv::bam(y ~ s(x, k = 5) + offset(off), data = d,
      subset = x > .2, family = poisson(), discrete = TRUE, na.action = action)
    rows <- as.integer(rownames(m$model))
    X <- predict(m, d[rows, ], type = "lpmatrix")
    eta <- drop(X %*% coef(m)) + d$off[rows]
    se <- sqrt(rowSums((X %*% m$Vp) * X))
    restore <- function(x) unname(napredict(m$na.action, x))
    for (interval in c("confidence", "simultaneous")) {
      a <- fitted_values(m, scale = "link", interval = interval, n_sim = 100, seed = 14)
      expect_equal(a$.fitted, restore(eta), tolerance = 1e-8)
      expect_equal(a$.se, restore(se), tolerance = 1e-8)
      valid <- !is.na(a$.fitted)
      expect_true(all(a$.lower_ci[valid] <= a$.fitted[valid]))
      expect_true(all(a$.upper_ci[valid] >= a$.fitted[valid]))
      expect_identical(is.na(a$.lower_ci), !valid)
    }
    # Missing responses in explicit prediction rows do not prevent prediction.
    nd <- d[c(50, 40, 35, 20), ]; nd$off <- nd$off + 1
    valid <- complete.cases(nd[c("x", "off")])
    ref <- rep(NA_real_, nrow(nd))
    ref[valid] <- drop(predict(m, nd[valid, ], type = "lpmatrix") %*% coef(m)) + nd$off[valid]
    a <- fitted_values(m, data = nd, scale = "link")
    expect_equal(a$.fitted, ref, tolerance = 1e-8)
  }
})

test_that("discrete prediction accepts stored frames and explicit tibbles", {
  d <- withr::with_seed(27, {
    d <- data.frame(x = seq(.1, 2, length.out = 90), off = runif(90))
    d$y <- rpois(90, exp(.2 + sin(d$x) + d$off)); d
  })
  for (has_offset in c(FALSE, TRUE)) {
    m <- if (has_offset) mgcv::bam(y ~ s(x, k = 5), data = d, offset = off,
      family = poisson(), discrete = TRUE) else
      mgcv::bam(y ~ s(x, k = 5), data = d, family = poisson(), discrete = TRUE)
    ref <- as.numeric(predict(m, newdata = m$model, type = "link"))
    # fitted_values follows explicit prediction semantics for argument offsets.
    a <- fitted_values(m, scale = "link")
    expect_equal(a$.fitted, ref, tolerance = 1e-8)
    expect_equal(add_fitted(NULL, m, type = "link")$.fitted, ref, tolerance = 1e-8)
    expect_equal(as.numeric(predict_model(m, newdata = tibble::as_tibble(m$model))), ref)
    nd <- tibble::as_tibble(d[c(80, 3, 45), ])
    a <- fitted_values(m, data = nd, scale = "link")
    expect_equal(a$.fitted, as.numeric(predict(m, as.data.frame(nd))), tolerance = 1e-8)
    expect_equal(add_fitted(nd, m, type = "link")$.fitted,
      as.numeric(predict(m, as.data.frame(nd))), tolerance = 1e-8)
  }
})
