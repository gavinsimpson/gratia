fitting_offset_data <- function() {
  withr::with_seed(101, {
    d <- data.frame(x = seq(.2, 2, length.out = 96), z = runif(96),
      off = runif(96, -.8, 1.2), off2 = runif(96, -.2, .3),
      g = factor(rep(letters[1:8], 12)))
    d$y <- rpois(nrow(d), exp(.3 + .6 * d$x + d$off + d$off2))
    d
  })
}

test_that("fitting-argument offsets survive ordinary bam storage and row restoration", {
  for (action in c("na.omit", "na.exclude")) for (combined in c(FALSE, TRUE)) {
    d <- fitting_offset_data()
    d$y[12] <- NA; d$off[20] <- NA
    f <- if (combined) y ~ s(x, k = 5) + offset(off2) else y ~ s(x, k = 5)
    m <- mgcv::bam(f, data = d, offset = off, subset = x > .3,
      family = poisson(), na.action = action)
    used_rows <- as.integer(rownames(m$model))
    X <- predict(m, d[used_rows, ], type = "lpmatrix")
    known <- d$off[used_rows] + if (combined) d$off2[used_rows] else 0
    eta <- drop(X %*% coef(m)) + known
    B <- matrix(coef(m), nrow = 1)
    restore <- function(x) unname(napredict(m$na.action, x))
    a <- fitted_samples(m, method = "user", draws = B, scale = "linear_predictor")
    expect_equal(a$.fitted, restore(eta), tolerance = 1e-9)
    a <- fitted_samples(m, method = "user", draws = B, scale = "linear_predictor",
      exclude = "s(x)")
    expect_equal(a$.fitted, restore(unname(coef(m)[1]) + known))
    expect_equal(response_distribution(m)$mean, exp(eta), ignore_attr = TRUE)
    expected <- withr::with_seed(13, rpois(length(eta), exp(eta)))
    a <- posterior_samples(m, n_draws = 1, method = "user", draws = B, seed = 13)
    expect_equal(a$.response, restore(expected))
    a <- simulate(m, nsim = 1, seed = 13)
    expect_equal(a[[1]], restore(expected))
    nd <- d[c(80, 3, 45), ]; nd$off <- nd$off + 2
    Xn <- predict(m, nd, type = "lpmatrix")
    ref <- drop(Xn %*% coef(m)) + if (combined) nd$off2 else 0
    a <- fitted_samples(m, data = nd, method = "user", draws = B, scale = "linear_predictor")
    expect_equal(a$.fitted, unname(ref))
    # The native object's slots are not repaired in place.
    before <- m$linear.predictors
    invisible(model_used_rows(m))
    expect_identical(m$linear.predictors, before)
  }
})

test_that("gamm4 GAM samples recover argument offsets without group effects", {
  skip_if_not_installed("gamm4")
  d <- fitting_offset_data()
  d$y <- withr::with_seed(102, rpois(nrow(d), exp(.3 + .6 * d$x + d$off +
    rep(c(-.6, .6), length.out = nrow(d)))))
  for (combined in c(FALSE, TRUE)) {
    f <- if (combined) y ~ s(x, k = 5) + offset(off2) else y ~ s(x, k = 5)
    m <- gamm4::gamm4(f, data = d, random = ~(1 | g), offset = d$off,
      family = poisson())
    X <- predict(m$gam, d, type = "lpmatrix")
    known <- d$off + if (combined) d$off2 else 0
    eta <- drop(X %*% coef(m$gam)) + known
    B <- matrix(coef(m$gam), nrow = 1)
    a <- fitted_samples(m, method = "user", draws = B, scale = "linear_predictor")
    expect_equal(a$.fitted, unname(eta), tolerance = 1e-9)
    expect_equal(response_distribution(m, conditioning = "gam_component")$mean,
      exp(eta), ignore_attr = TRUE)
    nd <- d[c(80, 3, 45), ]; nd$off <- nd$off + 2
    a <- fitted_samples(m, data = nd, method = "user", draws = B, scale = "linear_predictor")
    ref <- drop(predict(m$gam, nd, type = "lpmatrix") %*% coef(m$gam)) +
      if (combined) nd$off2 else 0
    expect_equal(a$.fitted, unname(ref))
  }
})
