bam_fixed_offset_fixture <- function(poisson = FALSE, combined = TRUE) {
  bnd <- list(data.frame(x = cos(seq(0, 2 * pi, length.out = 61)),
    y = sin(seq(0, 2 * pi, length.out = 61)),
    f = cos(seq(0, 2 * pi, length.out = 61))))
  d <- withr::with_seed(25, {
    d <- data.frame(x = runif(160, -1, 1), y = runif(160, -1, 1))
    d <- d[mgcv::in.out(bnd, as.matrix(d)), ]
    d$off <- .2 * d$x
    d$off2 <- .1 * d$y
    eta <- d$x + d$y^2 + d$off2 + if (combined) d$off else 0
    d$z <- if (poisson) rpois(nrow(d), exp(eta)) else
      eta + rnorm(nrow(d), sd = .03)
    d
  })
  knots <- expand.grid(x = seq(-.8, .8, length.out = 5),
    y = seq(-.8, .8, length.out = 5))
  knots <- knots[mgcv::in.out(bnd, as.matrix(knots)), ]
  f <- if (combined) {
    z ~ s(x, y, bs = "so", k = 12, xt = list(bnd = bnd, nmax = 40)) +
      offset(off)
  } else z ~ s(x, y, bs = "so", k = 12, xt = list(bnd = bnd, nmax = 40))
  m <- mgcv::bam(f, data = d, offset = off2, knots = knots, method = "REML",
    family = if (poisson) stats::poisson() else stats::gaussian())
  list(model = m, data = d)
}

test_that("ordinary BAM fitting offsets retain fixed smooth boundaries", {
  for (poisson in c(FALSE, TRUE)) for (combined in c(FALSE, TRUE)) {
    fit <- bam_fixed_offset_fixture(poisson, combined)
    m <- fit$model
    d <- fit$data
    X <- predict(m, d, type = "lpmatrix")
    fixed <- rowSums(attr(X, "offset"))
    expect_true(any(abs(fixed) > .1))
    known <- d$off2 + if (combined) d$off else 0
    eta <- drop(X %*% coef(m)) + fixed + known
    B <- matrix(coef(m), nrow = 1)
    used <- model_used_rows(m)
    expect_equal(used$linear.predictors, eta, ignore_attr = TRUE,
      tolerance = 1e-9)
    expect_equal(model_used_rows(used)$linear.predictors, eta,
      ignore_attr = TRUE, tolerance = 1e-9)
    a <- fitted_samples(m, method = "user", draws = B,
      scale = "linear_predictor")
    expect_equal(a$.fitted, eta, ignore_attr = TRUE, tolerance = 1e-9)
    expect_equal(response_distribution(m)$mean, family(m)$linkinv(eta),
      ignore_attr = TRUE, tolerance = 1e-9)

    # Excluding the soap term removes its fixed boundary as well as its columns.
    a <- fitted_samples(m, method = "user", draws = B,
      scale = "linear_predictor", exclude = m$smooth[[1]]$label)
    expect_equal(a$.fitted, unname(coef(m)[1]) + known,
      ignore_attr = TRUE, tolerance = 1e-9)
    a <- fitted_samples(m, data = d, method = "user", draws = B,
      scale = "linear_predictor", exclude = m$smooth[[1]]$label)
    expect_equal(a$.fitted, rep(unname(coef(m)[1]), nrow(d)) +
      if (combined) d$off else 0, ignore_attr = TRUE, tolerance = 1e-9)
  }
})


test_that("BAM diagnostic offset restoration uses block prediction", {
  d <- withr::with_seed(44, {
    d <- data.frame(x = seq(0, 1, length.out = 80), off = runif(80, -.5, .5))
    d$y <- rpois(80, exp(.3 + sin(d$x) + d$off))
    d
  })
  m <- mgcv::bam(y ~ s(x, k = 5), data = d, offset = off, family = poisson())
  eta <- as.numeric(predict(m, d, type = "link")) + d$off
  expect_gt(max(abs(d$off)), .1)

  # Diagnostic means need only one value per row. Requiring a full n by p
  # matrix here would make this path impractical for large BAM fits.
  native_prediction <- predict_model
  local_mocked_bindings(predict_model = function(object, newdata, ...,
                                                envir = NULL) {
    if (identical(list(...)$type, "lpmatrix")) {
      stop("Diagnostic offset restoration requested a full design matrix.")
    }
    if (missing(newdata)) return(native_prediction(object, ..., envir = envir))
    native_prediction(object, newdata = newdata, ..., envir = envir)
  })
  expect_equal(response_distribution(m)$mean, exp(eta), ignore_attr = TRUE,
    tolerance = 1e-9)
})
