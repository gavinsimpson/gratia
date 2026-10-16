sampling_fixed_fixture <- local({
  cache <- list()
  function(bam = FALSE, poisson = FALSE, missing = FALSE) {
    key <- paste(bam, poisson, missing)
    if (!is.null(cache[[key]])) return(cache[[key]])
    bnd <- list(data.frame(x = cos(seq(0, 2 * pi, length.out = 61)),
      y = sin(seq(0, 2 * pi, length.out = 61)),
      f = cos(seq(0, 2 * pi, length.out = 61))))
    d <- withr::with_seed(25, {
      d <- data.frame(x = runif(160, -1, 1), y = runif(160, -1, 1))
      d <- d[mgcv::in.out(bnd, as.matrix(d)), ]
      d$off <- .2 * d$x
      d$arg <- .1 * d$y
      d$w <- runif(nrow(d), .6, 1.5)
      eta <- d$x + d$y^2 + d$off + d$arg
      d$z <- if (poisson) rpois(nrow(d), exp(eta)) else
        eta + rnorm(nrow(d), sd = .1)
      if (missing) d$z[4] <- NA_real_
      d
    })
    knots <- expand.grid(x = seq(-.8, .8, length.out = 5),
      y = seq(-.8, .8, length.out = 5))
    knots <- knots[mgcv::in.out(bnd, as.matrix(knots)), ]
    f <- z ~ s(x, y, bs = "so", k = 12,
      xt = list(bnd = bnd, nmax = 40)) + offset(off)
    fitter <- if (bam) mgcv::bam else mgcv::gam
    m <- fitter(f, data = d, weights = w, offset = arg, knots = knots,
      method = "REML", na.action = na.exclude,
      family = if (poisson) stats::poisson() else stats::gaussian())
    cache[[key]] <<- list(model = m, data = d, bnd = bnd, knots = knots)
    cache[[key]]
  }
})

test_that("posterior targets retain fitting offsets and fixed smooth contributions", {
  for (bam in c(FALSE, TRUE)) for (poisson in c(FALSE, TRUE)) {
    fixture <- sampling_fixed_fixture(bam = bam, poisson = poisson, missing = TRUE)
    m <- fixture$model
    d <- fixture$data[rownames(m$model), ]
    X <- mgcv::predict.gam(m, newdata = d, type = "lpmatrix")
    expect_gt(max(abs(attr(X, "offset"))), .1)
    if (bam) {
      expect_error(sampling_family_target(m), "prescribed smooth contributions")
      expect_error(sampling_target(m, compiled = TRUE), "prescribed smooth contributions")
      expect_error(sampling_target(m, dense_limit = 0, block_size = 19L),
        "prescribed smooth contributions")
      if (!poisson) {
        # Independently verify the native Gaussian fit omits this boundary
        # from its likelihood, despite adding it in prediction.
        S <- sampling_target_penalty(m, "gratia")
        native_mean <- solve(crossprod(X, X * d$w) + S,
          crossprod(X, d$w * (m$y - m$offset)))
        expect_equal(unname(drop(native_mean)), unname(coef(m)), tolerance = 1e-7)
      }
      next
    }
    beta <- unname(coef(m)) + seq_along(coef(m)) / 100
    # Independent construction from raw fitting covariates. In installed mgcv,
    # gam includes the boundary in fitting offsets; bam omits it from fitting
    # even though native prediction adds it. Gaussian mode checks below verify
    # the stored fitting target instead of silently changing the fitted model.
    fixed <- if (bam) rep(0, nrow(X)) else rowSums(attr(X, "offset"))
    eta <- drop(X %*% beta) + fixed + d$off + d$arg
    family <- stats::family(m)
    expected <- -sum(family$dev.resids(m$y, family$linkinv(eta), d$w)) /
      (2 * m$sig2)
    callback <- sampling_family_target(m)
    expect_equal(callback$log_likelihood(beta), expected, tolerance = 1e-10)
    dense <- sampling_target(m, compiled = TRUE)
    reference <- sampling_target(m)
    blocks <- sampling_target(m, dense_limit = 0, block_size = 19L)
    difference <- function(target) sampling_target_eval(target, beta) -
      sampling_target_eval(target, unname(coef(m)))
    expect_equal(dense$offset, d$off + d$arg + fixed,
      tolerance = 1e-12)
    expect_equal(blocks$offset, dense$offset, tolerance = 1e-12)
    expect_null(blocks$X)
    expect_equal(difference(dense), difference(callback),
      tolerance = 1e-8)
    expect_equal(difference(reference), difference(callback),
      tolerance = 1e-8)
    expect_equal(difference(blocks), difference(callback),
      tolerance = 1e-8)
    # The target score independently matches central numerical differences.
    h <- 1e-5
    numerical <- vapply(seq_along(beta), function(j) {
      delta <- rep(0, length(beta)); delta[j] <- h
      (sampling_target_eval(callback, beta + delta) -
        sampling_target_eval(callback, beta - delta)) / (2 * h)
    }, numeric(1L))
    expect_equal(sampling_target_eval(dense, beta, gradient = TRUE)$gradient,
      numerical, tolerance = 1e-5)
  }
})

test_that("family target retains actually fitted general-family offsets", {
  withr::local_seed(92)
  d <- data.frame(x = runif(80), z = runif(80), off = runif(80),
    arg = runif(80), y = rnorm(80))
  m <- mgcv::gam(list(y ~ x + offset(off), ~ z), data = d,
    offset = arg, family = mgcv::gaulss())
  # mgcv can keep a call-level offset column while omitting that offset from
  # the fitted general-family likelihood. Use the actual fitting slots.
  target <- sampling_family_target(m)
  X <- mgcv::predict.gam(m, newdata = d, type = "lpmatrix")
  beta <- coef(m) + .01
  expected <- m$family$ll(m$y, X, beta, m$prior.weights,
    m$family, offset = m$offset, deriv = 0)$l / m$sig2
  expect_equal(target$log_likelihood(beta), expected, tolerance = 1e-10)
  offset <- sampling_fitting_offsets(model_used_rows(m), X)
  expect_equal(offset[[1L]], as.numeric(m$offset[[1L]]), tolerance = 1e-12)
  expect_equal(offset[[2L]], rep(0, nrow(d)))
})

test_that("ordinary and discrete bam targets evaluate original covariates", {
  withr::local_seed(19)
  d <- data.frame(x = runif(180), off = runif(180), arg = runif(180))
  d$y <- rpois(180, exp(sin(4 * d$x) + d$off + d$arg))
  for (discrete in c(FALSE, TRUE)) {
    m <- mgcv::bam(y ~ s(x, k = 6) + offset(off), data = d,
      offset = arg, family = poisson(), discrete = discrete)
    beta <- coef(m) + .03
    X <- mgcv::predict.gam(m, newdata = d, type = "lpmatrix")
    eta <- drop(X %*% beta) + d$off + d$arg
    likelihood <- -sum(poisson()$dev.resids(m$y, exp(eta), m$prior.weights)) / 2
    callback <- sampling_family_target(m)
    dense <- sampling_target(m, compiled = TRUE)
    blocks <- sampling_target(m, dense_limit = 0, block_size = 23L)
    difference <- function(target) sampling_target_eval(target, beta) -
      sampling_target_eval(target, unname(coef(m)))
    expect_equal(callback$log_likelihood(beta), likelihood, tolerance = 1e-10)
    expect_equal(dense$offset, d$off + d$arg, tolerance = 1e-12)
    expect_equal(blocks$offset, dense$offset, tolerance = 1e-12)
    expect_equal(difference(dense), difference(callback),
      tolerance = 1e-8)
    expect_equal(difference(blocks), difference(callback),
      tolerance = 1e-8)
  }
})

test_that("LSS evaluation draws include real prescribed smooth boundaries", {
  fixture <- sampling_fixed_fixture()
  d <- fixture$data
  bnd <- fixture$bnd
  m <- mgcv::gam(list(z ~ s(x, y, bs = "so", k = 12,
    xt = list(bnd = bnd, nmax = 40)) + offset(off), ~ offset(.1 * y)),
    data = d, knots = fixture$knots, family = mgcv::gaulss(), method = "REML")
  nd <- d[c(7, 1, 15, 3), ]
  nd$off <- nd$off + .3
  B <- rbind(coef(m), coef(m) + .01)
  eta <- predict(m, newdata = nd, type = "link")
  X <- predict(m, newdata = nd, type = "lpmatrix")
  expect_gt(max(abs(attr(X, "offset"))), .1)
  index <- attr(X, "lpi")
  expected <- withr::with_seed(82, {
    unlist(lapply(seq_len(nrow(B)), function(i) {
      lp <- eta
      for (k in seq_along(index)) {
        j <- index[[k]]
        lp[, k] <- lp[, k] + drop(X[, j, drop = FALSE] %*% (B[i, j] - coef(m)[j]))
      }
      rnorm(nrow(nd), lp[, 1], .01 + exp(lp[, 2]))
    }))
  })
  newdata <- nd; newdata$z <- NULL
  actual <- posterior_samples(m, data = newdata, method = "user", draws = B, seed = 82)
  expect_equal(actual$.response, expected, tolerance = 1e-10)
  actual <- fitted_samples(m, data = newdata, method = "user", draws = B[1, , drop = FALSE])
  expect_equal(actual$.fitted, as.numeric(eta[, 1]), tolerance = 1e-10)
  h <- 1e-5
  plus <- minus <- newdata
  plus$x <- plus$x + h / 2
  minus$x <- minus$x - h / 2
  expected <- (predict(m, plus, type = "response")[, 1] -
    predict(m, minus, type = "response")[, 1]) / h
  actual <- derivative_samples(m, data = newdata, focal = "x", eps = h, type = "central",
    method = "user", draws = B[1, , drop = FALSE])
  expect_equal(actual$.derivative, as.numeric(expected), tolerance = 1e-6)
})

test_that("grouped evaluation draws include real prescribed smooth boundaries", {
  fixture <- sampling_fixed_fixture()
  d <- fixture$data
  d$component <- rep(1:2, length.out = nrow(d))
  bnd <- fixture$bnd
  m <- mgcv::gam(cbind(z, component) ~ s(x, y, bs = "so", k = 12,
    xt = list(bnd = bnd, nmax = 40)) + offset(off), data = d,
    knots = fixture$knots, family = mgcv::gfam(list(gaussian(), gaussian())),
    method = "REML")
  nd <- d[c(7, 1, 15, 3, 8), ]; nd$off <- nd$off + .3
  B <- rbind(coef(m), coef(m) + .01)
  X <- mgcv::predict.gam(m, newdata = nd, type = "lpmatrix")
  expect_gt(max(abs(attr(X, "offset"))), .1)
  eta <- X %*% t(B) + rowSums(attr(X, "offset")) + nd$off
  scale <- exp(m$family$getTheta())
  expect_length(scale, 2L)
  expected <- withr::with_seed(28, {
    unlist(lapply(seq_len(nrow(B)), function(i) {
      value <- numeric(nrow(nd))
      for (k in 1:2) {
        j <- which(nd$component == k)
        value[j] <- rnorm(length(j), eta[j, i], sqrt(scale[k]))
      }
      value
    }))
  })
  nd$z <- NULL
  actual <- posterior_samples(m, data = nd, method = "user", draws = B, seed = 28)
  expect_equal(actual$.response, expected, tolerance = 1e-10)
  actual <- fitted_samples(m, data = nd, method = "user", draws = B)
  expect_equal(actual$.fitted, as.numeric(eta), tolerance = 1e-10)
})

test_that("Gaussian smoothing setup retains prescribed boundaries exactly once", {
  for (bam in c(FALSE, TRUE)) {
    fixture <- sampling_fixed_fixture(bam = bam)
    m <- fixture$model
    if (bam) {
      expect_error(sampling_smoothing_setup(m, block_size = 23L),
        "smoothing uncertainty prototype.*prescribed smooth contributions")
      next
    }
    setup <- sampling_smoothing_setup(m, block_size = 23L)
    conditional <- sampling_smoothing_conditional(setup, log(m$sp))
    expect_equal(conditional$mean, unname(coef(m)), tolerance = 1e-7)
    expect_equal(conditional$covariance, unname(vcov(m)), tolerance = 1e-6)
    X <- mgcv::predict.gam(m, newdata = fixture$data, type = "lpmatrix")
    expect_identical(sampling_fitting_offsets(model_used_rows(m), X)[[1L]], m$offset)
  }
})
