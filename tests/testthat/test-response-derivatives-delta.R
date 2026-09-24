response_delta_fixture <- function(family = poisson()) {
  withr::local_seed(32)
  d <- data.frame(x = seq(-1, 1, length.out = 400))
  eta <- .8 + .45 * d$x
  d$y <- if (family$family == "poisson") rpois(nrow(d), exp(eta)) else
    if (family$family == "binomial") rbinom(nrow(d), 1, plogis(eta)) else
      eta + rnorm(nrow(d), sd = .3)
  mgcv::gam(y ~ x, data = d, family = family, method = "REML")
}

test_that("delta derivatives agree with analytic log and logit gradients", {
  nd <- data.frame(x = c(-.6, 0, .6))
  for (fam in list(poisson(), binomial())) {
    m <- response_delta_fixture(fam)
    b <- coef(m)
    eta <- as.numeric(predict(m, nd, type = "link"))
    mu <- fam$linkinv(eta)
    h1 <- fam$mu.eta(eta)
    h2 <- if (fam$link == "log") mu else h1 * (1 - 2 * mu)
    h3 <- if (fam$link == "log") mu else h1 * (1 - 6 * mu + 6 * mu^2)
    for (ord in 1:2) for (type in c("forward", "backward", "central")) {
      truth <- if (ord == 1) h1 * b[2] else h2 * b[2]^2
      J <- if (ord == 1) {
        cbind(h2 * b[2], h2 * b[2] * nd$x + h1)
      } else cbind(h3 * b[2]^2, h3 * b[2]^2 * nd$x + 2 * h2 * b[2])
      se <- sqrt(rowSums((J %*% vcov(m)) * J))
      out <- response_derivatives(m, data = nd, focal = "x", order = ord,
        type = type, uncertainty = "delta", level = .9)
      expect_s3_class(out, "response_derivatives")
      expect_equal(out$x, nd$x)
      expect_lt(max(abs(out$.derivative - truth)), 1e-4)
      expect_lt(max(abs(out$.se - se)), 1e-4)
      expect_equal(out$.lower_ci, out$.derivative - qnorm(.95) * out$.se)
      expect_equal(out$.upper_ci, out$.derivative + qnorm(.95) * out$.se)
    }
  }
})

test_that("identity and linear predictor derivatives use analytic covariance", {
  m <- response_delta_fixture(gaussian())
  for (ord in 1:2) for (type in c("forward", "backward", "central")) {
    a <- response_derivatives(m, focal = "x", n = 4, order = ord,
      type = type, uncertainty = "delta")
    b <- response_derivatives(m, focal = "x", n = 4, order = ord,
      type = type, scale = "linear_predictor", uncertainty = "delta")
    expect_equal(a, b)
    expect_equal(a$.derivative, rep(if (ord == 1) unname(coef(m)[2]) else 0, 4),
      tolerance = 1e-5)
    expect_equal(a$.se, rep(if (ord == 1) sqrt(vcov(m)[2, 2]) else 0, 4),
      tolerance = 1e-5)
  }
  m$Vc <- m$Vp * 4
  a <- response_derivatives(m, focal = "x", n = 4, uncertainty = "delta")
  b <- response_derivatives(m, focal = "x", n = 4, uncertainty = "delta",
    unconditional = TRUE)
  expect_equal(b$.se, 2 * a$.se)
  m$Ve <- m$Vp * 9
  c <- response_derivatives(m, focal = "x", n = 4, uncertainty = "delta",
    freq = TRUE)
  expect_equal(c$.se, 3 * a$.se)
  expect_equal(response_derivatives(structure(list(gam = m), class = "gamm"),
    focal = "x", n = 4, uncertainty = "delta"), a)
})

test_that("offsets and excluded terms enter the response gradient correctly", {
  withr::local_seed(13)
  d <- data.frame(x = seq(-1, 1, length.out = 300), z = runif(300))
  d$y <- rpois(300, exp(1 + .3 * d$x + d$z))
  m <- mgcv::gam(y ~ x + s(z, k = 5) + offset(.2 * x), data = d,
    family = poisson(), method = "REML")
  nd <- data.frame(x = c(-.4, .5), z = .6)
  out <- response_derivatives(m, focal = "x", data = nd, type = "central",
    uncertainty = "delta", exclude = "s(z)")
  mu <- as.numeric(predict(m, nd, type = "response", exclude = "s(z)"))
  slope <- coef(m)[2] + .2
  J <- matrix(0, nrow(nd), length(coef(m)))
  J[, 1] <- mu * slope
  J[, 2] <- mu * (1 + nd$x * slope)
  expect_equal(out$.derivative, mu * unname(slope), tolerance = 1e-7)
  expect_equal(out$.se, sqrt(rowSums((J %*% vcov(m)) * J)), tolerance = 1e-7)
})

test_that("simulation keeps medians and quantiles and reports posterior SD", {
  m <- response_delta_fixture()
  nd <- data.frame(x = c(-.2, .3))
  draws <- rbind(coef(m) - .1, coef(m), coef(m) + .2)
  # All supplied draws are used, regardless of the default n_sim.
  ds <- derivative_samples(m, focal = "x", data = nd, method = "user",
    draws = draws, type = "central")
  out <- response_derivatives(m, focal = "x", data = nd, method = "user",
    draws = draws, type = "central")
  expect_identical(attr(out, "uncertainty"), "simulation")
  for (i in seq_len(nrow(nd))) {
    x <- ds$.derivative[ds$.row == i]
    expect_equal(out$.derivative[i], median(x))
    expect_equal(out$.se[i], sd(x))
    expect_equal(unname(out$.lower_ci[i]), unname(quantile(x, .025)))
    expect_equal(unname(out$.upper_ci[i]), unname(quantile(x, .975)))
  }
  m$Vp <- m$Vp * .001
  delta <- response_derivatives(m, focal = "x", data = nd,
    type = "central", uncertainty = "delta")
  sim <- response_derivatives(m, focal = "x", data = nd,
    type = "central", n_sim = 5000, seed = 29)
  expect_lt(max(abs(sim$.se / delta$.se - 1)), .06)
})

test_that("delta validates inputs and does not use the RNG or sampling controls", {
  m <- response_delta_fixture()
  withr::local_seed(21)
  before <- .Random.seed
  out <- response_derivatives(m, focal = "x", n = 3, uncertainty = "delta",
    method = "unused", n_sim = 0, seed = 8, draws = matrix(NA), n_cores = 0)
  expect_identical(.Random.seed, before)
  expect_identical(attr(out, "uncertainty"), "delta")
  for (level in list(0, 1, NA_real_, c(.8, .9))) {
    expect_error(response_derivatives(m, focal = "x", uncertainty = "delta",
      level = level), "level")
  }
  expect_error(response_derivatives(m, focal = "x", uncertainty = "delta",
    eps = 0), "eps")
  expect_error(response_derivatives(m, focal = "x", uncertainty = "delta",
    order = 3), "Only 1st or 2nd")
  expect_error(response_derivatives(m, focal = "x", uncertainty = "delta",
    se.fit = TRUE), "se.fit")
  expect_error(response_derivatives(m, focal = "x", uncertainty = "delta",
    data = data.frame(x = c(0, NA))), "finite.*retain")
  class(m) <- c("scam", class(m))
  expect_error(response_derivatives(m, focal = "x", uncertainty = "delta"),
    "not supported")
  class(m) <- setdiff(class(m), "scam")
  class(m$family) <- c("general.family", class(m$family))
  expect_error(response_derivatives(m, focal = "x", uncertainty = "delta"),
    "not supported")
})

test_that("delta supports BAM smooths and preserves single-row predictions", {
  withr::local_seed(36)
  d <- data.frame(x = seq(0, 1, length.out = 200))
  d$y <- rpois(200, exp(.5 + sin(3 * d$x)))
  m <- mgcv::bam(y ~ s(x, k = 6), data = d, family = poisson(),
    method = "fREML", discrete = TRUE)
  nd <- data.frame(x = c(.2, .7))
  for (ord in 1:2) {
    out <- response_derivatives(m, focal = "x", data = nd, order = ord,
      type = "central", uncertainty = "delta")
    one <- response_derivatives(m, focal = "x", data = nd[1, , drop = FALSE],
      order = ord, type = "central", uncertainty = "delta")
    expected <- out[1, ]
    estimates <- c(".derivative", ".se", ".lower_ci", ".upper_ci")
    metadata <- setdiff(names(expected), estimates)
    expect_identical(attributes(one), attributes(expected))
    expect_identical(one[metadata], expected[metadata])
    # Second finite differences amplify prediction roundoff across batch sizes.
    expect_equal(one[estimates], expected[estimates], tolerance = 1e-7)
    # Removing the only varying term leaves a constant response mean.
    zero <- response_derivatives(m, focal = "x", data = nd, order = ord,
      type = "central", uncertainty = "delta", exclude = "s(x)")
    expect_equal(zero$.derivative, c(0, 0))
    expect_equal(zero$.se, c(0, 0))
  }
})
