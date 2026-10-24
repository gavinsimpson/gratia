test_that("new random intercepts share groups and have the fitted variance", {
  m <- random_effect_fixture()
  nd <- data.frame(g = c("new", "new", "other"))
  result <- random_effect_samples(m, data = nd, n_draws = 12000, seed = 8)
  z <- matrix(result$.value, nrow = 3)
  variance <- unname((m$reml.scale %||% m$sig2) / tail(m$full.sp, 1))
  expect_equal(z[1, ], z[2, ])
  expect_lt(abs(mean(z[1, ])), .03 * sqrt(variance))
  expect_lt(abs(stats::var(z[1, ]) - variance), .04 * variance)
  expect_lt(abs(stats::cor(z[1, ], z[3, ])), .04)
  expect_equal(result$g[1:3], nd$g)
  expect_named(attr(result, "random_effect_info")$coefficient_covariance, "s(g)")
})

test_that("random slopes and crossed factor groups use independent coefficients", {
  m <- random_effect_fixture(stats::gaussian())
  d <- attr(m, "fixture_data")
  m <- mgcv::gam(y ~ s(x, k = 5) + s(x, g, h, bs = "re"), data = d,
    method = "REML")
  nd <- data.frame(x = c(1, 2, 1), g = c("new", "new", "new"),
    h = c("one", "one", "two"))
  result <- random_effect_samples(m, data = nd, n_draws = 2000, seed = 91)
  z <- matrix(result$.value, nrow = 3)
  expect_equal(z[2, ], 2 * z[1, ])
  expect_lt(abs(stats::cor(z[1, ], z[3, ])), .08)
})

test_that("fs draws retain the complete covariance of a random curve", {
  m <- random_effect_fixture(stats::gaussian(), fs = TRUE)
  sm <- m$smooth[[1]]
  nd <- data.frame(x = c(.1, .5, .9, .1), g = c("new", "new", "new", "other"))
  result <- random_effect_samples(m, data = nd, n_draws = 16000, seed = 8)
  z <- matrix(result$.value, nrow = 4)
  k <- sm$base$bs.dim
  lambda <- m$full.sp[1:3]
  Q <- Reduce(`+`, Map(function(S, a) a * S[1:k, 1:k], sm$S, lambda))
  covariance <- (m$reml.scale %||% m$sig2) * solve(Q)
  known <- nd
  known$g <- factor(rep(sm$flev[1], 4), levels = sm$flev)
  B <- mgcv::PredictMat(sm, known)[, 1:k]
  expected <- B %*% covariance %*% t(B)
  expect_equal(stats::cov(t(z[1:3, ])), expected[1:3, 1:3],
    tolerance = .04 * max(expected))
  expect_lt(abs(stats::cor(z[1, ], z[4, ])), .04)
})

test_that("integrated means agree with analytic identity, log and probit results", {
  for (family in list(stats::gaussian(), stats::poisson(), stats::binomial("probit"))) {
    m <- random_effect_fixture(family)
    nd <- data.frame(x = c(.1, .4, .8))
    result <- fitted_values(m, data = nd, marginalise = TRUE)
    eta <- as.numeric(predict(m, transform(nd, g = m$model$g[1]),
      exclude = "s(g)", type = "link"))
    v <- unname((m$reml.scale %||% m$sig2) / tail(m$full.sp, 1))
    expected <- switch(family$link, identity = eta, log = exp(eta + v / 2),
      probit = stats::pnorm(eta / sqrt(1 + v)))
    expect_equal(result$.fitted, expected, tolerance = 1e-6)
    X <- predict(m, transform(nd, g = m$model$g[1]), exclude = "s(g)", type = "lpmatrix")
    derivative <- switch(family$link, identity = rep(1, length(eta)),
      log = expected, probit = stats::dnorm(eta / sqrt(1 + v)) / sqrt(1 + v))
    J <- X * derivative
    expect_equal(result$.se, unname(sqrt(rowSums((J %*% vcov(m)) * J))), tolerance = 1e-6)
    expect_equal(attr(result, "prediction_info")$se_scale, "response")
  }
})

test_that("both integration methods agree with an independent logistic integral", {
  m <- random_effect_fixture(stats::binomial())
  nd <- data.frame(x = c(.2, .7))
  context <- marginal_context(m, nd, TRUE)
  eta <- drop(context$X %*% coef(m)) + drop(context$offsets)
  expected <- vapply(eta, function(e) stats::integrate(function(z)
    stats::plogis(e + sqrt(context$variance[1]) * z) * stats::dnorm(z),
    -Inf, Inf, rel.tol = 1e-10)$value, numeric(1))
  q <- fitted_values(m, data = nd, marginalise = TRUE)
  mc <- fitted_values(m, data = nd, marginalise = TRUE,
    integration = "monte_carlo", n_integrate = 30000, seed = 2)
  expect_equal(q$.fitted, unname(expected), tolerance = 1e-6)
  error <- attr(mc, "integration_info")$accuracy$error
  expect_true(all(abs(mc$.fitted - expected) < 4 * error))
  expect_true(all(error > 0))
  expect_error(fitted_values(m, data = nd, marginalise = TRUE, n_quad = 2),
    "quadrature did not converge")
})

test_that("integrated fitted draws contain no new-group variation", {
  m <- random_effect_fixture()
  draws <- matrix(rep(coef(m), each = 3), nrow = 3)
  nd <- data.frame(x = c(.2, .7))
  for (method in c("quadrature", "monte_carlo")) {
    fs <- fitted_samples(m, data = nd, marginalise = TRUE,
      method = "user", draws = draws, integration = method, seed = 2)
    z <- matrix(fs$.fitted, nrow = 2)
    expect_equal(z[, 1], z[, 2])
    expect_equal(z[, 1], z[, 3])
    if (method == "quadrature") expect_equal(z[, 1],
      fitted_values(m, data = nd, marginalise = TRUE)$.fitted)
  }
})

test_that("response draws retain group dependence and existing uncertainty choices", {
  m <- random_effect_fixture(stats::gaussian())
  # Removing observation noise makes the new-group relationship directly visible.
  m$sig2 <- m$reml.scale <- 1e-8
  m$full.sp[length(m$full.sp)] <- 1e-8
  nd <- data.frame(x = rep(.4, 3), g = c("new", "new", "other"))
  ps <- predicted_samples(m, data = nd, marginalise = TRUE, n_draws = 3000, seed = 3)
  z <- matrix(ps$.response, nrow = 3)
  expect_gt(stats::cor(z[1, ], z[2, ]), .99)
  expect_lt(abs(stats::cor(z[1, ], z[3, ])), .05)
  draws <- matrix(rep(coef(m), each = 3000), nrow = 3000)
  post <- posterior_samples(m, data = nd, marginalise = TRUE,
    method = "user", draws = draws, seed = 3)
  expect_equal(post$.response, ps$.response)
  expect_false(is.null(attr(post, "random_effect_info")))
})

test_that("partial selection, offsets, NA restoration and BAM remain supported", {
  m <- random_effect_fixture()
  d <- attr(m, "fixture_data")
  d$off <- seq(.1, .4, length.out = nrow(d))
  d$x[5] <- NA_real_
  m <- mgcv::gam(y ~ s(x, k = 5) + s(g, bs = "re") + s(h, bs = "re") + offset(off),
    data = d, family = poisson(), method = "REML", na.action = stats::na.exclude)
  f <- fitted_values(m, marginalise = "s(g)")
  expect_equal(nrow(f), nrow(d))
  expect_true(is.na(f$.fitted[5]))
  nd <- d[1:3, ]
  nd$g <- NULL
  expect_equal(nrow(fitted_values(m, data = nd, marginalise = "s(g)")), 3L)
  nd$x[2] <- NA_real_
  fs <- fitted_samples(m, data = nd, n_draws = 2, seed = 2, marginalise = "s(g)")
  expect_true(all(is.na(fs$.fitted[fs$.row == 2])))
  omitted <- fitted_values(m, data = nd, marginalise = "s(g)", na.action = stats::na.omit)
  expect_equal(omitted$.row, c(1L, 3L))
  for (fs in c(FALSE, TRUE)) {
    b <- random_effect_fixture(fs = fs, bam = TRUE, discrete = TRUE)
    nd <- data.frame(x = c(.2, .7))
    fv <- fitted_values(b, data = nd, marginalise = TRUE)
    expect_true(all(is.finite(fv$.fitted)))
    expect_equal(fv$.fitted, unname(exp(drop(predict(b,
      transform(nd, g = b$model$g[1]), exclude = smooths(b)[length(b$smooth)],
      type = "link")) + attr(fv, "integration_info")$variance / 2)), tolerance = 1e-6)
  }
})

test_that("fixed and shared smoothing parameters retain their penalty mapping", {
  m <- random_effect_fixture(stats::gaussian())
  d <- attr(m, "fixture_data")
  m <- mgcv::gam(y ~ s(x, k = 5) + s(g, bs = "re"), data = d,
    sp = c(-1, 2), method = "REML")
  f <- fitted_values(m, data = data.frame(x = .5), marginalise = TRUE)
  expect_equal(unname(attr(f, "integration_info")$variance), m$reml.scale / 2)
  m <- mgcv::gam(y ~ s(x, by = h, k = 4, id = 1) + s(g, bs = "re"),
    data = d, method = "REML")
  setup <- random_effect_setup(m)
  expect_gt(length(m$full.sp), length(m$sp))
  expect_equal(setup$terms[[1]]$lambda, unname(tail(m$full.sp, 1)))
  expect_true(all(is.finite(fitted_values(m, data = d[1:3, ],
    marginalise = TRUE)$.fitted)))
})

test_that("new interfaces preserve RNG state and unchanged defaults", {
  m <- random_effect_fixture()
  nd <- attr(m, "fixture_data")[1:3, ]
  calls <- list(function() random_effect_samples(m, data = nd, n_draws = 2, seed = 2),
    function() fitted_values(m, data = nd, marginalise = TRUE,
      integration = "monte_carlo", seed = 2),
    function() fitted_samples(m, data = nd, marginalise = TRUE, n_draws = 2, seed = 2),
    function() predicted_samples(m, data = nd, marginalise = TRUE, n_draws = 2, seed = 2),
    function() posterior_samples(m, data = nd, marginalise = TRUE, n_draws = 2, seed = 2))
  for (f in calls) {
    set.seed(89)
    before <- .Random.seed
    a <- f()
    expect_identical(.Random.seed, before)
    expect_identical(a, f())
    rm(".Random.seed", envir = .GlobalEnv)
    f()
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
  expect_equal(fitted_values(m, data = nd)$.fitted, as.numeric(predict(m, nd, type = "response")))
  expect_equal(add_fitted(nd, m, marginalise = TRUE)$.fitted,
    fitted_values(m, data = nd, marginalise = TRUE)$.fitted)
  expect_equal(add_fitted_samples(nd, m, marginalise = TRUE, n_draws = 2, seed = 3)$.fitted,
    fitted_samples(m, data = nd, marginalise = TRUE, n_draws = 2, seed = 3)$.fitted[c(1, 4, 2, 5, 3, 6)])
})

test_that("unsupported requests are rejected explicitly", {
  m <- random_effect_fixture()
  expect_error(fitted_values(m, marginalise = TRUE, scale = "link"), "response-scale")
  expect_error(fitted_samples(m, marginalise = TRUE, scale = "link", n_draws = 2), "response-scale")
  expect_error(fitted_values(m, marginalise = TRUE, exclude = "s(g)"), "both")
  expect_error(fitted_values(m, marginalise = TRUE, terms = "s(x)"), "'terms'")
  expect_error(fitted_values(m, marginalise = TRUE, interval = "simultaneous"), "pointwise")
  expect_error(fitted_values(m, marginalise = TRUE, interval_method = "bootstrap"), "pointwise")
  expect_error(random_effect_samples(m, select = "s(x)", n_draws = 2), "exact labels")
  expect_error(predicted_samples(m, data = data.frame(x = .5), marginalise = TRUE, n_draws = 2), "grouping")
  expect_error(fitted_moments(m, marginalise = TRUE), "not yet supported")
  expect_error(conditional_differences(m, by = "g", condition = "x", marginalise = TRUE), "not yet supported")
  expect_error(derivative_samples(m, focal = "x", n_draws = 2, marginalise = TRUE), "not supported")
  sm <- m$smooth[[2]]
  m$smooth[[2]]$xt <- list(S = sm$S)
  expect_error(random_effect_samples(m, n_draws = 2), "Custom")
  m <- random_effect_fixture(stats::gaussian())
  m$family <- stats::Gamma("inverse")
  expect_error(fitted_values(m, marginalise = TRUE), "unbounded Gaussian")
})

test_that("all-missing predictions, response weights and covariance metadata are retained", {
  m <- random_effect_fixture()
  nd <- data.frame(x = c(NA_real_, NA_real_), g = c("new", "new"))
  set.seed(5)
  before <- .Random.seed
  simulated <- predicted_samples(m, data = nd, marginalise = TRUE, n_draws = 2)
  expect_identical(.Random.seed, before)
  expect_true(all(is.na(simulated$.response)))
  expect_equal(simulated$.row, rep(1:2, 2))
  re <- random_effect_samples(m, data = data.frame(g = c(NA, NA)), n_draws = 2)
  expect_identical(.Random.seed, before)
  expect_true(all(is.na(re$.value)))
  expect_true(all(is.na(fitted_values(m, data = nd, marginalise = TRUE)$.fitted)))
  nd$x <- c(.1, .5)
  expect_false(is.null(attr(add_fitted_samples(nd, m, marginalise = TRUE,
    n_draws = 2, seed = 1), "integration_info")))
  expect_false(is.null(attr(add_predicted_samples(nd, m, marginalise = TRUE,
    n_draws = 2, seed = 1), "random_effect_info")))
  expect_false(is.null(attr(add_posterior_samples(nd, m, marginalise = TRUE,
    n_draws = 2, seed = 1), "random_effect_info")))
  b <- random_effect_fixture(stats::binomial())
  simulations <- predicted_samples(b, data = nd, weights = 10,
    marginalise = TRUE, n_draws = 20, seed = 2)
  expect_equal(simulations$.response * 10, round(simulations$.response * 10))
  expect_true(all(simulations$.response >= 0 & simulations$.response <= 1))
  b$family <- stats::quasi("log", variance = "mu(1-mu)")
  expect_error(fitted_values(b, data = nd, marginalise = TRUE), "unbounded Gaussian")
  b$family <- stats::binomial("log")
  expect_error(fitted_values(b, data = nd, marginalise = TRUE), "unbounded Gaussian")
})

test_that("fitting offsets retain each prediction interface's existing convention", {
  m <- random_effect_fixture()
  d <- attr(m, "fixture_data")
  d$off <- rep(.4, nrow(d))
  m <- mgcv::gam(y ~ x + s(g, bs = "re"), offset = off,
    data = d, family = poisson(), method = "REML")
  point <- fitted_values(m, marginalise = TRUE)
  draws <- matrix(coef(m), nrow = 1)
  sample <- fitted_samples(m, method = "user", draws = draws, marginalise = TRUE)
  expect_equal(sample$.fitted, point$.fitted * exp(.4))
  explicit <- fitted_samples(m, data = d, method = "user", draws = draws,
    marginalise = TRUE)
  expect_equal(explicit$.fitted, point$.fitted)
  set.seed(3)
  before <- .Random.seed
  fitted_samples(m, method = "user", draws = draws, marginalise = TRUE)
  expect_identical(.Random.seed, before)
  rm(".Random.seed", envir = .GlobalEnv)
  fitted_samples(m, method = "user", draws = draws, marginalise = TRUE)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("conditional values and chain identifiers use integrated predictions", {
  m <- random_effect_fixture()
  cv <- conditional_values(m, condition = "x", n_vals = 4, marginalise = TRUE)
  fv <- fitted_values(m, data = cv[c("x", "g")], marginalise = TRUE)
  expect_equal(cv$.fitted, fv$.fitted)
  fs <- fitted_samples(m, data = m$model[1:2, ], marginalise = TRUE,
    n_draws = 4, chains = 2, seed = 1)
  expect_equal(unique(fs$.draw), 1:4)
  expect_equal(unique(fs$.chain), 1:2)
  expect_false(is.null(attr(cv, "integration_info")))
})

test_that("transformed predictors and grouping expressions are evaluated consistently", {
  m <- random_effect_fixture()
  d <- attr(m, "fixture_data")
  d$id <- as.integer(d$g)
  m <- mgcv::gam(y ~ x + s(log(x), factor(id), bs = "fs", k = 4), data = d,
    family = poisson(), method = "REML")
  nd <- data.frame(x = c(.5, .5, 1), id = c(100, 100, 200))
  r <- random_effect_samples(m, data = nd, n_draws = 3, seed = 1)
  z <- matrix(r$.value, nrow = 3)
  expect_equal(z[2, ], z[1, ])
  fv <- fitted_values(m, data = nd, marginalise = TRUE)
  expect_true(all(is.finite(fv$.fitted)))
  expect_equal(fitted_values(m, data = nd["x"], marginalise = TRUE)$.fitted,
    fv$.fitted)
  expect_true(all(is.finite(predicted_samples(m, data = nd,
    marginalise = TRUE, n_draws = 3, seed = 1)$.response)))
  d$id <- as.integer(d$g)
  m <- mgcv::gam(y ~ id + x + s(x, factor(id), bs = "fs", k = 4), data = d,
    family = poisson(), method = "REML")
  expect_error(fitted_values(m, data = nd, marginalise = TRUE), "retained model terms")
})

test_that("new effects use the fitted likelihood dispersion used by gam.vcomp", {
  m <- random_effect_fixture(stats::gaussian())
  m$reml.scale <- 4
  m$sig2 <- .5
  setup <- random_effect_setup(m)
  expect_equal(setup$info$dispersion, 4)
  expect_equal(setup$info$dispersion_source, "fitted reml.scale")
  expect_equal(setup$terms[[1]]$covariance[1, 1], unname(4 / tail(m$full.sp, 1)))
  m$reml.scale <- NULL
  expect_equal(random_effect_setup(m)$info$dispersion, .5)
  m$reml.scale <- NA_real_
  expect_equal(random_effect_setup(m)$info$dispersion, .5)
})

# Compare the repeated-block implementation with the full fitted coefficient
# basis. This oracle includes every stored fs penalty and its fitted scale.
random_effect_full_covariance <- function(model, data) {
  X <- predict(model, data, type = "lpmatrix")
  sp <- model$full.sp %||% model$sp
  phi <- model$reml.scale %||% model$sig2
  covariance <- matrix(0, nrow(data), nrow(data))
  start <- 0L
  for (sm in model$smooth) {
    n <- length(sm$S)
    lambda <- sp[start + seq_len(n)]
    start <- start + n
    if (!inherits(sm, c("random.effect", "fs.interaction"))) next
    Q <- Reduce(`+`, Map(function(S, a) a * S, sm$S, lambda))
    j <- sm$first.para:sm$last.para
    covariance <- covariance + phi *
      X[, j, drop = FALSE] %*% solve(Q) %*% t(X[, j, drop = FALSE])
  }
  covariance
}

test_that("large random-effect variances do not give false quadrature convergence", {
  integrator <- marginal_integrator("quadrature", 40L, 5000L)
  eta <- c(0, 1)
  v <- rep(10000, 2)
  result <- marginal_integrate(eta, v, stats::binomial("probit"), integrator)
  expect_equal(result$value, stats::pnorm(eta / sqrt(1 + v)))
  expect_equal(result$derivative, stats::dnorm(eta / sqrt(1 + v)) / sqrt(1 + v))
  expect_equal(result$algorithm, "closed form")
  # The even rules agree at 0.5 with effectively zero derivatives. The
  # interlaced rule must expose that failure for links without a closed form.
  expect_error(marginal_integrate(eta, v, stats::binomial(), integrator),
    "quadrature did not converge")
  expect_error(marginal_integrate(0, 10000, stats::binomial(), integrator),
    "quadrature did not converge")

  # Interlaced rules alone can also agree in a tail while missing its slope.
  for (parameters in list(c(sd = 215.443469, location = 4.65),
      c(sd = 464.158883, location = 4.65), c(sd = 100, location = 4.95))) {
    sd <- unname(parameters["sd"])
    eta <- unname(parameters["location"]) * sd
    reference <- marginal_quadrature_check(eta, sd^2, stats::binomial())
    derivative <- stats::integrate(function(t)
      stats::dnorm((eta - t) / sd) / sd * stats::dlogis(t),
      -40, 40, abs.tol = 1e-12, rel.tol = 1e-10)$value
    expect_lt(abs(reference$derivative["value"] - derivative), 1e-12)
    expect_error(marginal_integrate(eta, sd^2, stats::binomial(), integrator),
      "quadrature did not converge")
  }

  m <- random_effect_fixture(stats::binomial("probit"), sp = c(-1, 1e-4))
  nd <- data.frame(x = c(.2, .7))
  result <- fitted_values(m, data = nd, marginalise = TRUE)
  X <- predict(m, transform(nd, g = m$model$g[1]), type = "lpmatrix",
    exclude = "s(g)")
  eta <- drop(X %*% coef(m))
  v <- unname(m$reml.scale / tail(m$full.sp, 1))
  derivative <- stats::dnorm(eta / sqrt(1 + v)) / sqrt(1 + v)
  J <- X * derivative
  expect_equal(result$.fitted, unname(stats::pnorm(eta / sqrt(1 + v))))
  expect_equal(result$.se, unname(sqrt(rowSums((J %*% vcov(m)) * J))))
  expect_true(all(result$.se > 1e-4))
  expect_equal(attr(result, "integration_info")$accuracy$algorithm, "closed form")
  draws <- matrix(rep(coef(m), each = 3), nrow = 3)
  samples <- fitted_samples(m, data = nd, marginalise = TRUE,
    method = "user", draws = draws)
  expect_equal(samples$.fitted, rep(result$.fitted, 3))
  m$family <- stats::binomial()
  expect_error(fitted_values(m, data = nd, marginalise = TRUE),
    "quadrature did not converge")
})

test_that("minimum smoothing parameters cannot silently change new-effect covariance", {
  d <- attr(random_effect_fixture(), "fixture_data")
  # Keep bounds outside the call as well: evaluating the call later would use
  # changed bounds, so it cannot reconstruct the fitted distribution safely.
  bounds <- 20
  m <- mgcv::gam(y ~ x + s(g, bs = "re"), data = d, family = poisson(),
    min.sp = bounds, method = "REML")
  bounds <- 0
  expect_error(random_effect_samples(m, n_draws = 2), "min.sp")
  expect_error(fitted_values(m, data = data.frame(x = .5), marginalise = TRUE), "min.sp")
  expect_error(fitted_samples(m, marginalise = TRUE, n_draws = 2), "min.sp")
  expect_error(predicted_samples(m, marginalise = TRUE, n_draws = 2), "min.sp")
  expect_error(posterior_samples(m, marginalise = TRUE, n_draws = 2), "min.sp")
  expect_equal(fitted_values(m)$.fitted, as.numeric(predict(m, type = "response")))
  fs <- mgcv::gam(y ~ x + s(x, g, bs = "fs", k = 4), data = d,
    family = poisson(), min.sp = rep(2, 3), method = "REML")
  expect_error(random_effect_samples(fs, n_draws = 2), "min.sp")
  expect_error(fitted_values(fs, marginalise = TRUE), "min.sp")
  # An explicit NULL does not supply any lower bounds.
  m <- mgcv::gam(y ~ x + s(g, bs = "re"), data = d, family = poisson(),
    min.sp = NULL, method = "REML")
  expect_true(all(is.finite(random_effect_samples(m, n_draws = 2)$.value)))
})

test_that("factor-by fs curves keep their by levels when another group is replaced", {
  d <- attr(random_effect_fixture(), "fixture_data")
  d$f <- factor(rep(c("A", "B"), length.out = nrow(d)))
  m <- mgcv::gam(y ~ x + s(f, bs = "re") + s(x, g, bs = "fs", k = 4, by = f),
    data = d, family = poisson(), sp = c(2, 3, 4, 5, 6, 7, 8), method = "REML")
  nd <- data.frame(x = c(.2, .8, .2, .8), g = d$g[rep(1, 4)],
    f = factor(c("A", "A", "B", "B"), levels = levels(d$f)))
  K <- random_effect_full_covariance(m, nd)
  result <- fitted_values(m, data = nd, marginalise = TRUE)
  labels <- smooths(m)
  eta <- as.numeric(predict(m, nd, type = "link", exclude = labels))
  expect_equal(unname(attr(result, "integration_info")$variance), unname(diag(K)))
  mu <- exp(eta + diag(K) / 2)
  expect_equal(result$.fitted, unname(mu))
  expect_equal(K[1:2, 3:4], matrix(0, 2, 2), ignore_attr = TRUE)
  characters <- transform(nd, f = as.character(f))
  expect_equal(fitted_values(m, data = characters, marginalise = TRUE)$.fitted,
    result$.fitted)
  expect_equal(random_effect_samples(m, data = characters, n_draws = 3, seed = 2)$.value,
    random_effect_samples(m, data = nd, n_draws = 3, seed = 2)$.value)
  expect_equal(fitted_values(m, data = nd[c("x", "f")], marginalise = TRUE)$.fitted,
    result$.fitted)
  expect_error(fitted_values(m, data = nd[c("x", "g")], marginalise = TRUE),
    "selected by smooths")
  unknown <- transform(nd, f = "new")
  expect_error(fitted_values(m, data = unknown, marginalise = TRUE), "selected by smooths")
  expect_error(predicted_samples(m, data = unknown, marginalise = TRUE, n_draws = 2),
    "selected by smooths")

  # Full new-group response covariance distinguishes independent by curves
  # from the within-curve dependence that simulations must retain.
  expected <- outer(mu, mu) * expm1(K) + diag(mu)
  nd$g <- "new"
  samples <- predicted_samples(m, data = nd, marginalise = TRUE,
    n_draws = 24000, seed = 381)
  z <- matrix(samples$.response, nrow = 4)
  expect_equal(rowMeans(z), unname(mu), tolerance = .04 * max(mu))
  expect_lt(max(abs(stats::cov(t(z)) - expected)), .07 * max(expected))
  expect_lt(abs(stats::cor(z[1, ], z[3, ])), .03)
  draws <- matrix(rep(coef(m), each = 3), nrow = 3)
  fitted <- fitted_samples(m, data = nd, marginalise = TRUE,
    method = "user", draws = draws)
  expect_equal(fitted$.fitted, rep(unname(mu), 3))
})

test_that("a random-effect grouping factor can also select its own by level", {
  d <- attr(random_effect_fixture(), "fixture_data")
  d$f <- factor(rep(c("A", "B"), length.out = nrow(d)))
  m <- mgcv::gam(y ~ x + s(f, bs = "re", by = f), data = d,
    family = poisson(), sp = c(2, 7), method = "REML")
  nd <- data.frame(x = rep(.5, 2), f = factor(c("A", "B"), levels = levels(d$f)))
  K <- random_effect_full_covariance(m, nd)
  sampled <- random_effect_samples(m, data = nd, n_draws = 20000, seed = 851)
  # Sum the independent smooth contributions to obtain the complete effect.
  z <- Reduce(`+`, lapply(split(sampled$.value, sampled$.smooth), matrix, nrow = 2))
  expect_lt(max(abs(stats::cov(t(z)) - K)), .04 * max(K))
  result <- fitted_values(m, data = nd, marginalise = TRUE)
  expect_equal(unname(attr(result, "integration_info")$variance), unname(diag(K)))
  for (sm in m$smooth) {
    values <- sampled$.value[sampled$.smooth == sm$label]
    expect_true(all(matrix(values, nrow = 2)[nd$f != sm$by.level, ] == 0))
  }
})
