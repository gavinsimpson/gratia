library("mgcv")

moment_test_models <- local({
  models <- NULL
  function() {
    if (is.null(models)) {
      models <<- withr::with_seed(481, {
        n <- 240L
        d <- data.frame(x = runif(n), f = factor(rep(c("a", "b"), n / 2)))
        mu <- exp(0.4 + d$x)
        fit <- function(y, family, formula = y ~ x, ...) {
          d$y <- y
          mgcv::gam(formula, data = d, family = family, method = "REML", ...)
        }
        positive <- rgamma(n, shape = 4, scale = mu / 4)
        count <- rpois(n, mu)
        normal <- mu + rnorm(n, sd = 0.4)
        tweedie <- mgcv::rTweedie(mu, p = 1.5, phi = 0.5)
        list(data = d,
          gaussian = fit(normal, gaussian(), y ~ s(x, k = 5)),
          gamma = fit(positive, Gamma(link = "log"), y ~ s(x, k = 5)),
          inverse_gaussian = fit(positive, inverse.gaussian(link = "log")),
          poisson = fit(count, poisson()),
          binomial = fit(rbinom(n, 1, plogis(d$x)), binomial()),
          negative_binomial = fit(rnbinom(n, mu = mu, size = 3), mgcv::nb()),
          beta_regression = fit(rbeta(n, plogis(d$x) * 8, (1 - plogis(d$x)) * 8), mgcv::betar()),
          scaled_t = fit(mu + rt(n, df = 6) / 3, mgcv::scat()),
          tweedie = fit(tweedie, mgcv::tw()),
          fixed_tweedie = fit(tweedie, mgcv::Tweedie(p = 1.5)),
          fixed_nb = fit(count, mgcv::negbin(theta = 2)),
          gaulss = fit(normal, mgcv::gaulss(), list(y ~ x, ~1)),
          gammals = fit(positive, mgcv::gammals(), list(y ~ x, ~1)),
          gumbls = fit(mu - 0.4 * log(-log(runif(n))), mgcv::gumbls(), list(y ~ x, ~1)),
          # Native GEV line searches can try an invalid support before
          # converging. Only suppress the fitter's warning in this fixture.
          gevlss = suppressWarnings(fit(mu + 0.4 * expm1(-0.1 * log(-log(runif(n)))) / 0.1,
            mgcv::gevlss(), list(y ~ x, ~1, ~1))),
          shash = fit(normal, mgcv::shash(), list(y ~ x, ~1, ~1, ~1)),
          twlss = fit(tweedie, mgcv::twlss(), list(y ~ x, ~1, ~1)),
          ziplss = fit(rbinom(n, 1, 0.7) * (1 + rpois(n, mu)),
            mgcv::ziplss(), list(y ~ x, ~1)),
          zip = fit(rbinom(n, 1, 0.7) * (1 + rpois(n, mu)), mgcv::ziP()),
          cnorm = fit(normal, mgcv::cnorm()),
          clog = fit(mu + rlogis(n) / 4, mgcv::clog()),
          cpois = fit(count, mgcv::cpois()),
          ordinal = fit(sample(1:4, n, replace = TRUE), mgcv::ocat(R = 4)),
          multinom = fit(sample(0:2, n, replace = TRUE), mgcv::multinom(K = 2),
            list(y ~ x, ~x)),
          mvn = {
            d$y1 <- normal
            d$y2 <- 1 + 0.5 * normal + rnorm(n, sd = 0.3)
            mgcv::gam(list(y1 ~ x, y2 ~ x), data = d, family = mgcv::mvn(d = 2), method = "REML")
          })
      })
    }
    models
  }
})

test_that("all supported family groups return finite population moments", {
  models <- moment_test_models()
  for (nm in setdiff(names(models), "data")) {
    m <- models[[nm]]
    # GEV and t higher moments need not exist at their fitted shape.
    moments <- if (nm %in% c("gevlss", "scaled_t")) c("mean", "variance", "sd", "cv") else "all"
    actual <- fitted_moments(m, data = models$data[1:2, ], moments = moments,
      uncertainty = "none")
    expect_true(inherits(actual, "fitted_moments"), info = nm)
    expect_true(all(is.finite(actual$.fitted)), info = nm)
    expect_true(all(actual$.status == "ok"), info = nm)
    expect_true(all(is.na(actual$.se)), info = nm)
    delta <- fitted_moments(m, data = models$data[1, , drop = FALSE],
      moments = c("mean", "variance"))
    expect_true(all(is.finite(delta$.se)), info = nm)
    simulation <- fitted_moments(m, data = models$data[1, , drop = FALSE],
      moments = c("mean", "variance"), uncertainty = "simulation", n_draws = 30, seed = 19)
    expect_true(all(is.finite(simulation$.se)), info = nm)
  }
})

test_that("Gamma moments use dispersion and include its uncertainty", {
  m <- moment_test_models()$gamma
  nd <- data.frame(x = c(0.2, 0.8))
  actual <- fitted_moments(m, data = nd, moments = "all")
  mu <- as.vector(predict(m, nd, type = "response"))
  phi <- m$sig2
  take <- function(name) actual$.fitted[actual$.parameter == name]
  expect_equal(take("mean"), mu)
  expect_equal(take("variance"), phi * mu^2)
  expect_equal(take("cv"), rep(sqrt(phi), 2))
  expect_equal(take("skewness"), 2 * take("cv"))
  expect_equal(take("kurtosis"), rep(3 + 6 * phi, 2))
  H <- m$outer.info$hess
  expect_equal(actual$.se[actual$.parameter == "cv"],
    rep(sqrt(phi) / (2 * sqrt(H[nrow(H), ncol(H)])), 2), tolerance = 1e-8)
  fixed <- fitted_moments(m, data = nd, nuisance = "fixed", moments = "cv")
  expect_equal(fixed$.se, c(0, 0))
  expect_gt(min(actual$.se[actual$.parameter == "cv"]), 0)
  expect_true(all(actual$.lower_ci[actual$.parameter == "variance"] > 0))
})

test_that("analytic derivatives match independent coefficient perturbations", {
  models <- moment_test_models()
  for (nm in c("gaussian", "poisson", "gamma", "inverse_gaussian", "tweedie", "gammals", "twlss")) {
    m <- models[[nm]]
    ctx <- lss_prediction_context(m, data = models$data[1:2, ])
    ctx$family <- unserialize(serialize(ctx$family, NULL))
    pars <- moment_parameter_context(m, "estimated", FALSE)
    p <- length(coef(m))
    eval <- function(b) {
      if (pars$nth) ctx$family$putTheta(b[p + seq_len(pars$nth)])
      scale <- if (pars$scale) exp(tail(b, 1L)) else m$sig2
      as.vector(moment_family_values(ctx$family,
        lss_predictors(ctx, b[seq_len(p)]), scale, b[seq_len(p)])$values)
    }
    point <- moment_family_values(ctx$family, lss_predictors(ctx, coef(m)), m$sig2, coef(m))
    numerical <- moment_gradient(eval, pars$centre, eval(pars$centre))
    # Reset the mutable test family before evaluating the analytic gradient.
    eval(pars$centre)
    analytic <- moment_analytic_gradient(ctx, pars, point, moment_names())
    expect_equal(analytic, numerical, tolerance = 2e-6, info = nm)
  }
})

test_that("joint uncertainty retains covariance between LSS predictors", {
  m <- moment_test_models()$gammals
  nd <- data.frame(x = c(0.3, 0.7))
  X <- predict(m, nd, type = "lpmatrix")
  idx <- attr(X, "lpi")
  # A controlled correlation supplies an oracle even when the fitted family
  # has orthogonal mean and dispersion estimates at its optimum.
  m$Vp <- diag(diag(m$Vp))
  a <- idx[[1]][1]
  b <- idx[[2]][1]
  m$Vp[a, b] <- m$Vp[b, a] <- 0.4 * sqrt(m$Vp[a, a] * m$Vp[b, b])
  actual <- fitted_moments(m, data = nd, moments = "variance")
  eta <- predict(m, nd, type = "link")
  mu <- exp(m$family$linfo[[1]]$linkinv(eta[, 1]))
  phi <- exp(m$family$linfo[[2]]$linkinv(eta[, 2]))
  J <- matrix(0, nrow(nd), length(coef(m)))
  J[, idx[[1]]] <- 2 * phi * mu^2 * X[, idx[[1]], drop = FALSE]
  J[, idx[[2]]] <- phi * mu^2 * m$family$linfo[[2]]$mu.eta(eta[, 2]) * X[, idx[[2]], drop = FALSE]
  expected <- sqrt(diag(J %*% m$Vp %*% t(J)))
  expect_equal(actual$.se, expected, tolerance = 1e-8)
  independent <- m$Vp
  independent[idx[[1]], idx[[2]]] <- 0
  independent[idx[[2]], idx[[1]]] <- 0
  expect_gt(max(abs(expected - sqrt(diag(J %*% independent %*% t(J))))), 1e-5)
})

test_that("delta and joint simulation agree locally for global and LSS parameters", {
  models <- moment_test_models()
  for (nm in c("gamma", "tweedie", "negative_binomial", "beta_regression", "gammals", "twlss")) {
    m <- models[[nm]]
    nd <- models$data[1, , drop = FALSE]
    before <- if (is.function(m$family$getTheta)) m$family$getTheta() else NULL
    delta <- fitted_moments(m, data = nd, moments = c("mean", "variance", "cv"))
    simulation <- fitted_moments(m, data = nd, moments = c("mean", "variance", "cv"),
      uncertainty = "simulation", n_draws = 6000, seed = 27)
    expect_equal(simulation$.fitted, delta$.fitted, info = nm)
    expect_equal(simulation$.se / delta$.se, rep(1, 3), tolerance = 0.14, info = nm)
    if (!is.null(before)) expect_identical(m$family$getTheta(), before, info = nm)
  }
})

test_that("GEV moments agree with integration through the Gumbel limit", {
  for (shape in c(-0.7, -0.1, -0.0001, 0, 0.0001, 0.1, 0.2)) {
    z <- function(x) if (shape == 0) x else expm1(shape * x) / shape
    integrate <- function(fun) stats::integrate(function(x)
      fun(z(x)) * exp(-x - exp(-x)), -12, 500, rel.tol = 1e-9)$value
    mean <- integrate(identity)
    variance <- integrate(function(x) (x - mean)^2)
    skew <- integrate(function(x) (x - mean)^3) / variance^1.5
    kurt <- integrate(function(x) (x - mean)^4) / variance^2
    actual <- moment_gev(0, 1, shape)
    expect_equal(as.vector(actual[, c("mean", "variance", "skewness", "kurtosis")]),
      c(mean, variance, skew, kurt), tolerance = 2e-7, info = as.character(shape))
  }
  expect_true(is.na(moment_gev(2, 1, 0.3)[, "kurtosis"]))
  expect_true(is.na(moment_gev(2, 1, 0.4)[, "skewness"]))
})

test_that("sinh-arcsinh and hurdle moments agree with independent integrals and sums", {
  for (delta in c(0.6, 1, 2, 200)) {
    eps <- 0.4
    transform <- function(z) 1 + delta * 0.7 * sinh((asinh(z) + eps) / delta)
    integral <- function(fun) stats::integrate(function(z)
      fun(transform(z)) * dnorm(z), -12, 12, rel.tol = 1e-10)$value
    mean <- integral(identity)
    variance <- integral(function(x) (x - mean)^2)
    skew <- integral(function(x) (x - mean)^3) / variance^1.5
    kurt <- integral(function(x) (x - mean)^4) / variance^2
    actual <- moment_shash(matrix(c(1, log(0.7), eps, log(delta)), 1))
    expect_equal(as.vector(actual[, c("mean", "variance", "skewness", "kurtosis")]),
      c(mean, variance, skew, kurt), tolerance = 1e-8)
  }
  for (lambda in c(1e-8, 0.3, 3, 100)) {
    p <- 0.6
    y <- 0:300
    prob <- c(1 - p, p * dpois(y[-1], lambda) / (-expm1(-lambda)))
    mean <- sum(y * prob)
    variance <- sum((y - mean)^2 * prob)
    expected <- c(mean, variance, sum((y - mean)^3 * prob) / variance^1.5,
      sum((y - mean)^4 * prob) / variance^2)
    actual <- moment_hurdle(lambda, p)
    expect_equal(as.vector(actual[, c("mean", "variance", "skewness", "kurtosis")]), expected,
      tolerance = 1e-7)
  }
})

test_that("categorical scores and multivariate covariance use the full model", {
  models <- moment_test_models()
  nd <- models$data[1:2, ]
  for (nm in c("ordinal", "multinom")) {
    m <- models[[nm]]
    p <- predict(m, nd, type = "response")
    category <- fitted_moments(m, data = nd, moments = "mean")
    expect_equal(category$.fitted, as.vector(t(p)))
    scores <- seq_len(ncol(p)) * 2
    scored <- fitted_moments(m, data = nd, moments = c("mean", "variance"), scores = scores)
    mean <- as.vector(p %*% scores)
    expect_equal(scored$.fitted[scored$.parameter == "mean"], mean)
    expect_equal(scored$.fitted[scored$.parameter == "variance"], as.vector(p %*% scores^2) - mean^2)
  }
  m <- models$mvn
  actual <- fitted_moments(m, data = nd, moments = "variance")
  variance <- diag(solve(crossprod(m$family$data$R)))
  expect_equal(actual$.fitted, rep(variance, 2))
  expect_true(all(actual$.se > 0))
  beta <- rbind(coef(m), coef(m))
  beta[2, ncol(beta)] <- beta[2, ncol(beta)] + 0.2
  changed <- fitted_moments(m, data = nd, moments = "variance",
    uncertainty = "simulation", method = "user", draws = beta, nuisance = "fixed")
  expect_true(any(changed$.se > 0))
})

test_that("prediction rows, offsets and conditional grid metadata are retained", {
  d <- withr::with_seed(30, data.frame(x = seq(0, 1, length = 100),
    y = rpois(100, 3), off = seq(0, 0.3, length = 100)))
  d$x[4] <- NA
  m <- mgcv::gam(y ~ x, data = d, family = poisson(), offset = off,
    na.action = na.exclude, method = "REML")
  actual <- fitted_moments(m, moments = "mean")
  expect_equal(actual$.fitted, as.vector(fitted(m)))
  expect_identical(actual$.status[4], "missing_predictors")
  nd <- data.frame(x = c(0.5, NA, 0.2), off = 0)
  new <- fitted_moments(m, data = nd, moments = "mean")
  expect_equal(new$.fitted, as.vector(predict(m, nd, type = "response")))
  omitted <- fitted_moments(m, data = nd, moments = "mean", na.action = na.omit)
  expect_identical(omitted$.row, c(1L, 3L))
  missing <- fitted_moments(m, data = nd[2, , drop = FALSE], moments = "all")
  expect_true(all(is.na(missing$.fitted)))
  expect_true(all(missing$.status == "missing_predictors"))
  conditional <- conditional_moments(m, condition = list(x = c(0.1, 0.9)),
    moments = c("mean", "variance"))
  expect_s3_class(conditional, "conditional_moments")
  expect_equal(unique(conditional$x), c(0.1, 0.9))
  expect_equal(attr(conditional[1:2, ], "channels"), attr(conditional, "channels"))
  expect_s3_class(draw(conditional), "patchwork")
})

test_that("undefined moments and undefined draws are retained explicitly", {
  m <- moment_test_models()$gaussian
  m$coefficients[] <- 0
  expect_warning(actual <- fitted_moments(m, data = data.frame(x = 0.5),
    moments = "cv"), "undefined")
  expect_identical(actual$.status, "nonpositive_mean")
  m <- moment_test_models()$gevlss
  idx <- attr(predict(m, data.frame(x = 0.5), type = "lpmatrix"), "lpi")
  m$coefficients[idx[[3]]] <- m$family$linfo[[3]]$linkfun(0.4)
  expect_warning(actual <- fitted_moments(m, data = data.frame(x = 0.5),
    moments = c("mean", "variance", "skewness", "kurtosis")), "undefined")
  expect_equal(actual$.status, c("ok", "ok", "moment_does_not_exist", "moment_does_not_exist"))
  m$coefficients[idx[[3]]] <- m$family$linfo[[3]]$linkfun(0.2)
  beta <- rbind(coef(m), coef(m))
  beta[2, idx[[3]]] <- m$family$linfo[[3]]$linkfun(0.4)
  expect_warning(actual <- fitted_moments(m, data = data.frame(x = 0.5),
    moments = "kurtosis", uncertainty = "simulation", method = "user", draws = beta), "undefined")
  expect_equal(actual$.p_defined, 0.5)
  expect_identical(actual$.status, "undefined_in_draws")
  expect_true(is.finite(actual$.fitted))
  expect_true(is.na(actual$.se) && is.na(actual$.lower_ci))
})

test_that("simulation is reproducible and unsupported covariance never falls back", {
  m <- moment_test_models()$gamma
  nd <- data.frame(x = 0.5)
  set.seed(431)
  before <- .Random.seed
  a <- fitted_moments(m, data = nd, uncertainty = "simulation", n_draws = 25, seed = 19)
  expect_identical(.Random.seed, before)
  expect_identical(a, fitted_moments(m, data = nd, uncertainty = "simulation", n_draws = 25, seed = 19))
  fitted_moments(m, data = nd, uncertainty = "simulation", n_draws = 25)
  expect_false(identical(.Random.seed, before))
  m$outer.info <- NULL
  expect_error(fitted_moments(m, data = nd), "Joint uncertainty")
  expect_s3_class(fitted_moments(m, data = nd, nuisance = "fixed"), "fitted_moments")
  expect_s3_class(fitted_moments(m, data = nd, uncertainty = "none"), "fitted_moments")
  expect_error(fitted_moments(m, moments = "bogus"), "moments")
  expect_error(fitted_moments(m, moments = c("mean", "mean")), "moments")
  expect_error(fitted_moments(m, uncertainty = "simulation", n_draws = 1), "Joint uncertainty|n_draws")
  expect_error(fitted_moments(m, type = "response"), "controlled|cannot be supplied")
  expect_error(fitted_moments(m, scores = 1:2), "scores")
  expect_error(fitted_moments(m, seed = NA_integer_), "seed")
  expect_error(fitted_moments(m, ci_level = 1), "ci_level")
  m$family <- quasipoisson()
  expect_error(fitted_moments(m), "quasi")
})

test_that("grouped moments use component families with joint parameter uncertainty", {
  d <- withr::with_seed(48, {
    index <- rep(1:3, 80)
    x <- runif(length(index))
    eta <- 0.7 + x
    y <- ifelse(index == 1, rbinom(length(x), 1, plogis(eta)),
      ifelse(index == 2, rnorm(length(x), eta, 0.4), rpois(length(x), exp(eta))))
    data.frame(x, index, y, group = factor(index))
  })
  m <- mgcv::gam(cbind(y, index) ~ group + x, data = d,
    family = mgcv::gfam(list(binomial(), gaussian(), poisson())), method = "REML")
  nd <- d[1:3, c("x", "index", "group")]
  before <- m$family$getTheta()
  actual <- fitted_moments(m, data = nd, moments = c("mean", "variance"))
  eta <- as.vector(predict(m, nd, type = "link"))
  mean <- c(plogis(eta[1]), eta[2], exp(eta[3]))
  variance <- c(mean[1] * (1 - mean[1]), exp(before[1]), mean[3])
  expect_equal(actual$.fitted[actual$.parameter == "mean"], mean)
  expect_equal(actual$.fitted[actual$.parameter == "variance"], variance)
  expect_identical(actual$.family, rep(1:3, each = 2))
  expect_true(all(actual$.se > 0))
  simulation <- fitted_moments(m, data = nd, moments = c("mean", "variance"),
    uncertainty = "simulation", n_draws = 500, seed = 26)
  expect_equal(simulation$.fitted, actual$.fitted)
  expect_equal(simulation$.se / actual$.se, rep(1, 6), tolerance = 0.2)
  expect_identical(m$family$getTheta(), before)
  indexed <- fitted_moments(m, data = nd[, c("x", "group")], family_index = 1:3,
    moments = "mean")
  expect_equal(indexed$.fitted, mean)
  expect_error(fitted_moments(m, data = nd[, c("x", "group")]), "membership")
  expect_error(fitted_moments(m, data = nd, family_index = c(2, 2, 3)), "conflicts")
  nd$x[2] <- NA
  missing <- fitted_moments(m, data = nd, moments = "mean")
  expect_identical(missing$.status[2], "missing_predictors")
  expect_identical(missing$.family, 1:3)
})

test_that("fixed Box-Cox zero gives original-response lognormal moments", {
  d <- withr::with_seed(76, data.frame(x = runif(150), y = exp(rnorm(150, 1, 0.3))))
  family <- mgcv::bcg(theta = c(0, 0.3))
  # mgcv 1.9-4's fixed-parameter constructor packs an extra element. Set its
  # documented lambda/log-SD vector explicitly before fitting this fixture.
  family$putTheta(c(0, log(0.3)))
  m <- mgcv::gam(y ~ x, data = d, family = family, method = "REML")
  actual <- fitted_moments(m, data = d[1:2, ], moments = c("mean", "variance"))
  mu <- as.vector(predict(m, d[1:2, ], type = "response"))
  sigma <- m$family$getTheta(TRUE)[2]
  mean <- exp(mu + sigma^2 / 2)
  expect_equal(actual$.fitted[actual$.parameter == "mean"], mean)
  expect_equal(actual$.fitted[actual$.parameter == "variance"], expm1(sigma^2) * mean^2)
  m$family$putTheta(c(0.2, log(sigma)))
  expect_error(fitted_moments(m, data = d[1:2, ]), "Box-Cox")
})

test_that("invalid response parameters are not treated as valid moment draws", {
  m <- moment_test_models()$gamma
  # Changing the fitted link creates a controlled inverse-link domain crossing.
  link <- stats::make.link("inverse")
  m$family$link <- link$name
  m$family$linkinv <- link$linkinv
  m$family$mu.eta <- link$mu.eta
  m$coefficients[] <- 0
  m$coefficients[1] <- 1
  beta <- rbind(coef(m), -coef(m))
  expect_warning(actual <- fitted_moments(m, data = data.frame(x = 0.5), moments = "variance",
    uncertainty = "simulation", nuisance = "fixed", method = "user", draws = beta), "undefined")
  expect_equal(actual$.p_defined, 0.5)
  expect_true(is.na(actual$.se))
  m <- moment_test_models()$poisson
  withr::local_seed(94)
  before <- .Random.seed
  fitted_moments(m, data = data.frame(x = 0.5), moments = "mean",
    uncertainty = "simulation", n_draws = 10)
  expect_false(identical(.Random.seed, before))
})

test_that("smoothing uncertainty is added once in the joint approximation", {
  m <- moment_test_models()$gamma
  nd <- data.frame(x = 0.5)
  actual <- fitted_moments(m, data = nd, moments = c("mean", "cv"), unconditional = TRUE)
  mu <- as.vector(predict(m, nd, type = "response"))
  X <- predict(m, nd, type = "lpmatrix")
  D <- cbind(m$db.drho, 0)
  covariance <- m$Vp + D %*% m$V.sp %*% t(D)
  expected_mean_se <- mu * sqrt(drop(X %*% covariance %*% t(X)))
  k <- nrow(m$V.sp)
  expected_cv_se <- sqrt(m$sig2 * m$V.sp[k, k]) / 2
  expect_equal(actual$.se, c(expected_mean_se, expected_cv_se), tolerance = 1e-8)
  expect_match(attr(actual, "moment_info")$covariance, "native outer covariance")
  conditional <- fitted_moments(m, data = nd, moments = "cv")
  expect_equal(conditional$.se, sqrt(m$sig2 / m$outer.info$hess[k, k]) / 2)
})

test_that("BAM supports coefficient uncertainty and rejects unavailable global curvature", {
  d <- withr::with_seed(52, data.frame(x = runif(100), y = rpois(100, 3)))
  poisson <- mgcv::bam(y ~ s(x, k = 5), data = d, family = poisson(), discrete = TRUE)
  actual <- fitted_moments(poisson, data = d[1:2, ], moments = "mean")
  native <- predict(poisson, d[1:2, ], type = "link", se.fit = TRUE)
  expect_equal(actual$.se, as.vector(exp(native$fit) * native$se.fit), tolerance = 1e-6)
  normal <- mgcv::bam(y ~ s(x, k = 5), data = d, discrete = TRUE)
  expect_error(fitted_moments(normal, data = d[1:2, ]), "Joint uncertainty")
  expect_s3_class(fitted_moments(normal, data = d[1:2, ], nuisance = "fixed"), "fitted_moments")
})

test_that("standard-family higher moments agree with independent response laws", {
  summarise <- function(raw) {
    mean <- raw[1]
    variance <- raw[2] - mean^2
    c(mean, variance, (raw[3] - 3 * mean * raw[2] + 2 * mean^3) / variance^1.5,
      (raw[4] - 4 * mean * raw[3] + 6 * mean^2 * raw[2] - 3 * mean^4) / variance^2)
  }
  columns <- c("mean", "variance", "skewness", "kurtosis")
  y <- 0:500
  nb <- moment_family_values(mgcv::nb(theta = 2.3), matrix(log(4.2), 1), 1, numeric())
  raw <- vapply(1:4, function(r) sum(y^r * dnbinom(y, size = 2.3, mu = 4.2)), numeric(1))
  expect_equal(as.vector(nb$values[, columns]), summarise(raw), tolerance = 1e-9)

  cases <- list(
    list(family = Gamma(link = "log"), eta = log(3.5), scale = 0.4,
      density = function(y) dgamma(y, shape = 2.5, scale = 1.4), lower = 0, upper = Inf),
    list(family = mgcv::betar(theta = 6.2), eta = qlogis(0.35), scale = 1,
      density = function(y) dbeta(y, shape1 = 0.35 * 6.2, shape2 = 0.65 * 6.2), lower = 0, upper = 1),
    list(family = inverse.gaussian(link = "log"), eta = log(2), scale = 0.3,
      density = function(y) exp(-(y - 2)^2 / (2 * 0.3 * 4 * y)) /
        sqrt(2 * pi * 0.3 * y^3), lower = 0, upper = Inf),
    list(family = mgcv::scat(theta = c(6, 0.7)), eta = 2, scale = 1,
      density = function(y) dt((y - 2) / 0.7, df = 6) / 0.7, lower = -Inf, upper = Inf))
  for (case in cases) {
    raw <- vapply(1:4, function(r) stats::integrate(function(y)
      y^r * case$density(y), case$lower, case$upper, rel.tol = 1e-9)$value, numeric(1))
    actual <- moment_family_values(case$family, matrix(case$eta, 1), case$scale, numeric())
    expect_equal(as.vector(actual$values[, columns]), summarise(raw), tolerance = 1e-7)
  }

  # Sum the Gamma moments conditional on the Poisson number of increments.
  mu <- 3
  phi <- 0.4
  power <- 1.4
  rate <- mu^(2 - power) / (phi * (2 - power))
  n <- seq_len(qpois(1 - 1e-14, rate))
  shape <- n * (2 - power) / (power - 1)
  scale <- phi * (power - 1) * mu^(power - 1)
  raw <- vapply(1:4, function(r) sum(dpois(n, rate) *
    exp(r * log(scale) + lgamma(shape + r) - lgamma(shape))), numeric(1))
  actual <- moment_family_values(mgcv::Tweedie(p = power), matrix(log(mu), 1), phi, numeric())
  expect_equal(as.vector(actual$values[, columns]), summarise(raw), tolerance = 1e-8)
})

test_that("fixed global parameters stay fixed when smoothing uncertainty is requested", {
  models <- moment_test_models()
  m <- models$gamma
  nd <- data.frame(x = 0.5)
  idx <- seq_along(m$sp)
  D <- m$db.drho
  C <- solve(m$outer.info$hess[idx, idx, drop = FALSE])
  V <- m$Vp + D %*% C %*% t(D)
  X <- predict(m, nd, type = "lpmatrix")
  mu <- as.vector(predict(m, nd, type = "response"))
  actual <- fitted_moments(m, data = nd, moments = c("mean", "cv"),
    nuisance = "fixed", unconditional = TRUE)
  expect_equal(actual$.se, c(mu * sqrt(drop(X %*% V %*% t(X))), 0))
  sim <- fitted_moments(m, data = nd, moments = c("mean", "cv"),
    nuisance = "fixed", unconditional = TRUE, uncertainty = "simulation", n_draws = 2000, seed = 47)
  expect_equal(sim$.se[1] / actual$.se[1], 1, tolerance = 0.07)
  expect_lt(sim$.se[2], 1e-14)
  for (nm in c("negative_binomial", "beta_regression")) {
    m <- models[[nm]]
    actual <- fitted_moments(m, data = nd, moments = "mean", nuisance = "fixed", unconditional = TRUE)
    eta <- predict(m, nd, type = "link")
    X <- predict(m, nd, type = "lpmatrix")
    expected <- m$family$mu.eta(eta) * sqrt(drop(X %*% m$Vp %*% t(X)))
    expect_equal(actual$.se, as.vector(expected), tolerance = 1e-7)
  }
  m <- models$fixed_nb
  m$method <- "GCV.Cp"
  expect_error(fitted_moments(m, data = nd), "Legacy negbin")
  expect_s3_class(fitted_moments(m, data = nd, nuisance = "fixed"), "fitted_moments")
  m <- models$inverse_gaussian
  link <- stats::make.link("inverse")
  m$family$linkinv <- link$linkinv
  m$coefficients[] <- 0
  m$coefficients[1] <- 1
  beta <- rbind(coef(m), -coef(m))
  warnings <- character()
  withr::local_seed(83)
  before <- .Random.seed
  withCallingHandlers(fitted_moments(m, data = nd, moments = "variance",
    uncertainty = "simulation", nuisance = "fixed", method = "user", draws = beta),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_length(warnings, 1)
  expect_match(warnings, "undefined")
  expect_identical(.Random.seed, before)
})
