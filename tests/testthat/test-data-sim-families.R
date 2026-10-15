test_that("legacy example fixes preserve predictor truth and arbitrary sample sizes", {
  for (model in c("eg5", "eg6")) for (n in c(1, 3, 10)) {
    expect_equal(nrow(data_sim(model, n = n, seed = 4)), n)
  }
  for (dist in c("normal", "poisson", "binary", "negbin", "tweedie", "gamma")) {
    d <- data_sim("eg6", n = 80, dist = dist, scale = 0.3,
      theta = 7, power = 1.7, seed = 8)
    raw <- d$f0 + d$f1 + d$f2 + as.integer(d$fac) * 3
    expected <- switch(dist, normal = raw, poisson =, negbin = raw * 0.3,
      binary = (raw - 5) * 0.3, tweedie =, gamma = raw / 3 + 0.1)
    expect_equal(d$f, expected)
  }
  expect_error(data_sim("absent"), "Unknown 'model'")
  for (n in list(0, -1, 1.5, NA, Inf, c(1, 2))) expect_error(data_sim(n = n), "'n'")
  expect_error(data_sim(dist = "negbin", theta = 0), "theta")
  expect_error(data_sim(dist = "tweedie", power = 2), "power")
  expect_error(data_sim(dist = "gamma", scale = 0), "scale")
})

test_that("seeded simulation restores existing and absent caller RNG states", {
  withr::with_preserve_seed({
    set.seed(199)
    before <- .Random.seed
    expect_identical(data_sim(seed = 3), data_sim(seed = 3))
    expect_identical(.Random.seed, before)
    expect_error(data_sim(seed = 3, family = mgcv::nb()), "theta")
    expect_identical(.Random.seed, before)
    rm(".Random.seed", envir = .GlobalEnv)
    data_sim(seed = 3)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_error(data_sim(seed = 3, family = mgcv::nb()), "theta")
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    data_sim(n = 4)
    expect_true(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
})

test_that("legacy grouped families retain positions and use supplied parameters", {
  a <- data_sim("gfam", n = 40, seed = 2,
    gfam_families = c("ordered categorical", "normal", "normal"))
  expect_false(anyNA(a$index))
  expect_setequal(unique(a$index), 1:3)
  expect_true(all(a$family[a$index == 1] == "ocat"))
  expect_identical(a, data_sim("gfam", n = 40, seed = 2, dist = "ocat",
    gfam_families = c("ordered categorical", "normal", "normal")))
  expect_false(identical(a$x0, data_sim("gfam", n = 40, seed = 8)$x0))
  a <- data_sim("gfam", seed = 2, gfam_families = c("normal", "negbin"))
  b <- data_sim("gfam", seed = 2, gfam_families = c("normal", "negbin"), scale = 2, theta = 8)
  expect_identical(a$x0, b$x0)
  expect_false(identical(a$y, b$y))
  expect_error(data_sim("gfam", gfam_families = "bad"), "Unknown distribution")
  expect_error(data_sim("gfam", n = 1), "at least")
  expect_error(data_sim("gfam", gfam_families = character()), "nonempty")
  expect_false(identical(create_reference_simulations(n = 8, power = 1.3)[["eg1-tweedie"]]$y,
    create_reference_simulations(n = 8, power = 1.8)[["eg1-tweedie"]]$y))
})

test_that("all family generators return aligned finite responses including one row", {
  cases <- list(
    list(stats::gaussian(), list()), list(stats::poisson(), list()),
    list(stats::binomial(), list(trials = 5)), list(stats::Gamma(), list()),
    list(stats::inverse.gaussian(), list()), list(mgcv::nb(theta = 3), list()),
    list(mgcv::negbin(3), list()), list(mgcv::tw(theta = 1.5), list()),
    list(mgcv::Tweedie(p = 1.5), list()), list(mgcv::betar(theta = 10), list()),
    list(mgcv::scat(theta = c(5, 1)), list()),
    list(mgcv::ocat(R = 4), list(cuts = c(-1, 0, 5))),
    list(mgcv::ziP(theta = c(0, 0)), list()),
    list(mgcv::cnorm(theta = 1), list()), list(mgcv::clog(theta = 1), list()),
    list(mgcv::cpois(), list()), list(mgcv::bcg(), list(lambda = 0, sigma = 1)),
    list(mgcv::gaulss(), list()), list(mgcv::gammals(), list()),
    list(mgcv::gumbls(), list()), list(mgcv::gevlss(), list()),
    list(mgcv::shash(), list()), list(mgcv::twlss(), list()),
    list(mgcv::ziplss(), list()), list(mgcv::multinom(K = 2), list()),
    list(mgcv::mvn(d = 3), list()), list(mgcv::cox.ph(), list()))
  for (case in cases) for (n in c(1, 20)) {
    d <- data_sim(n = n, seed = 4, family = case[[1]], parameters = case[[2]])
    expect_equal(nrow(d), n)
    expect_equal(NROW(d$y), n)
    expect_true(all(is.finite(d$y)), info = case[[1]]$family)
    expect_equal(d$f, attr(d, "truth")$eta[, 1L])
    if (family_type(case[[1]]) != "cox_ph") expect_identical(d$y, attr(d, "truth")$response)
  }
})

test_that("family inputs and generating parameter domains are checked", {
  expect_error(data_sim(family = stats::poisson), "instantiated")
  expect_error(data_sim(family = stats::quasipoisson()), "Quasi")
  expect_error(data_sim(family = stats::poisson(), dist = "poisson"), "instead of: dist")
  expect_error(data_sim(family = stats::gaussian(), scale = 2), "instead of: scale")
  expect_error(data_sim(parameters = list(dispersion = 2)), "require 'family'")
  expect_error(data_sim(family = stats::gaussian(), parameters = list(phi = 1)), "Unknown generating")
  expect_error(data_sim(family = stats::gaussian(), parameters = list(1)), "uniquely named")
  for (family in list(mgcv::nb(), mgcv::tw(), mgcv::betar(), mgcv::scat(),
      mgcv::ocat(R = 4), mgcv::ziP(), mgcv::cnorm(), mgcv::clog(), mgcv::bcg())) {
    expect_error(data_sim(family = family), "Supply")
  }
  expect_error(data_sim(family = mgcv::nb(theta = 3), parameters = list(theta = 4)), "conflicts")
  expect_error(data_sim(family = mgcv::bcg(), parameters = list(lambda = 1, sigma = 1)), "lambda = 0")
  expect_error(data_sim(family = mgcv::ocat(R = 4), parameters = list(cuts = c(0, 1))), "number of categories")
  expect_error(data_sim(family = stats::binomial(), parameters = list(trials = 1.5)), "integer")
  expect_error(data_sim(family = stats::gaussian(), predictors = 1), "function")
  expect_error(data_sim(family = mgcv::gaulss(), predictors = function(d) d$f), "one column")
  expect_error(data_sim(family = stats::gaussian(), predictors = function(d) rep(NA_real_, nrow(d))), "finite")
  expect_error(data_sim(family = stats::Gamma(link = "identity"), predictors = function(d) -d$f), "invalid")
  expect_error(data_sim(family = mgcv::mvn(d = 2), parameters = list(covariance = matrix(1, 2, 2))), "positive definite")
  for (family in list(mgcv::nb(theta = 3), mgcv::ocat(R = 4), mgcv::gaulss(),
      mgcv::gfam(list(stats::gaussian(), mgcv::nb(theta = 3))))) {
    before <- unserialize(serialize(family, NULL))
    p <- if (family_type(family) == "ordered_categorical") list(cuts = c(-1, 0, 5)) else list()
    data_sim(family = family, parameters = p, seed = 1)
    expect_equal(family, before)
    if (is.function(family$getTheta)) expect_equal(theta(family), theta(before))
  }
})

test_that("scalar and location-scale generators have the specified moments", {
  withr::with_seed(221, {
    n <- 60000L
    scalar <- function(family, parameters = list(), eta = 0) {
      sim_family_response(family, matrix(rep(eta, n), ncol = 1), parameters)$y
    }
    cases <- list(
      list(stats::gaussian(), list(dispersion = 0.7), 2, 2, 0.7),
      list(stats::poisson(), list(), log(2), 2, 2),
      list(stats::binomial(), list(trials = 10), stats::qlogis(0.3), 0.3, 0.021),
      list(stats::Gamma(link = "log"), list(dispersion = 0.4), log(2), 2, 1.6),
      list(stats::inverse.gaussian(link = "log"), list(dispersion = 0.2), log(2), 2, 1.6),
      list(mgcv::nb(theta = 3), list(), log(2), 2, 2 + 4 / 3),
      list(mgcv::betar(theta = 10), list(), stats::qlogis(0.3), 0.3, 0.21 / 11),
      list(mgcv::scat(theta = c(8, 0.7)), list(), 2, 2, 0.7^2 * 8 / 6),
      list(mgcv::tw(theta = 1.5), list(dispersion = 0.4), log(2), 2, 0.4 * 2^1.5))
    for (case in cases) {
      y <- scalar(case[[1]], case[[2]], case[[3]])
      expect_lt(abs(mean(y) - case[[4]]), 6 * sqrt(case[[5]] / n))
      expect_lt(abs(var(y) / case[[5]] - 1), 0.06)
    }
    eta <- cbind(rep(2, n), rep(log(0.5 - 0.01), n))
    d <- sim_family_response(mgcv::gaulss(), eta)
    expect_equal(d$parameters$sigma, rep(0.5, n))
    expect_lt(abs(var(d$y) - 0.25), 0.01)
    eta <- cbind(rep(log(2), n), rep(0, n))
    f <- mgcv::gammals()
    d <- sim_family_response(f, eta)
    phi <- exp(f$linfo[[2]]$linkinv(0))
    expect_lt(abs(mean(d$y) - 2), 0.02)
    expect_lt(abs(var(d$y) / (4 * phi) - 1), 0.05)
    # Both the Tweedie power and dispersion vary by row; do not recycle
    # gamma shapes across compound-Poisson events.
    eta <- cbind(rep(log(2), n), rep(c(-1, 1), each = n / 2), rep(c(-0.5, 0.5), each = n / 2))
    d <- sim_family_response(mgcv::twlss(), eta)
    for (rows in list(seq_len(n / 2), seq.int(n / 2 + 1, n))) {
      p <- d$parameters$power[rows[1]]; phi <- d$parameters$dispersion[rows[1]]
      expect_lt(abs(mean(d$y[rows]) - 2), 0.06)
      expect_lt(abs(var(d$y[rows]) / (phi * 2^p) - 1), 0.08)
    }
  })
})

test_that("ordinal, hurdle, multinomial and multivariate responses follow their laws", {
  withr::with_seed(76, {
    n <- 40000L
    d <- sim_family_response(mgcv::ocat(R = 4), matrix(0, n, 1), list(cuts = c(-1, 0, 1)))
    p <- diff(stats::plogis(c(-Inf, -1, 0, 1, Inf)))
    expect_equal(as.numeric(tabulate(d$y, 4) / n), p, tolerance = 0.01)
    for (f in list(mgcv::ziP(theta = c(0, 0)), mgcv::ziplss())) {
      eta <- matrix(0, n, sim_family_nlp(f))
      d <- sim_family_response(f, eta)
      presence <- -expm1(-1)
      expect_lt(abs(mean(d$y == 0) - (1 - presence)), 0.01)
      expect_lt(abs(mean(d$y[d$y > 0]) - 1 / (-expm1(-1))), 0.025)
    }
    expect_true(all(sim_hurdle_poisson(rep(1e-15, 100), rep(1, 100)) == 1))
    f <- mgcv::multinom(K = 2)
    d <- sim_family_response(f, cbind(rep(log(2), n), rep(log(3), n)))
    expect_equal(tabulate(d$y + 1, 3) / n, c(1, 2, 3) / 6, tolerance = 0.01)
    expect_true(all(is.finite(sim_family_response(f, cbind(rep(1000, 2), rep(-1000, 2)))$y)))
    V <- matrix(c(1, 0.6, 0.6, 2), 2)
    d <- sim_family_response(mgcv::mvn(d = 2), matrix(0, n, 2), list(covariance = V))
    expect_equal(unname(stats::cov(d$y)), V, tolerance = 0.04)
  })
})

test_that("censoring records the actual complete response and family encodings", {
  for (f in list(mgcv::cnorm(theta = 1), mgcv::clog(theta = 1),
      mgcv::cpois(), mgcv::bcg())) {
    key <- family_type(f)
    pars <- if (key == "bcg") list(lambda = 0, sigma = 1) else list()
    obs <- if (key == "cpois") list(left = 0.5, right = 2.5, interval_width = 2) else
      if (key == "bcg") list(left = 0.5, right = 2, interval_width = 0.2) else
        list(left = -0.5, right = 0.5, interval_width = 0.2)
    d <- data_sim(n = 300, seed = 6, family = f, parameters = pars,
      predictors = function(d) rep(0, nrow(d)), observation = obs)
    y <- attr(d, "truth")$response
    expect_equal(dim(d$y), c(300L, 2L))
    left <- y <= obs$left; right <- y >= obs$right
    expect_true(all(d$y[left, 1] == obs$left))
    expect_true(all(d$y[left, 2] == if (key == "bcg") 0 else -Inf))
    expect_true(all(d$y[right, 1] == obs$right))
    expect_true(all(d$y[right, 2] == Inf))
    middle <- !left & !right
    expect_true(all(d$y[middle, 1] <= y[middle] & d$y[middle, 2] >= y[middle]))
  }
  expect_error(data_sim(family = mgcv::cpois(), observation = list(left = 1)), "non-integer")
  expect_error(data_sim(family = stats::poisson(), observation = list(left = 1)), "censored families")
  expect_error(data_sim(family = mgcv::cnorm(theta = 1), observation = list(left = 2, right = 1)), "below")
})

test_that("Cox times use the chosen baseline and independent censoring", {
  d <- data_sim(n = 20000, seed = 19, family = mgcv::cox.ph(),
    predictors = function(d) rep(log(2), nrow(d)),
    parameters = list(baseline_shape = 2, baseline_scale = 3, censor_rate = 0.2))
  p <- attr(d, "truth")$parameters
  expect_equal(d$y, pmin(p$event_time, p$censor_time))
  expect_equal(d$status, as.integer(p$event_time <= p$censor_time))
  expect_lt(abs(mean((p$event_time / 3)^2 * 2) - 1), 0.03)
  expect_true(any(d$status == 0) && any(d$status == 1))
  expect_true(all(data_sim(n = 20, seed = 4, family = mgcv::cox.ph(),
    parameters = list(censor_rate = 0))$status == 1))
})

test_that("family-object groups support repeated families and component parameters", {
  f <- mgcv::gfam(list(stats::gaussian(), stats::gaussian(), stats::binomial()))
  d <- data_sim("gfam", n = 600, family = f, seed = 4,
    predictors = function(d) rep(0, nrow(d)),
    parameters = list(components = list(list(dispersion = 0.01), list(dispersion = 4), list(trials = 10))))
  expect_setequal(unique(d$index), 1:3)
  expect_lt(stats::var(d$y[d$index == 1]), 0.02)
  expect_gt(stats::var(d$y[d$index == 2]), 3)
  expect_true(all(d$trials[d$index == 3] == 10))
  expect_error(data_sim(family = f, observation = list(left = 1)), "matrix responses")
  expect_error(data_sim(family = f, parameters = list(components = list(list()))), "one parameter list")
  expect_error(data_sim(family = f, parameters = list(index = 4)), "index")
})

test_that("extreme-value and sinh-arcsinh adapters preserve the specified response laws", {
  withr::with_seed(459, {
    n <- 30000L
    f <- mgcv::gumbls()
    eta <- cbind(rep(2, n), rep(0, n))
    d <- sim_family_response(f, eta)
    beta <- exp(f$linfo[[2]]$linkinv(0))
    expect_equal(d$parameters$location, rep(2, n))
    expect_equal(d$parameters$mu, rep(2 + 0.577215664901533 * beta, n))
    expect_lt(abs(mean(d$y) - (2 + 0.577215664901533 * beta)), 0.03)
    u <- exp(-exp(-(d$y - 2) / beta))
    expect_equal(as.numeric(stats::quantile(u, c(0.1, 0.5, 0.9))), c(0.1, 0.5, 0.9), tolerance = 0.01)
    for (xi in c(0, 1e-10, -1.2, -0.2, 0.2, 0.8)) {
      f <- mgcv::gevlss(link = list("identity", "identity", "identity"))
      d <- sim_family_response(f, cbind(rep(2, n), rep(log(0.7), n), rep(xi, n)))
      z <- (d$y - 2) / 0.7
      log_hazard <- if (xi == 0) -z else -log1p(xi * z) / xi
      u <- exp(-exp(log_hazard))
      expect_equal(as.numeric(stats::quantile(u, c(0.1, 0.5, 0.9))), c(0.1, 0.5, 0.9), tolerance = 0.01)
    }
    f <- mgcv::shash()
    sigma <- 0.8; skewness <- 0.4; tail <- 1.2
    eta <- cbind(rep(2, n), rep(log(sigma - 0.01), n), rep(skewness, n), rep(log(tail), n))
    d <- sim_family_response(f, eta)
    z <- sinh(tail * asinh((d$y - 2) / (tail * sigma)) - skewness)
    expect_lt(abs(mean(z)), 0.025)
    expect_lt(abs(stats::var(z) - 1), 0.04)
  })
})

test_that("row-level generating parameters and custom links are honoured", {
  d <- data_sim(n = 120, seed = 2, family = stats::gaussian(link = "log"),
    predictors = function(d) rep(log(2), nrow(d)),
    parameters = list(dispersion = rep(c(0.01, 4), each = 60)))
  expect_equal(attr(d, "truth")$parameters$mu, rep(2, 120))
  expect_lt(stats::var(d$y[1:60]), 0.02)
  expect_gt(stats::var(d$y[61:120]), 2)
  d <- data_sim(n = 40, seed = 2, family = stats::binomial(link = "probit"),
    predictors = function(d) rep(0, nrow(d)), parameters = list(trials = rep(1:4, 10)))
  expect_equal(d$trials, rep(1:4, 10))
  expect_equal(d$y * d$trials, round(d$y * d$trials))
  expect_error(data_sim(n = 10, family = stats::gaussian(), parameters = list(dispersion = c(1, 2))), "one value or 10")
  f <- mgcv::gfam(list(mgcv::cnorm(theta = 1), mgcv::cpois()))
  expect_true(all(is.finite(data_sim(n = 30, family = f, seed = 2)$y)))
})

test_that("legacy vector controls retain recycling and affect the intended parameters", {
  for (dist in c("normal", "poisson", "binary", "negbin", "gamma", "ocat")) {
    d <- data_sim("eg1", n = 40, dist = dist, scale = c(0.1, 0.4), seed = 5,
      theta = c(2, 8))
    raw <- d$f0 + d$f1 + d$f2
    expected <- switch(dist, normal = raw, poisson =, negbin = raw * c(0.1, 0.4),
      binary = (raw - 5) * c(0.1, 0.4), gamma = raw / 3 + 0.1,
      ocat = raw - mean(raw))
    expect_equal(d$f, expected)
    expect_true(all(is.finite(d$y)))
  }
  d <- data_sim("gfam", n = 20, gfam_families = "normal", scale = 0,
    theta = NA, power = NA, seed = 3)
  expect_equal(d$y, d$f)
  d <- data_sim("gfam", n = 20, gfam_families = "normal", scale = c(0, 1), seed = 3)
  expect_equal(d$y[seq(1, 20, 2)], d$f[seq(1, 20, 2)])
  expect_equal(data_sim(n = 20, family = stats::gaussian(), seed = 3,
    parameters = list(dispersion = 0))$y, data_sim(n = 20, scale = 0, seed = 3)$y)
})

test_that("legacy Tweedie vector dispersion has the compound Poisson-Gamma moments", {
  withr::with_seed(194, {
    n <- 60000L
    scale <- rep(c(0.3, 1.1), each = n / 2)
    power <- rep(c(1.2, 1.8), each = n / 2)
    # A constant raw function isolates the sampling law from the template.
    d <- sim_tweedie(rep(3 * (log(2) - 0.1), n), scale = scale, power = power)
    for (rows in list(seq_len(n / 2), seq.int(n / 2 + 1, n))) {
      expect_lt(abs(mean(d$y[rows]) - 2), 0.05)
      expected <- scale[rows[1]] * 2^power[rows[1]]
      expect_lt(abs(stats::var(d$y[rows]) / expected - 1), 0.08)
    }
    expect_true(all(is.finite(data_sim(n = 40, dist = "tweedie", scale = c(0.3, 1.1),
      power = c(1.2, 1.8), seed = 9)$y)))
  })
})

test_that("GEV default predictors respect a supplied identity shape link", {
  f <- mgcv::gevlss(link = list("identity", "identity", "identity"))
  d <- data_sim(n = 20, family = f, seed = 2)
  expect_equal(attr(d, "truth")$parameters$shape, rep(0, 20))
})

test_that("negative binomial fitting ranges do not fix the generating size", {
  f <- mgcv::negbin(c(1, 10))
  expect_error(data_sim(n = 20, family = f, seed = 3), "Supply 'parameters\\$theta'")
  d <- data_sim(n = 20, family = f, parameters = list(theta = 4), seed = 3)
  expect_equal(attr(d, "truth")$parameters$theta, 4)
  expect_equal(d$y, data_sim(n = 20, family = mgcv::nb(theta = 4), seed = 3)$y)
})

test_that("infinite negative binomial size retains the Poisson limit", {
  d <- data_sim(n = 40, dist = "negbin", theta = Inf, scale = 0.1, seed = 2)
  expect_true(all(is.finite(d$y)))
  for (f in list(mgcv::nb(theta = Inf), mgcv::negbin(Inf))) {
    d <- data_sim(n = 30000, family = f, seed = 2,
      predictors = function(d) rep(log(2), nrow(d)))
    expect_equal(attr(d, "truth")$parameters$theta, Inf)
    expect_true(all(d$y >= 0 & d$y == floor(d$y)))
    expect_lt(abs(mean(d$y) - 2), 0.04)
    expect_lt(abs(stats::var(d$y) - 2), 0.06)
  }
})
