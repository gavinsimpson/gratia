# The density calculations here do not use gratia's response-distribution or
# likelihood helpers. They check normalising constants as well as predictors.
log_lik_values <- function(x) matrix(as.numeric(x), nrow = nrow(x))

test_that("ordinary likelihoods retain native weights, scale and offsets", {
  d <- log_lik_test_data()
  for (name in c("gaussian", "poisson", "Gamma", "inverse.gaussian")) {
    m <- log_lik_ordinary_fixture(name)
    f <- m$family
    d$y <- m$y
    B <- rbind(coef(m), coef(m) + c(.1, -.05))
    ll <- log_lik(m, method = "user", draws = B)
    expect_equal(dim(ll), c(2L, nrow(d)))
    expected <- t(vapply(seq_len(nrow(B)), function(j) {
      mu <- f$linkinv(B[j, 1] + B[j, 2] * d$x + d$off)
      phi <- m$sig2
      switch(name,
        gaussian = dnorm(d$y, mu, sqrt(phi / d$w), log = TRUE),
        poisson = d$w * dpois(d$y, mu, log = TRUE),
        Gamma = dgamma(d$y, shape = d$w / phi,
          scale = mu * phi / d$w, log = TRUE),
        inverse.gaussian = -.5 * log(2 * pi * (phi / d$w) * d$y^3) -
          (d$y - mu)^2 / (2 * (phi / d$w) * mu^2 * d$y))
    }, numeric(nrow(d))))
    expect_equal(log_lik_values(ll), unname(expected), info = name)
  }
})

test_that("binomial encodings use trials once and retain frequency weights", {
  d <- log_lik_test_data()
  d$trials <- rep(c(4, 8, 12), 24)
  d$success <- withr::with_seed(3, rbinom(nrow(d), d$trials, plogis(d$x)))
  pair <- mgcv::gam(cbind(success, trials - success) ~ x + offset(off),
    data = d, weights = w, family = binomial(), method = "REML")
  prop <- mgcv::gam(I(success / trials) ~ x + offset(off), data = d,
    weights = I(w * trials), family = binomial(), method = "REML")
  binary <- mgcv::gam(I(success > trials / 2) ~ x + offset(off), data = d,
    weights = w, family = binomial(), method = "REML")
  for (m in list(pair, prop, binary)) {
    B <- rbind(coef(m), coef(m) + c(.1, -.05))
    ll <- log_lik(m, method = "user", draws = B)
    expected <- t(vapply(seq_len(nrow(B)), function(j) {
      p <- plogis(B[j, 1] + B[j, 2] * d$x + d$off)
      if (identical(m, pair)) d$w * dbinom(d$success, d$trials, p, log = TRUE) else
        if (identical(m, prop)) dbinom(d$success * d$w, d$trials * d$w, p,
          log = TRUE) else d$w * dbinom(d$success > d$trials / 2, 1, p, log = TRUE)
    }, numeric(nrow(d))))
    expect_equal(log_lik_values(ll), unname(expected))
  }
})

test_that("extended likelihoods retain fitted nuisance parameters", {
  for (m in list(m_nb, m_betar, m_scat, m_ocat, m_tw)) {
    B <- rbind(coef(m), coef(m) + .002)
    ll <- log_lik(m, method = "user", draws = B)
    mu <- m$family$linkinv(predict(m, type = "lpmatrix") %*% t(B))
    y <- m$y
    wt <- m$prior.weights
    phi <- m$sig2
    theta <- m$family$getTheta(TRUE)
    expected <- t(vapply(seq_len(nrow(B)), function(j) {
      switch(tolower(sub("[ (].*", "", m$family$family)),
        negative = wt * dnbinom(y, size = theta, mu = mu[, j], log = TRUE),
        beta = wt * dbeta(y, mu[, j] * theta, (1 - mu[, j]) * theta, log = TRUE),
        scaled = wt * (dt((y - mu[, j]) / theta[2], df = theta[1], log = TRUE) -
          log(theta[2])),
        ordered = {
          cuts <- c(-Inf, theta, Inf)
          wt * log(plogis(cuts[y + 1] - mu[, j]) - plogis(cuts[y] - mu[, j]))
        },
        tweedie = wt * mgcv::ldTweedie(y, mu[, j], p = theta, phi = phi)[, 1])
    }, numeric(length(y))))
    expect_equal(log_lik_values(ll), unname(expected), info = m$family$family)
  }
})

test_that("general-family likelihoods agree with native full-data likelihoods", {
  for (m in list(m_gaulss, m_gammals, m_gumbls, m_gevlss,
      m_twlss, m_ziplss)) {
    B <- rbind(coef(m), coef(m) + .0001)
    ll <- log_lik(m, method = "user", draws = B)
    X <- predict(m, type = "lpmatrix")
    expected <- vapply(seq_len(nrow(B)), function(j)
      m$family$ll(m$y, X, B[j, ], wt = m$prior.weights,
        family = m$family, offset = m$offset, deriv = 0)$l, numeric(1))
    expect_equal(unname(rowSums(ll)), expected, info = m$family$family)
    expect_equal(ncol(ll), NROW(m$y))
  }
})

test_that("distributional likelihoods agree with normalized densities", {
  d <- log_lik_test_data()
  shash <- mgcv::gam(list(y ~ x, ~ 1, ~ 1, ~ 1), family = mgcv::shash(), data = d)
  for (name in c("gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
    m <- switch(name, gammals = m_gammals, gumbls = m_gumbls,
      gevlss = m_gevlss, shash = shash, twlss = m_twlss, ziplss = m_ziplss)
    B <- rbind(coef(m), coef(m) + .0001)
    # A substantial change in tail shape makes a retained shash penalty visible.
    if (name == "shash") B[2, ncol(B)] <- B[2, ncol(B)] + .5
    X <- predict(m, type = "lpmatrix")
    lpi <- attr(X, "lpi")
    expected <- t(vapply(seq_len(nrow(B)), function(j) {
      eta <- vapply(seq_along(lpi), function(k)
        drop(X[, lpi[[k]], drop = FALSE] %*% B[j, lpi[[k]]]), numeric(nrow(X)))
      pars <- vapply(seq_along(lpi), function(k)
        m$family$linfo[[k]]$linkinv(eta[, k]), numeric(nrow(X)))
      switch(name,
        gammals = dgamma(m$y, shape = exp(-pars[, 2]),
          scale = exp(pars[, 1] + pars[, 2]), log = TRUE),
        gumbls = {
          z <- (m$y - pars[, 1]) / exp(pars[, 2])
          -pars[, 2] - z - exp(-z)
        },
        gevlss = {
          z <- (m$y - pars[, 1]) / exp(pars[, 2])
          xi <- pars[, 3]
          term <- 1 + xi * z
          -pars[, 2] - (1 + 1 / xi) * log(term) - term^(-1 / xi)
        },
        shash = {
          z <- (m$y - pars[, 1]) / (exp(pars[, 2]) * exp(pars[, 4]))
          v <- exp(pars[, 4]) * asinh(z) - pars[, 3]
          dnorm(sinh(v), log = TRUE) + log(cosh(v)) -
            pars[, 2] - .5 * log1p(z^2)
        },
        twlss = {
          a <- environment(m$family$ll)$.a
          b <- environment(m$family$ll)$.b
          power <- a + (b - a) * plogis(eta[, 2])
          mgcv::ldTweedie(m$y, pars[, 1], p = power, phi = exp(eta[, 3]))[, 1]
        },
        ziplss = {
          lambda <- exp(eta[, 1])
          intensity <- exp(eta[, 2])
          ifelse(m$y == 0, -intensity, log(-expm1(-intensity)) +
            dpois(m$y, lambda, log = TRUE) - log(-expm1(-lambda)))
        })
    }, numeric(nrow(X))))
    expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)),
      unname(expected), info = name)
  }
})

test_that("single-predictor hurdle Poisson likelihoods include truncation constants", {
  d <- log_lik_test_data()
  d$y <- rep(c(0, 0, 1, 2, 1, 3), 12)
  m <- mgcv::gam(y ~ x + offset(off), data = d,
    weights = w, family = mgcv::ziP(theta = c(.3, .5), b = .2), method = "REML")
  B <- rbind(coef(m), coef(m) + c(.1, -.05))
  theta <- m$family$getTheta(TRUE)
  expected <- t(vapply(seq_len(nrow(B)), function(j) {
    eta <- B[j, 1] + B[j, 2] * d$x + d$off
    lambda <- exp(eta)
    presence <- exp(theta[1] + theta[2] * eta)
    ifelse(d$y == 0, -presence, log(-expm1(-presence)) +
      dpois(d$y, lambda, log = TRUE) - log(-expm1(-lambda)))
  }, numeric(nrow(d))))
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)),
    unname(expected))
})

test_that("ordinal and hurdle Poisson log masses retain extreme predictor tails", {
  ordinal <- mgcv::ocat(R = 4)
  cuts <- ordinal$getTheta(TRUE)
  expected <- c(plogis(cuts[1] - 1000, log.p = TRUE),
    plogis(cuts[3] + 1000, lower.tail = FALSE, log.p = TRUE))
  actual <- posterior_likelihood_family(ordinal, c(1, 4),
    matrix(c(1000, -1000), ncol = 1L), c(1, 1), 1)
  expect_equal(actual, expected)
  # In this tail, an interior category's log probability is a difference
  # between two exponential tails, rather than two rounded probabilities.
  interior <- cuts[2] - 1000 + log1p(-exp(cuts[1] - cuts[2]))
  expect_equal(posterior_likelihood_family(ordinal, 2,
    matrix(1000, 1L), 1, 1), interior)
  hurdle <- mgcv::ziplss()
  eta <- rbind(c(-1000, 0), c(0, -1000), c(-1000, -1000))
  actual <- posterior_likelihood_family(hurdle, rep(1, 3), eta, rep(1, 3), 1)
  expected <- c(log(-expm1(-1)),
    -1000 - 1 - log(-expm1(-1)), -1000)
  expect_true(all(is.finite(actual)))
  expect_equal(actual, expected)
})

test_that("GEV observations outside a draw's support have zero density", {
  m <- m_gevlss
  lpi <- attr(predict(m, type = "lpmatrix"), "lpi")
  B <- matrix(0, 1L, length(coef(m)))
  B[1, lpi[[1]][1]] <- -1e5
  # With the default shape link, a zero predictor means xi = -0.25;
  # this draw's upper endpoint lies below every observed response.
  ll <- log_lik(m, method = "user", draws = B)
  expect_true(all(ll == -Inf))
  expect_false(anyNA(ll))
})

test_that("fixed-power Tweedie likelihoods retain fitted dispersion and weights", {
  d <- log_lik_test_data()
  d$y <- withr::with_seed(9, mgcv::rTweedie(exp(.5 + d$x), p = 1.5, phi = .7))
  m <- mgcv::gam(y ~ x + offset(off), data = d, weights = w,
    family = mgcv::Tweedie(p = 1.5), method = "REML")
  B <- rbind(coef(m), coef(m) + c(.1, -.05))
  native <- vapply(seq_len(nrow(B)), function(j) {
    mu <- exp(B[j, 1] + B[j, 2] * d$x + d$off)
    mgcv::fix.family.ls(m$family)$ls(m$y, m$prior.weights, length(m$y), m$sig2)[1] -
      sum(m$family$dev.resids(m$y, mu, m$prior.weights)) / (2 * m$sig2)
  }, numeric(1))
  expect_equal(unname(rowSums(log_lik(m, method = "user", draws = B))), native)
  expected <- t(vapply(seq_len(nrow(B)), function(j) {
    mu <- exp(B[j, 1] + B[j, 2] * d$x + d$off)
    d$w * mgcv::ldTweedie(d$y, mu, p = 1.5, phi = m$sig2)[, 1]
  }, numeric(nrow(d))))
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)),
    unname(expected))
})

test_that("extended-family weights multiply normalized log densities", {
  d <- log_lik_test_data()
  for (name in c("nb", "betar", "scat")) {
    f <- switch(name, nb = mgcv::nb(theta = 3),
      betar = mgcv::betar(theta = 8), scat = mgcv::scat(theta = c(5, .7)))
    d$y <- switch(name,
      nb = withr::with_seed(6, rnbinom(nrow(d), size = 3, mu = exp(d$x))),
      betar = withr::with_seed(7, rbeta(nrow(d), 3, 5)),
      scat = withr::with_seed(8, .5 * d$x + .7 * rt(nrow(d), 5)))
    m <- mgcv::gam(y ~ x + offset(off), data = d, weights = w,
      family = f, method = "REML")
    B <- rbind(coef(m), coef(m) + c(.1, -.05))
    expected <- t(vapply(seq_len(nrow(B)), function(j) {
      mu <- f$linkinv(B[j, 1] + B[j, 2] * d$x + d$off)
      d$w * switch(name,
        nb = dnbinom(d$y, size = 3, mu = mu, log = TRUE),
        betar = dbeta(d$y, mu * 8, (1 - mu) * 8, log = TRUE),
        scat = dt((d$y - mu) / .7, df = 5, log = TRUE) - log(.7))
    }, numeric(nrow(d))))
    expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)),
      unname(expected), info = name)
  }
})

test_that("multivariate normal likelihoods retain joint response covariance", {
  m <- m_mvn
  B <- rbind(coef(m), coef(m) + .002)
  X <- predict(m, type = "lpmatrix")
  lpi <- attr(X, "lpi")
  expected <- t(vapply(seq_len(nrow(B)), function(j) {
    mu <- vapply(lpi, function(k) drop(X[, k, drop = FALSE] %*% B[j, k]),
      numeric(nrow(X)))
    theta <- tail(B[j, ], 3)
    R <- matrix(c(exp(theta[1]), 0, theta[2], exp(theta[3])), 2)
    residual <- (m$y - mu) %*% t(R)
    sum(log(diag(R))) - log(2 * pi) - rowSums(residual^2) / 2
  }, numeric(nrow(X))))
  actual <- log_lik(m, method = "user", draws = B)
  expect_equal(dim(actual), c(2L, nrow(m$y)))
  expect_equal(log_lik_values(actual), unname(expected))
})

test_that("multinomial likelihoods select observed-category probabilities", {
  d <- log_lik_test_data()
  d$z <- rev(d$x)^2
  d$off2 <- -.2 + .1 * d$x
  d$y <- rep(0:2, 24)
  m <- mgcv::gam(list(y ~ x + offset(off), ~ z + offset(off2)),
    data = d, family = mgcv::multinom(K = 2))
  B <- rbind(coef(m), coef(m) + c(.15, -.08, -.2, .12))
  expected <- t(vapply(seq_len(nrow(B)), function(j) {
    eta <- cbind(0, B[j, 1] + B[j, 2] * d$x + d$off,
      B[j, 3] + B[j, 4] * d$z + d$off2)
    eta[cbind(seq_len(nrow(d)), d$y + 1)] - log(rowSums(exp(eta)))
  }, numeric(nrow(d))))
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)), unname(expected))
})

test_that("Gaussian location-scale likelihoods respect native precision links", {
  m <- m_gaulss
  B <- rbind(coef(m), coef(m) + .002)
  X <- predict(m, type = "lpmatrix")
  lpi <- attr(X, "lpi")
  expected <- t(vapply(seq_len(nrow(B)), function(j) {
    mu <- m$family$linfo[[1]]$linkinv(drop(X[, lpi[[1]], drop = FALSE] %*%
      B[j, lpi[[1]]]))
    tau <- m$family$linfo[[2]]$linkinv(drop(X[, lpi[[2]], drop = FALSE] %*%
      B[j, lpi[[2]]]))
    dnorm(m$y, mu, 1 / tau, log = TRUE)
  }, numeric(length(m$y))))
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)), unname(expected))
})

test_that("grouped likelihoods use each component's native weights and scale", {
  m <- gfam_model
  B <- rbind(coef(m), coef(m) + .002)
  index <- attr(m$family$getfl(), "fi")
  X <- predict(m, type = "lpmatrix")
  expected <- t(vapply(seq_len(nrow(B)), function(j) {
    eta <- drop(X %*% B[j, ]) + m$offset
    ll <- numeric(length(m$y))
    i <- which(index == 1L)
    ll[i] <- dbinom(m$y[i] * m$prior.weights[i], m$prior.weights[i],
      plogis(eta[i]), log = TRUE)
    i <- which(index == 2L)
    ll[i] <- dnorm(m$y[i], eta[i],
      sqrt(exp(m$family$getTheta()[1]) / m$prior.weights[i]), log = TRUE)
    i <- which(index == 3L)
    ll[i] <- m$prior.weights[i] * dpois(m$y[i], exp(eta[i]), log = TRUE)
    ll
  }, numeric(length(m$y))))
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)), unname(expected))
})

test_that("fitting rows and transformed responses retain their identity", {
  d <- log_lik_test_data()
  d$y <- exp(d$y)
  d$x[c(4, 9)] <- NA_real_
  rownames(d) <- paste0("observation-", seq_len(nrow(d)))
  m <- mgcv::gam(log(y) ~ x + offset(off), data = d, subset = seq_len(60),
    na.action = na.exclude, method = "REML")
  B <- matrix(coef(m), 1L)
  ll <- log_lik(m, method = "user", draws = B)
  used <- setdiff(seq_len(60), c(4, 9))
  expected <- dnorm(log(d$y[used]), B[1, 1] + B[1, 2] * d$x[used] +
    d$off[used], sqrt(m$sig2), log = TRUE)
  expect_equal(as.numeric(ll), expected)
  expect_equal(ncol(ll), length(used))
  expect_identical(colnames(ll), rownames(m$model))
})

test_that("seeded likelihood extraction restores RNG and preserves scalar logLik", {
  m <- m_tiny_eg1
  before_loglik <- stats::logLik(m)
  withr::local_seed(58)
  before <- .Random.seed
  a <- log_lik(m, n_draws = 4, seed = 42)
  expect_identical(.Random.seed, before)
  expect_identical(log_lik(m, n_draws = 4, seed = 42), a)
  expect_identical(stats::logLik(m), before_loglik)
  expect_equal(dim(a), c(4L, length(m$y)))
})

test_that("unsupported observation likelihoods and invalid draws fail clearly", {
  d <- log_lik_test_data()
  d$y <- abs(round(d$y))
  q <- mgcv::gam(y ~ x, data = d, family = quasipoisson())
  expect_error(log_lik(q, n_draws = 2), "[Qq]uasi|[Ll]ikelihood")
  ar <- mgcv::bam(y ~ s(x, k = 5), data = d, rho = .3)
  expect_error(log_lik(ar, n_draws = 2), "correlat|independent|AR")
  expect_error(log_lik(m_gamm, n_draws = 2), "gamm|[Ll]ikelihood|supported")
  for (B in list(matrix(0, 2, 1), matrix(NA_real_, 2, length(coef(m_tiny_eg1))),
      matrix(Inf, 2, length(coef(m_tiny_eg1))))) {
    expect_error(log_lik(m_tiny_eg1, method = "user", draws = B))
  }
})

test_that("family likelihood hooks receive each draw's parameter values", {
  d <- log_lik_test_data()
  m <- mgcv::gam(y ~ x + offset(off), data = d, method = "REML")
  m$family$family <- "custom Gaussian likelihood"
  m$family$gratia_diagnostics <- function(mu, wt, scale, y, eta) {
    list(log_lik = function(y) dnorm(y, mu, sqrt(scale / wt), log = TRUE))
  }
  B <- rbind(coef(m), coef(m) + c(.1, -.05))
  expected <- t(vapply(seq_len(nrow(B)), function(j)
    dnorm(d$y, B[j, 1] + B[j, 2] * d$x + d$off, sqrt(m$sig2), log = TRUE),
    numeric(nrow(d))))
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)), unname(expected))
  for (bad in list(function(y) NA_real_, function(y) rep(Inf, length(y)),
      function(y) matrix(0, length(y)), function(y) rep(0, length(y) + 1L))) {
    m$family$gratia_diagnostics <- local({
      fun <- bad
      function(mu, wt, scale, y, eta) list(log_lik = fun)
    })
    expect_error(log_lik(m, method = "user", draws = B), "log_lik|likelihood")
  }
})

test_that("grouped likelihood hooks receive their own fitted rows and scales", {
  m <- gfam_model
  families <- m$family$getfl()
  families[[2]]$gratia_diagnostics <- function(mu, wt, scale, y, eta) {
    list(log_lik = function(y) dnorm(y, mu, 2 * sqrt(scale / wt), log = TRUE))
  }
  m$family$getfl <- function() families
  B <- rbind(coef(m), coef(m) + .002)
  expected <- log_lik_values(log_lik(gfam_model, method = "user", draws = B))
  rows <- which(attr(families, "fi") == 2L)
  X <- predict(m, type = "lpmatrix")
  for (j in seq_len(nrow(B))) {
    mu <- drop(X[rows, , drop = FALSE] %*% B[j, ]) + m$offset[rows]
    expected[j, rows] <- dnorm(m$y[rows], mu,
      2 * sqrt(exp(m$family$getTheta()[1]) / m$prior.weights[rows]), log = TRUE)
  }
  expect_equal(log_lik_values(log_lik(m, method = "user", draws = B)), expected)
})

test_that("zero fitting weights and zero binomial trials contribute zero", {
  d <- log_lik_test_data()
  d$w[1] <- 0
  m <- mgcv::gam(y ~ x, data = d, weights = w, method = "REML")
  expect_equal(as.numeric(log_lik(m, method = "user",
    draws = matrix(coef(m), 1))[, 1]), 0)
  d$trials <- rep(c(0, 4, 8), 24)
  d$success <- withr::with_seed(4, rbinom(nrow(d), d$trials, plogis(d$x)))
  m <- mgcv::gam(cbind(success, trials - success) ~ x, data = d,
    family = binomial(), method = "REML")
  ll <- log_lik(m, method = "user", draws = matrix(coef(m), 1))
  expect_true(all(is.finite(ll)))
  expect_equal(as.numeric(ll[, d$trials == 0]), rep(0, sum(d$trials == 0)))
})

test_that("one-observation callbacks agree with matrix extraction for every family", {
  d <- log_lik_test_data()
  shash <- mgcv::gam(list(y ~ x, ~ 1, ~ 1, ~ 1), data = d,
    family = mgcv::shash())
  d$y <- exp(d$y)
  positive <- lapply(list(Gamma(link = "log"), inverse.gaussian(link = "log"),
      mgcv::Tweedie(p = 1.5)), function(f)
    mgcv::gam(y ~ x + offset(off), data = d, family = f, method = "REML"))
  d$y <- rep(c(0, 0, 1, 2, 1, 3), 12)
  zip <- mgcv::gam(y ~ x, data = d,
    family = mgcv::ziP(theta = c(.3, .5), b = .2))
  binomial_fit <- mgcv::gam(cbind(y, 4 - y) ~ x + offset(off), data = d,
    family = binomial(), weights = w, method = "REML")
  models <- c(list(m_tiny_eg1, b_pois, m_nb, m_negbin, m_betar, m_scat, m_ocat,
    m_tw, m_gaulss, m_gammals, m_gumbls, m_gevlss, shash, m_twlss, m_ziplss,
    m_multinom, m_mvn, gfam_model, gfam_extended_model, gfam_ordinal_model,
    gfam_gamma_model, zip, binomial_fit), positive)
  for (m in models) {
    B <- rbind(coef(m), coef(m) + .0001)
    prep <- posterior_likelihood_prepare(m,
      sampling_args = list(method = "user", draws = B))
    full <- posterior_likelihood_matrix(prep)
    for (i in unique(c(1L, 2L, ncol(full)))) {
      single <- tryCatch(posterior_likelihood_evaluate(prep, i), error = identity)
      expect_false(inherits(single, "error"), info = paste(m$family$family,
        if (inherits(single, "error")) conditionMessage(single) else ""))
      if (inherits(single, "error")) next
      expect_equal(dim(single), c(2L, 1L), info = m$family$family)
      expect_equal(as.numeric(single), as.numeric(full[, i]), info = m$family$family)
    }
  }
})

test_that("Gamma and inverse Gaussian likelihoods agree with native totals", {
  for (unit_weights in c(FALSE, TRUE)) {
    for (name in c("Gamma", "inverse.gaussian")) {
      m <- log_lik_ordinary_fixture(name, unit_weights)
      B <- rbind(coef(m), coef(m) + c(.1, -.05))
      ll <- log_lik(m, method = "user", draws = B)
      X <- predict(m, type = "lpmatrix")
      for (j in seq_len(nrow(B))) {
        mu <- m$family$linkinv(drop(X %*% B[j, ]) + m$offset)
        native <- mgcv::fix.family.ls(m$family)$ls(m$y, m$prior.weights,
          length(m$y), m$sig2)[1] -
          sum(m$family$dev.resids(m$y, mu, m$prior.weights)) / (2 * m$sig2)
        expect_equal(sum(ll[j, ]), unname(native), info = name)
        density <- if (name == "Gamma") {
          dgamma(m$y, shape = m$prior.weights / m$sig2,
            scale = mu * m$sig2 / m$prior.weights, log = TRUE)
        } else {
          dnorm((m$y - mu) / (mu * sqrt(m$y * m$sig2 / m$prior.weights)),
            log = TRUE) - .5 * log(m$sig2 * m$y^3 / m$prior.weights)
        }
        expect_equal(as.numeric(ll[j, ]), unname(density), info = name)
      }
    }
  }
})

test_that("grouped Gamma and inverse Gaussian likelihoods retain their densities", {
  for (unit_weights in c(FALSE, TRUE)) {
    m <- log_lik_grouped_fixture(unit_weights)
    B <- rbind(coef(m), coef(m) + c(.1, -.03, .05))
    ll <- log_lik(m, method = "user", draws = B)
    X <- predict(m, type = "lpmatrix")
    families <- m$family$getfl()
    index <- attr(families, "fi")
    # This fixture has two ordinary families, each with an estimated log scale.
    scales <- exp(m$family$getTheta())
    for (j in seq_len(nrow(B))) {
      eta <- drop(X %*% B[j, ]) + m$offset
      for (i in seq_along(families)) {
        rows <- which(index == i)
        f <- families[[i]]
        y <- m$y[rows]
        wt <- m$prior.weights[rows]
        mu <- f$linkinv(eta[rows])
        phi <- scales[i]
        native <- mgcv::fix.family.ls(f)$ls(y, wt, length(y), phi)[1] -
          sum(f$dev.resids(y, mu, wt)) / (2 * phi)
        expect_equal(sum(ll[j, rows]), unname(native), info = f$family)
        density <- if (i == 1L) dgamma(y, shape = wt / phi,
          scale = mu * phi / wt, log = TRUE) else
          dnorm((y - mu) / (mu * sqrt(y * phi / wt)), log = TRUE) -
            .5 * log(phi * y^3 / wt)
        expect_equal(as.numeric(ll[j, rows]), unname(density), info = f$family)
      }
    }
  }
})

test_that("zero weights omit Gamma and inverse Gaussian observations", {
  for (f in list(Gamma(link = "log"), inverse.gaussian(link = "log"))) {
    # Omitted rows must not evaluate a density at undefined response values.
    expect_equal(posterior_likelihood_family(f, c(NA_real_, 1),
      matrix(c(NA_real_, 0), ncol = 1L), c(0, 2), .5),
      c(0, if (f$family == "Gamma") dgamma(1, 4, scale = .25, log = TRUE) else
        dnorm(0, log = TRUE) - .5 * log(.25)))
  }
})
