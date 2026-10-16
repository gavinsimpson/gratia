test_that("fix family cdf is null if we can not fix it", {
  expect_null(fix_family_cdf(quasipoisson())$cdf)
})

test_that("fix family qf is null if we can not fix it", {
  expect_null(fix_family_qf(quasipoisson())$qf)
})

test_that("fix family rd is null if we can not fix it", {
  expect_null(fix_family_rd(quasipoisson())$rd)
})

test_that("fix family cdf works for a GAM", {
  expect_equal(
    fix_family_cdf(gaussian())$cdf, cdf_gaussian
  )
})

test_that("MVN family functions use each response's marginal variance", {
  fam <- mgcv::mvn(d = 2)
  V <- matrix(c(4, 1, 1, 9), 2)
  fam$data$R <- chol(solve(V))
  fam <- fix_family_qf(fix_family_cdf(fam))
  mu <- cbind(c(-2, 0, 3), c(1, 5, -1))
  z <- matrix(c(-2, 0, 1, 2, -1, 3), 3)
  y <- mu + sweep(z, 2, c(2, 3), "*")

  # Nonzero covariance makes marginal variances differ from conditional ones.
  # Prior weights and a separate dispersion do not rescale mgcv's MVN family.
  for (lower in c(TRUE, FALSE)) {
    for (log_p in c(TRUE, FALSE)) {
      p <- pnorm(z, lower.tail = lower, log.p = log_p)
      expect_equal(fam$cdf(y, mu, wt = c(.5, 2, 4), scale = 17,
        log_p = log_p, lower_tail = lower), p)
      expect_equal(fam$qf(p, mu, wt = c(.5, 2, 4), scale = 17,
        log_p = log_p, lower_tail = lower), y)
    }
  }

  # The factories retain the covariance supplied when the family was fixed.
  fam$data$R <- diag(2)
  expect_equal(fam$cdf(y, mu, 1, 1), pnorm(z))
  expect_equal(fam$qf(pnorm(z), mu, 1, 1), y)
  expect_identical(fix_family_cdf(fam)$cdf, fam$cdf)
  expect_identical(fix_family_qf(fam)$qf, fam$qf)
})

test_that("MVN family functions recycle probabilities and retain extreme tails", {
  fam <- mgcv::mvn(d = 2)
  fam$data$R <- diag(c(.5, 1 / 3))
  fam <- fix_family_qf(fix_family_cdf(fam))
  mu <- cbind(c(-2, 0, 3), c(1, 5, -1))
  p <- c(.1, .5, .9)
  expected <- mu + outer(qnorm(p), c(2, 3))
  expect_equal(fam$qf(p, mu, 1, 1), expected)
  expect_equal(fam$cdf(expected, mu, 1, 1), unname(cbind(p, p)))
  expect_equal(fam$cdf(0, mu, 1, 1), pnorm(-sweep(mu, 2, c(2, 3), "/")))
  expect_equal(fam$qf(.5, mu, 1, 1), mu)
  one <- mu[1, , drop = FALSE]
  expect_equal(fam$qf(.5, one, 1, 1), one)
  expect_equal(fam$qf(0, mu, 1, 1), matrix(-Inf, 3, 2))
  expect_equal(fam$qf(1, mu, 1, 1), matrix(Inf, 3, 2))
  expect_equal(fam$cdf(-Inf, mu, 1, 1), matrix(0, 3, 2))
  expect_equal(fam$cdf(Inf, mu, 1, 1), matrix(1, 3, 2))
  for (lower in c(TRUE, FALSE)) {
    q <- fam$qf(-1000, mu, 1, 1, log_p = TRUE, lower_tail = lower)
    expect_true(all(is.finite(q)))
    expect_equal(fam$cdf(q, mu, 1, 1, log_p = TRUE, lower_tail = lower),
      matrix(-1000, 3, 2))
  }
})

test_that("MVN fixes preserve supplied functions without needing covariance", {
  fam <- mgcv::mvn(d = 2)
  fam$cdf <- function(q, mu, wt, scale) q * 0 + .25
  fam$qf <- function(p, mu, wt, scale) mu + 7
  expect_identical(fix_family_cdf(fam), fam)
  expect_identical(fix_family_qf(fam), fam)
})

test_that("MVN family functions work with a fitted model's covariance", {
  fam <- fix_family_qf(fix_family_cdf(family(m_mvn)))
  mu <- fitted(m_mvn)
  sigma <- sqrt(diag(solve(crossprod(fam$data$R))))
  z <- sweep(m_mvn$y - mu, 2, sigma, "/")
  p <- fam$cdf(m_mvn$y, mu, m_mvn$prior.weights, m_mvn$sig2)
  expect_equal(p, pnorm(z))
  expect_equal(fam$qf(p, mu, m_mvn$prior.weights, m_mvn$sig2),
    m_mvn$y, ignore_attr = TRUE)
})

test_that("fix family funs work for gaussian", {
  q     <- c(-1, 3, 2.3)
  mu    <- c(0, 2, 1.5)
  wt    <- rep(1, 3)
  scale <- c(1, 2, 1)

  cdf <- cdf_gaussian(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(family(m_gam))$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for poisson", {
  q     <- c(0, 1, 3)
  mu    <- c(0.1, 1.3, 3.5)
  wt    <- rep(1, 3)
  scale <- c(1, 1, 1)

  cdf <- cdf_poisson(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(poisson())$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for binomial", {
  q     <- c(0.1, 0.7, 0.8)
  mu    <- c(0.1, 0.5, 0.7)
  wt    <- rep(10, 3)
  scale <- c(1, 1, 1)

  cdf <- cdf_binomial(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(binomial())$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for gamma", {
  q     <- c(1.1, 3.2, 2.3)
  mu    <- c(2, 2, 1.5)
  wt    <- rep(10, 3)
  scale <- c(1, 2, 1)

  cdf <- cdf_gamma(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(Gamma())$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for gaulss", {
  q     <- c(1.1, 3.2, 2.3, 1.3, 4.5, 2.2)
  mu    <- head(fitted(m_gaulss))
  cdf <- cdf_gaulss(q = q, mu = mu)

  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(gaulss())$qf
  qf <- qf_fun(cdf, mu = mu)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for gammals", {
  q     <- c(1.1, 3.2, 2.3, 1.3, 4.5, 2.2)
  mu    <- head(fitted(m_gammals))
  cdf <- cdf_gammals(q = q, mu = mu)

  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(gammals())$qf
  qf <- qf_fun(cdf, mu = mu)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for gumbls", {
  q     <- c(1.1, 3.2, 2.3, 1.3, 4.5, 2.2)
  mu    <- head(fitted(m_gumbls))
  cdf <- cdf_gumbls(q = q, mu = mu)

  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(gumbls())$qf
  qf <- qf_fun(cdf, mu = mu)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for ziplss", {
  q     <- c(1.1, 3, 2, 5, 4, 10)
  mu    <- head(fitted(m_ziplss))
  cdf <- cdf_ziplss(q = q, mu = mu)

  qf_fun <- fix_family_qf(ziplss())$qf
  # Test inside each CDF jump; roundoff at the jump itself can select either
  # neighbouring count even for otherwise accurate discrete quantiles.
  lower <- cdf_ziplss(q = floor(q) - 1, mu = mu)
  qf <- qf_fun((lower + cdf) / 2, mu = mu)
  expect_equal(qf, floor(q))
})

test_that("fix family funs work for gevlss", {
  q     <- c(1.1, 3.2, 2.3, 1.3, 4.5, 2.2)
  mu    <- head(fitted(m_gevlss))
  cdf <- cdf_gevlss(q = q, mu = mu)

  qf_fun <- fix_family_qf(gevlss())$qf
  qf <- qf_fun(cdf, mu = mu)
  expect_equal(q, qf)
})

test_that("fix family funs work for scat", {
  q     <- c(1.1, 3.2, 2.3)
  mu    <- c(2, 2, 1.5)
  wt    <- rep(1, 3)
  scale <- c(1, 2, 1)
  fam <- family(m_scat) |> fix_family_cdf()

  cdf <- fam$cdf(q = q, mu = mu, wt = wt, scale = scale)
  pars <- theta(fam)
  expect_equal(cdf, pt((q - mu) / pars[2], df = pars[1]))
  qf_fun <- fix_family_qf(fam)$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_equal(q, qf)
})

test_that("fix family funs work for nb", {
  q     <- c(1, 3, 4)
  mu    <- c(2, 2, 1.5)
  wt    <- rep(1, 3)
  scale <- c(1, 2, 1)
  fam <- family(m_nb) |> fix_family_cdf()

  cdf <- fam$cdf(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(fam)$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for tw", {
  q     <- c(1.1, 3.2, 2.3)
  mu    <- c(2, 2, 1.5)
  wt    <- rep(1, 3)
  scale <- c(1, 2, 1)
  fam <- family(m_tw) |> fix_family_cdf()

  cdf <- fam$cdf(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(fam)$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})

test_that("fix family funs work for betar", {
  q     <- c(0.2, 0.5, 0.9)
  mu    <- c(0.1, 0.4, 0.7)
  wt    <- rep(1, 3)
  scale <- c(1, 2, 1)
  fam <- family(m_betar) |> fix_family_cdf()

  cdf <- fam$cdf(q = q, mu = mu, wt = wt, scale = scale)
  expect_snapshot(
    cdf |> round(4)
  )
  qf_fun <- fix_family_qf(fam)$qf
  qf <- qf_fun(cdf, mu = mu, wt = wt, scale = scale)
  expect_snapshot(round(qf, 4))
  expect_equal(q, qf)
})
