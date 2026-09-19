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
 