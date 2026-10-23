test_that("total term variance includes covariance and paired posterior uncertainty", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$y <- 2 * d$x + 2 * d$z + sin(6 * d$w) + (d$fac == "b") +
    d$off + rnorm(nrow(d), sd = 0.3)
  for (fit in list(gam, bam)) {
    m <- fit(y ~ x + z + fac * poly(w, 2) + s(w, k = 6) + offset(off),
      data = d, method = "REML")
    got <- variance_comp(m, type = "terms", n_draws = 137, seed = 83,
      coverage = 0.8)
    draws <- attr(got, "variance_draws")
    values <- predict(m, type = "terms")
    i <- match("total", got$.component)
    expect_equal(i, nrow(got))
    expect_equal(got$.variance[i], var(rowSums(values)))
    expect_equal(got$.variance[i], sum(attr(got, "covariance")))
    expect_gt(abs(got$.variance[i] - sum(got$.variance[-i])), 0.1)
    expected <- reference_term_variance_draws(m, 137, 83)
    expect_equal(unname(draws[, i]), expected[, i])
    expect_gt(max(abs(draws[, i] - rowSums(draws[, -i, drop = FALSE]))), 0.1)
    bounds <- quantile(sqrt(expected[, i]), c(0.1, 0.9), names = FALSE)
    expect_equal(c(got$.lower_ci[i], got$.upper_ci[i]), bounds)
    expect_equal(colnames(draws), got$.component)
    expect_equal(rownames(attr(got, "covariance")), got$.component[-i])
    no_draws <- variance_comp(m, type = "terms", n_draws = 0)
    expect_equal(no_draws$.variance, got$.variance)
    expect_true(all(is.na(no_draws$.lower_ci)))
    expect_null(attr(no_draws, "variance_draws"))
  }
})

test_that("negative covariance reduces the total in the fitted values and draws", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$z <- d$x + runif(nrow(d), -0.1, 0.1)
  d$y <- 5 * d$x - 5 * d$z + rnorm(nrow(d), sd = 0.05)
  m <- gam(y ~ x + z, data = d)
  got <- variance_comp(m, type = "terms", n_draws = 30, seed = 83)
  covariance <- attr(got, "covariance")
  expect_lt(covariance[1, 2], -0.1)
  expect_lt(got$.variance[3], 0.2 * sum(got$.variance[1:2]))
  draws <- attr(got, "variance_draws")
  expect_true(all(draws[, "total"] < 0.2 * rowSums(draws[, 1:2])))
  expect_equal(draws, reference_term_variance_draws(m, 30, 83), ignore_attr = TRUE)
})

test_that("a single term and an intercept-only predictor have consistent totals", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- gam(y ~ x, data = d)
  got <- variance_comp(m, type = "terms", n_draws = 20, seed = 83)
  expect_equal(got$.variance[1], got$.variance[2])
  expect_equal(got$.lower_ci[1], got$.lower_ci[2])
  expect_equal(got$.upper_ci[1], got$.upper_ci[2])
  expect_equal(attr(got, "variance_draws")[, 1], attr(got, "variance_draws")[, 2])
  m <- gam(y ~ 1 + offset(off), data = d)
  state <- .Random.seed
  got <- variance_comp(m, type = "terms", n_draws = 20, seed = 83)
  expect_identical(.Random.seed, state)
  expect_equal(got$.component, "total")
  expect_equal(got$.variance, 0)
  expect_equal(got$.lower_ci, 0)
  expect_equal(got$.upper_ci, 0)
  expect_equal(dim(attr(got, "covariance")), c(0L, 0L))
  expect_equal(unname(attr(got, "variance_draws")), matrix(0, 20, 1))
})

test_that("total labels remain distinct from formula term names", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$total <- d$x
  m <- gam(y ~ total + s(w, k = 6), data = d)
  got <- variance_comp(m, type = "terms", n_draws = 20, seed = 83)
  expect_equal(got$.component, c("total.1", "s(w)", "total"))
  expect_equal(colnames(attr(got, "variance_draws")), got$.component)
  expect_equal(rownames(attr(got, "covariance")), c("total.1", "s(w)"))
  expect_equal(tail(got$.variance, 1), sum(attr(got, "covariance")))
})

test_that("distributional GAMs have one total per predictor, including shared terms", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  models <- list(
    gam(list(y ~ x + z + offset(off), ~1), data = d, family = gaulss()),
    gam(list(y ~ x + s(w, k = 6), ~s(z, k = 6)), data = d, family = gaulss()),
    gam(list(y ~1, ~1, 1 + 2 ~ 0 + s(w, k = 6)), data = d, family = gaulss()),
    gam(list(y ~1, ~1, 1 + 2 ~ 0 + x), data = d, family = gaulss())
  )
  for (m in models) {
    got <- variance_comp(m, type = "terms", n_draws = 50, seed = 83,
      coverage = 0.8)
    i <- (nrow(got) - 1L):nrow(got)
    expect_equal(got$.component[i], c("total", "total.1"))
    link <- predict(m, type = "link")
    expected <- reference_term_variance_draws(m, 50, 83)
    expect_equal(attr(got, "variance_draws"), expected, ignore_attr = TRUE)
    for (j in 1:2) {
      expect_equal(got$.variance[i[j]], var(link[, j] - (m$offset[j][[1L]] %||% 0)))
      bounds <- quantile(sqrt(expected[, i[j]]), c(0.1, 0.9), names = FALSE)
      expect_equal(c(got$.lower_ci[i[j]], got$.upper_ci[i[j]]), bounds)
    }
  }
})

test_that("mixed model totals include joint GAM and fitted random-effect draws", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$y <- d$y - d$off
  models <- list(
    gamm(y ~ x + z + fac + s(w, k = 6), random = list(group = ~1 + z),
      data = d, method = "REML"),
    gamm4::gamm4(y ~ x + z + fac + s(w, k = 6), random = ~(1 + z | group),
      data = d)
  )
  for (m in models) {
    g <- model_used_rows(m$gam)
    eta <- if (inherits(m, "gamm")) {
      as.numeric(m$lme$fitted[, ncol(m$lme$fitted)])
    } else as.numeric(predict(m$mer, type = "link"))
    expected <- withr::with_seed(83, {
      X <- predict_model(g, type = "lpmatrix")
      betas <- gaussian_draws(g, n_draws = 137, mvn_method = "mgcv")
      delta <- t(sweep(betas, 2, coef(g), `-`))
      extra <- variance_terms_random_draws(m, g, X, delta)
      values <- as.matrix(X %*% delta) + eta
      for (j in seq_along(extra$designs)) {
        values <- values + as.matrix(extra$designs[[j]] %*% extra$perturbations[[j]])
      }
      apply(values, 2L, var)
    })
    got <- variance_comp(m, type = "terms", n_draws = 137, seed = 83,
      coverage = 0.8)
    expect_equal(tail(got$.variance, 1), var(eta))
    expect_equal(attr(got, "variance_draws")[, "total"], unname(expected))
    bounds <- quantile(sqrt(expected), c(0.1, 0.9), names = FALSE)
    expect_equal(c(tail(got$.lower_ci, 1), tail(got$.upper_ci, 1)), bounds)
  }
})
