# Fixtures use explicit exclusions, not random missing positions, so row identity
# and numerical correctness can be checked independently of implementation.
missing_row_fixture <- function(fit = mgcv::gam, family = gaussian()) {
  set.seed(349)
  d <- data.frame(x = runif(80), z = runif(80), w = rep(c(0.5, 2), 40))
  d$y <- sin(4 * d$x) + rnorm(80, sd = 1 / sqrt(d$w))
  if (family$family == "poisson") {
    d$y <- rpois(80, exp(d$x))
  }
  d$x[c(1, 21)] <- NA
  d$y[45] <- NA
  d$w[80] <- NA
  d$irrelevant <- NA_real_
  rownames(d) <- paste0("obs", seq_len(NROW(d)))
  keep <- !seq_len(NROW(d)) %in% c(1, 21, 45, 80)
  formula <- y ~ s(x, k = 5) + s(z, k = 5)
  list(
    data = d,
    keep = keep,
    excluded = fit(
      formula,
      data = d,
      weights = w,
      family = family,
      na.action = na.exclude
    ),
    omitted = fit(
      formula,
      data = d,
      weights = w,
      family = family,
      na.action = na.omit
    )
  )
}

test_that("partial residuals restore once and retain numerical alignment", {
  withr::local_seed(1)
  for (fit in list(mgcv::gam, mgcv::bam)) {
    f <- missing_row_fixture(fit)
    m <- f$excluded
    expected <- predict(f$omitted, type = "terms")
    attr(expected, "constant") <- NULL
    expected <- expected +
      as.numeric(residuals(f$omitted, "working")) *
        sqrt(weights(f$omitted, type = "working"))
    expect_silent(p <- partial_residuals(m))
    expect_equal(NROW(p), NROW(f$data))
    expect_true(all(is.na(as.matrix(p[!f$keep, ]))))
    expect_equal(unname(as.matrix(p[f$keep, ])), unname(expected))
    expect_equal(add_partial_residuals(f$data, m)[names(p)], p)
    expect_equal(
      as.matrix(add_partial_residuals(model.frame(m), m)[names(p)]),
      unname(expected),
      ignore_attr = TRUE
    )
    expect_equal(
      unname(add_residuals(f$data, m)$.residual),
      unname(residuals(m))
    )
    expect_equal(
      unname(add_residuals(model.frame(m), m)$.residual),
      unname(residuals(f$omitted))
    )
    expect_error(
      add_partial_residuals(f$data[-1, ], m),
      "Length of model residuals"
    )
    expect_silent(draw(m, residuals = TRUE))
  }
})

test_that("diagnostics use retained observations for every QQ method", {
  withr::local_seed(1)
  for (family in list(gaussian(), poisson())) {
    f <- missing_row_fixture(family = family)
    for (type in c("deviance", "pearson", "response")) {
      for (method in c("normal", "uniform", "simulate")) {
        for (fun in list(qq_plot, worm_plot)) {
          expect_silent(
            a <- fun(
              f$excluded,
              method = method,
              type = type,
              seed = 12,
              n_simulate = 4,
              n_uniform = 3
            )
          )
          b <- fun(
            f$omitted,
            method = method,
            type = type,
            seed = 12,
            n_simulate = 4,
            n_uniform = 3
          )
          expect_equal(a$data, b$data)
          expect_silent(ggplot2::ggplot_build(a))
        }
      }
    }
    expect_silent(appraise(f$excluded, n_uniform = 3))
    p <- observed_fitted_plot(f$excluded)
    expect_equal(p$data$fitted, unname(fitted(f$excluded)))
    expect_equal(p$data$observed[f$keep], f$excluded$y, ignore_attr = TRUE)
    expect_silent(ggplot2::ggplot_build(p))
    expect_silent(ggplot2::ggplot_build(residuals_hist_plot(f$excluded)))
  }
})

test_that("lm and glm dispersion and diagnostics survive exclusion", {
  withr::local_seed(349)
  d <- data.frame(x = runif(60), y = rnorm(60), w = rep(c(0.5, 2), 30))
  d$x[c(1, 30, 60)] <- NA
  for (fit in list(stats::lm, stats::glm)) {
    for (w in list(NULL, d$w)) {
      a <- fit(y ~ x, data = d, weights = w, na.action = na.exclude)
      b <- fit(y ~ x, data = d, weights = w, na.action = na.omit)
      if (inherits(a, "glm")) {
        expect_equal(dispersion(a), summary(a)$dispersion)
      }
      for (method in c("normal", "uniform", "simulate")) {
        for (fun in list(qq_plot, worm_plot)) {
          expect_equal(
            fun(a, method = method, seed = 2, n_simulate = 3)$data,
            fun(b, method = method, seed = 2, n_simulate = 3)$data
          )
        }
      }
      expect_silent(appraise(a, n_uniform = 3))
    }
  }
})

test_that("default fitted values and draws follow fitted's row convention", {
  withr::local_seed(1)
  f <- missing_row_fixture()
  a <- f$excluded
  b <- f$omitted
  expect_silent(fv <- fitted_values(a))
  expect_equal(fv$.fitted, unname(fitted(a)))
  expect_identical(fv$.row, seq_len(NROW(f$data)))
  expect_true(all(is.na(as.matrix(fv[
    !f$keep,
    c(".fitted", ".se", ".lower_ci", ".upper_ci")
  ]))))
  expect_true(all(is.na(fv$x[!f$keep])))
  expect_equal(fv$.se[f$keep], fitted_values(b)$.se)
  for (fun in list(fitted_samples, posterior_samples)) {
    expect_silent(x <- fun(a, n = 3, seed = 21))
    y <- fun(b, n = 3, seed = 21)
    value <- if (".fitted" %in% names(x)) ".fitted" else ".response"
    expect_equal(NROW(x), 3 * NROW(f$data))
    expect_true(all(is.na(x[[value]][!f$keep[x$.row]])))
    expect_equal(x[[value]][f$keep[x$.row]], y[[value]])
  }
  expect_silent(x <- simulate(a, nsim = 3, seed = 21))
  y <- simulate(b, nsim = 3, seed = 21)
  expect_equal(unname(as.matrix(x[f$keep, ])), unname(as.matrix(y)))
  expect_true(all(is.na(as.matrix(x[!f$keep, ]))))
})

test_that("explicit data retain prediction rows independently of training NAs", {
  withr::local_seed(1)
  f <- missing_row_fixture()
  d <- f$data
  m <- f$excluded
  valid <- !is.na(d$x) # response, training weights and irrelevant NAs do not matter
  expected <- predict(m, newdata = d, type = "response")
  expect_equal(fitted_values(m, data = d)$.fitted, as.numeric(expected))
  expect_equal(add_fitted(d, m)$.fitted, as.numeric(expected))
  expect_false(is.na(fitted_values(m, data = d)$.fitted[45]))
  for (action in list(na.omit, na.exclude, na.pass)) {
    omitted <- identical(action, na.omit)
    rows <- if (omitted) which(valid) else seq_len(NROW(d))
    fv <- fitted_values(m, data = d, na.action = action)
    expect_identical(fv$.row, rows)
    expect_equal(fv$.fitted, as.numeric(expected[rows]))
    expect_equal(add_fitted(d, m, na.action = action)$.fitted, fv$.fitted)
    for (fun in list(fitted_samples, posterior_samples)) {
      expect_silent(x <- fun(m, data = d, n = 2, seed = 1, na.action = action))
      expect_identical(x$.row, rep(rows, 2))
    }
    expect_silent(x <- simulate(m, data = d, nsim = 2, na.action = action))
    expect_equal(NROW(x), length(rows))
  }
  expect_error(
    fitted_values(m, data = d, na.action = na.fail),
    "missing values"
  )
  for (d in list(
    data.frame(x = NA_real_, z = NA_real_),
    data.frame(x = c(NA, 0.5), z = c(NA, 0.5))
  )) {
    for (fun in list(fitted_values, fitted_samples, posterior_samples)) {
      expect_silent(x <- fun(m, data = d))
      value <- if (".fitted" %in% names(x)) ".fitted" else ".response"
      expect_true(is.na(x[[value]][1]))
    }
    expect_silent(x <- simulate(m, data = d))
    expect_true(is.na(x[1, 1]))
  }
})

test_that("ordinal simulation never invents values for missing predictors", {
  withr::local_seed(349)
  d <- data.frame(x = runif(60), y = sample(1:3, 60, TRUE))
  d$x[c(1, 20, 60)] <- NA
  m <- gam(
    y ~ s(x, k = 5),
    family = ocat(R = 3),
    data = d,
    na.action = na.exclude
  )
  for (data in list(NULL, d)) {
    expect_silent(x <- simulate(m, data = data, nsim = 2, seed = 2))
    expect_true(all(is.na(as.matrix(x[c(1, 20, 60), ]))))
    expect_silent(p <- posterior_samples(m, data = data, n = 2, seed = 2))
    expect_true(all(is.na(p$.response[p$.row %in% c(1, 20, 60)])))
  }
})

test_that("smooth and basis evaluations only require their own variables", {
  withr::local_seed(1)
  f <- missing_row_fixture()
  d <- f$data
  m <- f$excluded
  for (smooth in c("s(x)", "s(z)")) {
    x <- smooth_estimates(m, select = smooth, data = d)
    expect_equal(NROW(x), NROW(d))
    expected_na <- if (smooth == "s(x)") which(is.na(d$x)) else integer()
    expect_identical(which(is.na(x$.estimate)), expected_na)
    b <- basis(m, select = smooth, data = d)
    expect_equal(NROW(b), NROW(d) * 4)
  }
  d$x <- NA_real_
  expect_true(all(is.na(
    smooth_estimates(m, select = "s(x)", data = d)$.estimate
  )))
})

test_that("subsets, factors and offsets use recorded exclusions", {
  withr::local_seed(349)
  d <- data.frame(
    x = runif(80),
    y = rnorm(80),
    off = runif(80),
    f = factor(rep(c("a", "b"), 40))
  )
  d$x[10] <- NA
  d$off[30] <- NA
  d$f[50] <- NA
  d$y[60] <- NA
  m <- gam(
    y ~ s(x, k = 5) + f + offset(off),
    data = d,
    subset = seq_len(80) > 5,
    na.action = na.exclude
  )
  fv <- fitted_values(m)
  expect_equal(NROW(fv), 75)
  expect_identical(which(is.na(fv$.fitted)), c(5L, 25L, 45L, 55L))
  expect_equal(fv$.fitted, unname(fitted(m)))
  expect_identical(levels(fv$f), levels(d$f))
  expect_equal(
    fitted_values(m, data = d)$.fitted,
    as.numeric(predict(m, newdata = d, type = "response"))
  )
})

test_that("multi-parameter and multivariate restoration uses the row dimension", {
  withr::local_seed(349)
  d <- data.frame(x = runif(80), y = rnorm(80), z = rnorm(80))
  d$x[c(1, 30, 80)] <- NA
  for (m in list(
    gam(
      list(y ~ s(x, k = 5), ~1),
      family = gaulss(),
      data = d,
      na.action = na.exclude
    ),
    gam(
      list(y ~ s(x, k = 5), z ~ s(x, k = 5)),
      family = mvn(d = 2),
      data = d,
      na.action = na.exclude
    )
  )) {
    expect_silent(fv <- fitted_values(m))
    expect_equal(NROW(fv), 160)
    expect_true(all(is.na(fv$.fitted[fv$.row %in% c(1, 30, 80)])))
    expect_silent(p <- residuals_linpred_plot(m))
    expect_equal(NROW(p$data), if (is_multivariate_y(m)) 160 else 80)
    expect_silent(ggplot2::ggplot_build(p))
    expect_silent(ggplot2::ggplot_build(residuals_hist_plot(m)))
    expect_silent(observed_fitted_plot(m))
    if (is_multivariate_y(m)) {
      expect_silent(x <- posterior_samples(m, n = 2, seed = 3))
      expect_equal(NROW(x), 320)
      expect_true(all(is.na(x$.response[x$.row %in% c(1, 30, 80)])))
      expect_silent(x <- simulate(m, nsim = 2, seed = 3))
      expect_equal(NROW(x), 160)
      expect_silent(
        p <- posterior_samples(m, n = 2, data = data.frame(x = NA_real_))
      )
      expect_equal(NROW(p), 4)
      expect_true(all(is.na(p$.response)))
      expect_setequal(p$.parameter, c("response1", "response2"))
    }
  }
})

test_that("mixed model adapters do not restore already expanded components twice", {
  skip_if_not_installed("gamm4")
  withr::local_seed(349)
  d <- data.frame(x = runif(80), y = rnorm(80), g = factor(rep(1:8, 10)))
  d$x[c(1, 30, 80)] <- NA
  for (m in list(
    gamm(y ~ s(x, k = 5), data = d, na.action = na.exclude)$gam,
    gamm4::gamm4(
      y ~ s(x, k = 5),
      data = d,
      random = ~ (1 | g),
      na.action = na.exclude
    )$gam
  )) {
    original <- m
    expect_silent(x <- partial_residuals(m))
    expect_equal(NROW(x), 80)
    expect_identical(which(is.na(x[[1]])), c(1L, 30L, 80L))
    expect_silent(appraise(m, n_uniform = 3))
    expect_silent(x <- fitted_values(m))
    expect_equal(NROW(x), 80)
    expect_silent(x <- quantile_residuals(m))
    expect_length(x, 80)
    expect_identical(m, original)
  }
})

test_that("binomial trials, supplied weights and RNG state stay aligned", {
  withr::local_seed(349)
  d <- data.frame(x = runif(70), trials = rep(c(5, 10), 35))
  d$success <- rbinom(70, d$trials, plogis(d$x))
  d$failure <- d$trials - d$success
  d$x[c(1, 35, 70)] <- NA
  a <- gam(
    cbind(success, failure) ~ s(x, k = 5),
    data = d,
    family = binomial(),
    na.action = na.exclude
  )
  b <- gam(
    cbind(success, failure) ~ s(x, k = 5),
    data = d,
    family = binomial(),
    na.action = na.omit
  )
  before <- .Random.seed
  x <- simulate(a, nsim = 3, seed = 5)
  expect_identical(.Random.seed, before)
  y <- simulate(b, nsim = 3, seed = 5)
  keep <- !is.na(d$x)
  expect_equal(unname(as.matrix(x[keep, ])), unname(as.matrix(y)))
  expect_equal(
    quantile_residuals(a, seed = 5)[keep],
    quantile_residuals(b, seed = 5),
    ignore_attr = TRUE
  )
  for (fun in list(fitted_samples, posterior_samples)) {
    fun(a, n = 3, seed = 5)
    expect_identical(.Random.seed, before)
  }
  d$trials[10] <- NA
  expect_silent(x <- simulate(a, data = d, weights = d$trials, nsim = 2))
  expect_true(all(is.na(as.matrix(x[c(1, 10, 35, 70), ]))))
  expect_error(
    simulate(a, data = d, weights = c(1, 2)),
    "one value per prediction row"
  )
  expect_error(
    posterior_samples(a, data = d, weights = c(1, 2)),
    "one value per prediction row"
  )
})

test_that("factor-by evaluation preserves missing row covariates", {
  withr::local_seed(349)
  d <- data.frame(
    x = runif(80),
    y = rnorm(80),
    f = factor(rep(c("a", "b"), 40))
  )
  m <- gam(y ~ f + s(x, by = f, k = 5), data = d)
  d$f[2] <- NA
  d$x[3] <- NA
  x <- smooth_estimates(m, data = d, select = "s(x):fa")
  expect_equal(NROW(x), 41)
  expect_equal(sum(is.na(x$.estimate)), 2)
  expect_equal(x$x[is.na(x$f)], d$x[2])
})

test_that("scam missing rows follow its supported NA option", {
  skip_if_not_installed("scam")
  withr::local_seed(349)
  withr::local_options(na.action = "na.exclude")
  d <- data.frame(x = runif(60), y = rnorm(60))
  d$x[c(1, 30, 60)] <- NA
  m <- scam::scam(y ~ s(x, k = 5, bs = "mpd"), data = d)
  expect_silent(fv <- fitted_values(m))
  expect_identical(which(is.na(fv$.fitted)), c(1L, 30L, 60L))
  expect_silent(x <- simulate(m, nsim = 2, seed = 1))
  expect_equal(NROW(x), 60)
  expect_true(all(is.na(as.matrix(x[c(1, 30, 60), ]))))
  expect_silent(x <- fitted_samples(m, n = 2, seed = 1))
  expect_equal(NROW(x), 120)
})


test_that("one evaluable row does not change draw ordering or weight alignment", {
  withr::local_seed(349)
  d <- data.frame(x = runif(60), y = rnorm(60))
  m <- gam(y ~ s(x, k = 5), data = d)
  nd <- data.frame(x = c(NA, 0.5, NA))
  for (fun in list(fitted_samples, posterior_samples)) {
    expect_silent(x <- fun(m, data = nd, n = 3, seed = 3))
    expect_identical(x$.row, rep(1:3, 3))
    expect_identical(x$.draw, rep(1:3, each = 3))
  }
  x <- posterior_samples(m, data = nd, weights = c(1, 4, 9), n = 3, seed = 3)
  y <- posterior_samples(
    m,
    data = nd[2, , drop = FALSE],
    weights = 4,
    n = 3,
    seed = 3
  )
  expect_equal(x$.response[x$.row == 2], y$.response)
})
