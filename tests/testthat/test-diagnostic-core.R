test_that("one QQ core retains exact references in memory and on disk", {
  withr::local_seed(101)
  for (nr in c(1L, 2L, 31L)) {
    for (n in c(1L, 2L, 7L)) {
      y <- rnorm(nr)
      for (discrete in c(FALSE, TRUE)) {
        draw <- function(p) if (discrete) rpois(length(p), 1) else rnorm(length(p))
        before <- list.files(tempdir(), pattern = "^gratia-qq-")
        a <- withr::with_seed(42, qq_reference_draws(y, draw, identity, n,
          "simulate", max_cells = 1e7))
        b <- withr::with_seed(42, qq_reference_draws(y, draw, identity, n,
          "simulate", max_cells = 3))
        expect_equal(b, a, tolerance = 1e-12)
        expect_equal(list.files(tempdir(), pattern = "^gratia-qq-"), before)
        w <- withr::with_seed(42, qq_reference_draws(y, draw, identity, n,
          "simulate", detrend = TRUE, max_cells = 3))
        expect_equal(w$residuals, a$residuals - a$theoretical)
        expect_equal(w$lower, a$lower - a$theoretical)
      }
    }
  }
  expect_error(qq_reference_draws(1:3, function(p) 1:2, identity, 2,
    "simulate"), "layout")
})

test_that("uniform references stream the historical per-rank average", {
  probabilities <- (1:17 - .5) / 17
  qfun <- function(p) qnorm(p, mean = seq_along(p) / 10)
  expected <- withr::with_seed(15, rowMeans(replicate(9,
    sort(qfun(sample(probabilities))))))
  actual <- withr::with_seed(15, qq_reference_draws(rep(1, 17),
    function(p) qfun(sample(p)), identity, 9, "uniform"))
  expect_equal(actual$theoretical, expected, tolerance = 1e-12)
})

test_that("LM and GLM dispersion and seeded references agree across entry points", {
  withr::local_seed(17)
  dat <- data.frame(x = runif(100), w = rep(c(.2, 2), 50))
  dat$y <- rgamma(100, shape = 3, scale = exp(dat$x) / 3)
  models <- list(lm(y ~ x, weights = w, data = dat),
    glm(y ~ x, family = Gamma(link = "log"), data = dat))
  for (m in models) {
    expected <- if (inherits(m, "glm")) summary(m)$dispersion else summary(m)$sigma^2
    expect_equal(diagnostic_data(m)$source$sig2, expected)
    for (method in c("uniform", "simulate")) {
      q <- qq_plot(m, method = method, n_uniform = 3, n_simulate = 4, seed = 42)
      a <- appraise(m, method = method, n_uniform = 3, n_simulate = 4,
        seed = 42, scalable = FALSE)
      s <- appraise(m, method = method, n_uniform = 3, n_simulate = 4,
        seed = 42, scalable = TRUE)
      expect_equal(q$data$theoretical, a[[1]]$data$theoretical)
      expect_equal(q$data$theoretical, s[[1]]$data$theoretical)
    }
  }
})

test_that("calibrated PIT QQ and worm bands use a uniform reference", {
  withr::local_seed(42)
  dat <- data.frame(x = runif(90), y = rpois(90, 2))
  m <- glm(y ~ x, family = poisson(), data = dat)
  q <- qq_plot(m, type = "pit", seed = 23)
  w <- worm_plot(m, type = "pit", seed = 23)
  expect_equal(q$data$theoretical, ppoints(90))
  expect_equal(q$data$lower, qbeta(.05, 1:90, 90:1))
  expect_equal(w$data$residuals, q$data$residuals - q$data$theoretical)
  expect_equal(w$data$lower, q$data$lower - q$data$theoretical)
  expect_error(qq_plot(m, type = "pit", method = "simulate"), "fixed")
  for (scalable in c(FALSE, TRUE)) {
    a <- appraise(m, type = "pit", seed = 23, scalable = scalable)
    expect_equal(a[[1]]$data$residuals, q$data$residuals)
    if (!scalable) expect_equal(sort(a[[2]]$data$residuals), q$data$residuals)
  }
})

test_that("new LM calibrated residuals restore exclusions and preserve RNG absence", {
  withr::local_preserve_seed()
  dat <- data.frame(y = c(1, 3, NA, 2, 6, 5), x = 1:6)
  m <- lm(y ~ x, data = dat, na.action = na.exclude)
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    rm(".Random.seed", envir = .GlobalEnv)
  q <- quantile_residuals(m, type = "quantile", seed = 2)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_length(q, 6L)
  expect_identical(which(is.na(q)), 3L)
  used <- complete.cases(dat)
  expect_equal(unname(q[used]), unname((dat$y[used] - fitted(m)[used]) / summary(m)$sigma))
  p <- residuals_linpred_plot(m, type = "quantile", seed = 2)
  expect_identical(which(is.na(p$data$residuals)), 3L)
})

test_that("multinomial native residual layouts match simulated references", {
  withr::local_seed(87)
  dat <- data.frame(x = runif(100), y = sample(0:2, 100, TRUE))
  m <- mgcv::gam(list(y ~ x, ~x), data = dat, family = mgcv::multinom(K = 2))
  p <- qq_plot(m, method = "simulate", n_simulate = 3, seed = 42)
  expect_equal(nrow(p$data), nrow(dat))
  expect_equal(p$data$residuals, sort(as.vector(residuals(m, type = "deviance"))))
  expect_s3_class(appraise(m, method = "simulate", n_simulate = 3), "patchwork")
  expect_error(quantile_residuals(m), "CDF")
})

test_that("dependent calibrated references use joint rank simulations", {
  n <- 40L
  distribution <- new_response_distribution(rep(0, n),
    rd = function() rnorm(1, sd = sqrt(.95)) + rnorm(n, sd = sqrt(.05)),
    cdf = function(q, log_p = FALSE, lower_tail = TRUE)
      pnorm(q, lower.tail = lower_tail, log.p = log_p),
    bounds = function(y) list(lower = y, upper = y), independent = FALSE)
  data <- data.frame(residuals = seq(-1, 1, length.out = n))
  reference <- withr::with_seed(4, calibrated_residual_reference(data,
    "quantile", .9, distribution = distribution, n_simulate = 500L))
  iid <- calibrated_residual_reference(data, "quantile", .9)
  expect_identical(attr(reference, "band_method"), "joint")
  expect_equal(reference$theoretical, qnorm(ppoints(n)))
  expect_gt(reference$upper[20] - reference$lower[20],
    2 * (iid$upper[20] - iid$lower[20]))
  distribution$rd <- NULL
  no_bands <- calibrated_residual_reference(data, "quantile", .9,
    distribution = distribution)
  expect_false("lower" %in% names(no_bands))
  expect_identical(attr(no_bands, "band_method"), "none")
})

test_that("BAM AR1 simulation and calibrated bands retain residual dependence", {
  withr::local_seed(77)
  dat <- data.frame(x = runif(80), y = rnorm(80), start = FALSE)
  dat$start[c(1, 41)] <- TRUE
  m <- mgcv::bam(y ~ s(x, k = 5), data = dat, rho = .6, AR.start = start)
  distribution <- response_distribution(m)
  expect_false(distribution$independent)
  sims <- as.matrix(simulate(m, nsim = 150, seed = 12))
  errors <- sims - as.numeric(fitted(m))
  left <- c(1:39, 41:79)
  expect_equal(cor(as.vector(errors[left, ]), as.vector(errors[left + 1, ])),
    .6, tolerance = .04)
  p <- qq_plot(m, type = "quantile", n_simulate = 20, seed = 4)
  expect_match(p$labels$subtitle, "simulated[[:space:]]+joint")
  expect_true(all(is.finite(p$data$lower)))
})

test_that("prepared diagnostic sources preserve original fitting-row identities", {
  dat <- data.frame(y = c(1, 4, NA, 3, 5, 2), x = seq_len(6))
  m <- lm(y ~ x, data = dat, na.action = na.exclude)
  prepared <- diagnostic_data(m)
  expect_identical(prepared$data$.row, c(1L, 2L, 4L, 5L, 6L))
  expect_equal(observed_fitted_plot(prepared)$data, observed_fitted_plot(m)$data)
  expect_equal(nrow(observed_fitted_plot(prepared)$data), nrow(dat))
  expect_true(is.na(observed_fitted_plot(prepared)$data$fitted[3]))
  expect_equal(residuals_linpred_plot(prepared)$data,
    residuals_linpred_plot(m)$data)
})

test_that("dependent model references use the joint generator across fitters", {
  distribution <- new_response_distribution(rep(0, 20),
    rd = function() rep(rnorm(1), 20),
    qf = function(p, log_p = FALSE, lower_tail = TRUE) qnorm(p),
    cdf = function(q, log_p = FALSE, lower_tail = TRUE) pnorm(q),
    bounds = function(y) list(lower = y, upper = y), independent = FALSE)
  proxy <- new_diagnostic_proxy(seq(-1, 1, length.out = 20), rep(0, 20),
    rep(0, 20), gaussian(), distribution)
  expect_message(method <- diagnostic_reference_method(proxy, "uniform"), "joint")
  expect_identical(method, "simulate")
  expect_message(p <- qq_plot(proxy, type = "response", method = "uniform",
    n_simulate = 3, seed = 2), "joint")
  expect_match(p$labels$subtitle, "Method: simulate")
})
