test_that("GJRM adapters use final distributional means and residuals for each family", {
  skip_if_not_installed("GJRM")
  withr::local_seed(27)
  families <- c("N", "LN", "GA", "GAi", "P", "NBI", "NBII")
  dat <- data.frame(x = runif(200, -1, 1))
  # GJRM initialisation can delete .Random.seed. Generate every response before
  # fitting so later fixtures remain reproducible and independent of fit order.
  responses <- setNames(lapply(families, function(fam) GJRM::sim.resp(
    fam,
    nrow(dat),
    eta = if (fam == "GAi") 2 + dat$x / 4 else dat$x / 4,
    sigma2 = 0.5,
    nu = 1
  )), families)
  for (fam in families) {
    dat$y <- responses[[fam]]
    f <- if (fam == "P") list(y ~ x) else list(y ~ x, ~x)
    # Current GJRM exposes GAi in distribution helpers but rejects it in
    # gamlss(). Represent the equivalent identity-mean Gamma parameterization
    # using the final fitted distribution from a supported GA fit.
    rng <- .Random.seed
    expect_silent(m <- withr::with_preserve_seed(GJRM::gamlss(
      f, data = dat, family = if (fam == "GAi") "GA" else fam
    )))
    expect_identical(.Random.seed, rng)
    if (fam == "GAi") {
      m$margins[] <- "GAi"
      m$VC$margins[] <- "GAi"
      m$eta1 <- exp(m$eta1)
    }
    d <- diagnostic_data(m, seed = 87)
    mu <- as.vector(m$eta1)
    expected_mean <- if (fam %in% c("N", "GAi")) {
      mu
    } else if (fam == "LN") {
      exp(mu + as.vector(m$sigma2)^2 / 2)
    } else {
      exp(mu)
    }
    expect_equal(d$data$fitted, expected_mean, info = fam)
    expect_equal(d$data$eta, mu, info = fam)
    expect_equal(d$data$observed, dat$y, info = fam)
    cdf_at <- function(q) switch(fam,
      N = pnorm(q, mean = mu, sd = m$sigma2),
      LN = plnorm(q, meanlog = mu, sdlog = m$sigma2),
      GA = pgamma(q, shape = 1 / m$sigma2^2, scale = exp(mu) * m$sigma2^2),
      GAi = pgamma(q, shape = 1 / m$sigma2^2, scale = mu * m$sigma2^2),
      P = ppois(q, lambda = exp(mu)),
      NBI = pnbinom(q, mu = exp(mu), size = 1 / m$sigma2),
      NBII = pnbinom(q, mu = exp(mu), size = exp(mu) / m$sigma2))
    cdf <- as.vector(cdf_at(dat$y))
    if (fam %in% c("P", "NBI", "NBII")) {
      cdf <- withr::with_seed(87,
        runif(length(cdf), as.vector(cdf_at(dat$y - 1)), cdf))
    }
    expect_equal(d$data$residuals, qnorm(cdf), info = fam)
    # NBII's stored CDF is Poisson in some upstream versions, despite a
    # negative-binomial likelihood. Check the likelihood mass independently.
    if (fam == "NBII") expect_equal(as.vector(m$fit$pdf2),
      as.vector(dnbinom(dat$y, mu = exp(mu), size = exp(mu) / m$sigma2)),
      tolerance = 1e-6)
    expect_equal(quantile_residuals(m, type = "quantile", seed = 87),
      d$data$residuals, ignore_attr = TRUE)
    expect_equal(quantile_residuals(m, type = "pit", seed = 87),
      cdf, tolerance = 1e-12, ignore_attr = TRUE)
    expect_equal(nrow(simulate(m, nsim = 3, seed = 87)), nrow(dat))
    before <- get0(".Random.seed", envir = .GlobalEnv)
    a <- appraise(m, scalable = TRUE, seed = 87, qq_bins = 20)
    expect_identical(get0(".Random.seed", envir = .GlobalEnv), before)
    expect_equal(sum(a[[2]]$data$count), nrow(dat))
    expect_equal(sum(a[[3]]$data$count), nrow(dat))
    expect_equal(a[[1]]$data, scalable_qq_plot(d, qq_bins = 20)$data)
    expect_silent(ggplot2::ggplot_build(a[[2]]))
    ordinary <- appraise(m, scalable = FALSE, seed = 87)
    expect_equal(ordinary[[2]]$data$residuals, d$data$residuals)
    expect_equal(nrow(ordinary[[1]]$data), nrow(dat))
    expect_equal(qq_plot(m, seed = 87)$data$residuals, sort(d$data$residuals))
    expect_equal(
      worm_plot(m, seed = 87)$data$residuals,
      sort(d$data$residuals) - qnorm(ppoints(nrow(dat)))
    )
    expect_error(scalable_qq_plot(m, method = "simulate"), "fixed")
    expect_s3_class(scalable_residuals_hist_plot(m, type = "pearson"), "ggplot")
  }
})

test_that("GJRM class collisions and unsupported fits are rejected", {
  expect_error(
    diagnostic_data(structure(list(), class = "gamlss")),
    "Only univariate"
  )
  dummy <- structure(
    list(
      univar.gamlss = TRUE,
      margins = "WEI",
      cens = 1,
      VC = list(surv = FALSE)
    ),
    class = c("gamlss", "SemiParBIV")
  )
  expect_error(diagnostic_data(dummy), "uncensored")
  dummy$margins <- "N"
  dummy$cens <- 0
  expect_error(diagnostic_data(dummy), "uncensored")
  dummy$univar.gamlss <- FALSE
  expect_error(diagnostic_data(dummy), "Only univariate")
})

test_that("GJRM standard-normal bands and worm detrending are aligned", {
  skip_if_not_installed("GJRM")
  dat <- withr::with_seed(2, data.frame(y = rnorm(100), x = runif(100)))
  m <- GJRM::gamlss(list(y ~ x, ~1), data = dat)
  d <- diagnostic_data(m)
  ref <- diagnostic_qq_data(m, d, level = 0.9)
  expect_equal(ref$data$lower, qnorm(qbeta(0.05, 1:100, 100:1)))
  expect_equal(ref$data$upper, qnorm(qbeta(0.95, 1:100, 100:1)))
  expect_equal(ref$intercept, 0)
  expect_equal(ref$slope, 1)
  expect_equal(
    scalable_worm_plot(m, qq_bins = 15)$data,
    bin_diagnostic_qq(ref, 15, worm = TRUE)
  )
})

test_that("GJRM negative-binomial diagnostics handle mixed dispersions at the Poisson limit", {
  # Known final parameters isolate gratia from upstream likelihood evaluation.
  # gamlss.dist 6.1.11 can recycle probabilities between rows at this boundary.
  # Stored probabilities are deliberately wrong so the adapter must rebuild them.
  mu <- c(2, 1, 4, 3, 2, 5)
  sigma <- c(1, 0.00011, 0.00009, 0.0001, 2, 1e-8)
  y <- c(0, 1, 3, 2, 0, 2)
  probabilities <- c(0.1, 0.3, 0.5, 0.7, 0.9, 0.95)
  for (fam in c("NBI", "NBII")) {
    model <- structure(list(
      univar.gamlss = TRUE, margins = fam, y1 = y,
      eta1 = log(mu), sigma2 = sigma,
      gam1 = list(model = data.frame(y = y)),
      VC = list(surv = FALSE),
      fit = list(pdf2 = dpois(y, mu), p = ppois(y, mu))
    ), class = c("gamlss", "SemiParBIV"))
    size <- if (fam == "NBI") 1 / sigma else mu / sigma
    expected_cdf <- pnbinom(y, mu = mu, size = size)
    expected_left <- pnbinom(y - 1, mu = mu, size = size)
    expect_silent(dist <- response_distribution(model))
    expect_equal(dist$mean, mu, info = fam)
    expect_equal(dist$variance, mu + mu^2 / size, info = fam)
    expect_equal(dist$cdf(y), expected_cdf, info = fam)
    expect_equal(dist$cdf_left(y), expected_left, info = fam)
    # Subtracting base-R CDFs loses precision at very large NB sizes.
    # Keep this full-vector identity at the standard numerical tolerance;
    # the direct CDF checks above and mass-at-zero check below are stricter.
    expect_equal(dist$cdf(y) - dist$cdf_left(y),
      dnbinom(y, mu = mu, size = size), tolerance = 1e-8, info = fam)
    expect_equal(dist$cdf(0),
      dnbinom(0, mu = mu, size = size), tolerance = 1e-12, info = fam)
    expect_equal(dist$cdf(y, log_p = TRUE, lower_tail = FALSE),
      pnbinom(y, mu = mu, size = size, log.p = TRUE, lower.tail = FALSE),
      info = fam)
    expect_equal(dist$qf(probabilities),
      qnbinom(probabilities, mu = mu, size = size), info = fam)
    expected_pit <- withr::with_seed(87,
      runif(length(y), expected_left, expected_cdf))
    expect_silent(d <- diagnostic_data(model, seed = 87))
    expect_equal(d$data$fitted, mu, info = fam)
    expect_equal(d$data$residuals, qnorm(expected_pit), info = fam)
    expect_equal(quantile_residuals(model, type = "pit", seed = 87),
      expected_pit, ignore_attr = TRUE, info = fam)
    expected_response <- withr::with_seed(87,
      replicate(3, rnbinom(length(y), mu = mu, size = size)))
    withr::local_seed(36)
    rng <- .Random.seed
    expect_silent(response <- simulate(model, nsim = 3, seed = 87))
    expect_identical(.Random.seed, rng)
    expect_equal(unname(as.matrix(response)), expected_response, info = fam)
  }
})
