test_that("continuous response constructors retain probabilities and tail quantiles", {
  location <- c(-1, 0, 2)
  scale <- c(.4, .8, 1.3)
  probabilities <- c(.1, .5, .9)
  distributions <- list(
    logistic_response_distribution(location, scale),
    weibull_response_distribution(exp(location), c(1, 2, 3)),
    inverse_gaussian_response_distribution(exp(location), scale^2),
    gumbel_response_distribution(location, scale),
    gumbel_response_distribution(location, scale, reverse = TRUE)
  )
  for (d in distributions) {
    expect_equal(d$cdf(d$qf(probabilities)), probabilities, info = d$family)
    expect_equal(d$cdf_left(d$qf(probabilities)), probabilities, info = d$family)
    for (lower in c(TRUE, FALSE)) {
      log_p <- c(-10, -100, -1000)
      values <- d$qf(log_p, log_p = TRUE, lower_tail = lower)
      expect_true(all(is.finite(values)), info = d$family)
      expect_equal(d$cdf(values, log_p = TRUE, lower_tail = lower), log_p,
        tolerance = 1e-8, info = d$family)
    }
    expect_equal(d$cdf(-Inf), rep(0, 3), info = d$family)
    expect_equal(d$cdf(Inf), rep(1, 3), info = d$family)
    expect_equal(d$qf(0), rep(if (d$family %in% c("weibull", "inverse_gaussian"))
      0 else -Inf, 3), info = d$family)
    expect_equal(d$qf(1), rep(Inf, 3), info = d$family)
    expect_equal(d$qf(0, lower_tail = FALSE), rep(Inf, 3), info = d$family)
    expect_true(all(is.na(d$cdf(NA_real_))), info = d$family)
    expect_true(all(is.na(d$qf(NA_real_))), info = d$family)
  }
})

gjrm_additional_fixture <- local({
  models <- list()
  function(family, constant = FALSE) {
    key <- paste(family, constant)
    if (is.null(models[[key]])) {
      d <- withr::with_seed(145, {
        d <- data.frame(x = runif(200, -1, 1), z = runif(200, -1, 1))
        sigma <- if (family == "BE") plogis(-1 + .4 * d$z) else exp(-.3 + .4 * d$z)
        d$y <- GJRM::sim.resp(family, nrow(d), eta = .2 + .4 * d$x,
          sigma2 = sigma, nu = 1)
        d
      })
      formulas <- if (constant) list(y ~ x) else list(y ~ x, ~ z)
      models[[key]] <<- withr::with_preserve_seed(
        GJRM::gamlss(formulas, data = d, family = family))
    }
    models[[key]]
  }
})

test_that("GJRM additional families use final likelihood parameters and response moments", {
  skip_if_not_installed("GJRM")
  for (fam in c("BE", "LO", "WEI", "IG", "GU", "rGU")) {
    m <- gjrm_additional_fixture(fam)
    eta <- as.vector(m$eta1)
    sigma <- as.vector(m$sigma2)
    mu <- if (fam == "BE") plogis(eta) else if (fam %in% c("WEI", "IG")) exp(eta) else eta
    # GJRM imports these independently implemented distribution functions.
    suffix <- switch(fam, rGU = "RG", fam)
    reference <- function(prefix) get(paste0(prefix, suffix), asNamespace("GJRM"))
    # Some gamlss.dist versions set pGU to zero at every finite value. Use
    # the analytic CDF from GJRM's likelihood for this orientation instead.
    cdf <- if (fam == "GU") function(q, mu, sigma)
      -expm1(-exp((q - mu) / sigma)) else reference("p")
    qf <- reference("q")
    z <- (as.vector(m$y1) - mu) / sigma
    density <- switch(fam,
      BE = dbeta(m$y1, mu * (1 / sigma^2 - 1), (1 - mu) * (1 / sigma^2 - 1)),
      LO = dlogis(m$y1, mu, sigma),
      WEI = dweibull(m$y1, shape = sigma, scale = mu),
      IG = exp(-.5 * log(2 * pi) - log(sigma) - 1.5 * log(m$y1) -
        (m$y1 - mu)^2 / (2 * sigma^2 * mu^2 * m$y1)),
      GU = exp(z - exp(z)) / sigma,
      rGU = exp(-z - exp(-z)) / sigma)
    mean <- switch(fam, BE =, LO =, IG = mu,
      WEI = mu * gamma(1 + 1 / sigma),
      GU = mu - 0.5772156649015329 * sigma,
      rGU = mu + 0.5772156649015329 * sigma)
    variance <- switch(fam, BE = sigma^2 * mu * (1 - mu),
      LO = pi^2 * sigma^2 / 3, IG = sigma^2 * mu^3,
      WEI = mu^2 * (gamma(1 + 2 / sigma) - gamma(1 + 1 / sigma)^2),
      GU =, rGU = pi^2 * sigma^2 / 6)
    distribution <- response_distribution(m)
    expect_equal(distribution$mean, mean, info = fam)
    expect_equal(distribution$variance, variance, info = fam)
    expect_equal(distribution$cdf(m$y1), as.vector(cdf(m$y1, mu, sigma)), info = fam)
    probabilities <- rep(c(.1, .5, .9, .99), length.out = length(mu))
    if (fam == "IG") {
      # qIG uses approximate numerical inversion; check our quantiles against
      # its independently calculated CDF instead.
      expect_equal(cdf(distribution$qf(probabilities), mu, sigma), probabilities,
        tolerance = 1e-10, info = fam)
    } else {
      expect_equal(distribution$qf(probabilities), qf(probabilities, mu, sigma),
        tolerance = 1e-8, info = fam)
    }
    expect_equal(as.vector(m$fit$pdf2), as.vector(density),
      tolerance = 1e-6, info = fam)
    d <- diagnostic_data(m, seed = 9)
    expect_equal(d$data$fitted, mean, info = fam)
    expect_equal(d$data$residuals, qnorm(cdf(m$y1, mu, sigma)),
      ignore_attr = TRUE, info = fam)
    expect_equal(diagnostic_data(m, type = "response")$data$residuals,
      as.vector(m$y1) - mean, info = fam)
    expect_equal(diagnostic_data(m, type = "pearson")$data$residuals,
      (as.vector(m$y1) - mean) / sqrt(variance), info = fam)
    expect_equal(quantile_residuals(m, type = "pit"), as.vector(cdf(m$y1, mu, sigma)),
      ignore_attr = TRUE, info = fam)
    expect_equal(d$data$eta, eta, info = fam)
    label <- switch(fam, WEI = "scale", LO =, GU =, rGU = "location", "mu")
    expect_identical(d$eta_label, paste0("Linear predictor (", label, ")"))
    # Working GAM values and stored probabilities must not define diagnostics.
    altered <- m
    altered$gam1$fitted.values[] <- 100
    altered$gam1$coefficients[] <- 100
    altered$fit$p[] <- .5
    expect_equal(diagnostic_data(altered)$data, d$data, info = fam)
    expect_error(diagnostic_data(m, type = "deviance"), "does not define")
    expect_error(response_distribution(m, conditioning = "marginal"), "conditional")
    altered$cens <- c(0, rep(1, length(mu) - 1L))
    expect_error(diagnostic_data(altered), "uncensored")
  }
})

test_that("GJRM additional families support ordinary and scalable residual plots", {
  skip_if_not_installed("GJRM")
  withr::local_seed(23)
  for (fam in c("BE", "LO", "WEI", "IG", "GU", "rGU")) {
    m <- gjrm_additional_fixture(fam)
    before <- .Random.seed
    sims <- simulate(m, nsim = 3, seed = 9)
    expect_identical(.Random.seed, before)
    expect_equal(dim(sims), c(length(m$y1), 3L), info = fam)
    expect_true(all(is.finite(as.matrix(sims))), info = fam)
    if (fam == "BE") expect_true(all(as.matrix(sims) > 0 & as.matrix(sims) < 1))
    if (fam %in% c("WEI", "IG")) expect_true(all(as.matrix(sims) > 0))
    for (scalable in c(FALSE, TRUE)) {
      p <- appraise(m, scalable = scalable, seed = 9, qq_bins = 30)
      expect_s3_class(p, "patchwork")
      expect_silent(patchwork::patchworkGrob(p))
      expect_equal(qq_plot(m, seed = 9)$data$residuals,
        sort(diagnostic_data(m, seed = 9)$data$residuals), info = fam)
      expect_s3_class(worm_plot(m, seed = 9), "ggplot")
      for (type in c("response", "pearson")) {
        for (method in c("uniform", "simulate", "normal", "marginal")) {
          q <- if (scalable) scalable_qq_plot(m, type = type, method = method,
            n_uniform = 3, n_simulate = 4, seed = 9) else
            qq_plot(m, type = type, method = method, n_uniform = 3,
              n_simulate = 4, seed = 9)
          expect_silent(ggplot2::ggplot_build(q))
        }
      }
      r <- dharma_residuals(m, n = 12, seed = 9)
      a <- appraise(m, type = "dharma", n_dharma = 12, seed = 9, scalable = scalable)
      expect_silent(patchwork::patchworkGrob(a))
      if (!scalable) expect_equal(a[[2]]$data$residuals, as.vector(r), info = fam)
    }
    expect_identical(.Random.seed, before)
  }
})

test_that("GJRM additional posterior checks work with varying and implicit constant scales", {
  skip_if_not_installed("GJRM")
  for (fam in c("BE", "LO", "WEI", "IG", "GU", "rGU")) {
    for (constant in c(FALSE, TRUE)) {
      m <- gjrm_additional_fixture(fam, constant)
      raw <- posterior_predictive_plot(m, n = 3, seed = 11, scalable = FALSE)
      streamed <- posterior_predictive_plot(m, n = 3, seed = 11, scalable = TRUE,
        batch_size = 2)
      expect_equal(lapply(raw$layers, `[[`, "data"),
        lapply(streamed$layers, `[[`, "data"), info = fam)
      expect_equal(raw$data$.response[raw$data$.draw == 0], as.vector(m$y1), info = fam)
      expect_true(all(is.finite(raw$data$.response)), info = fam)
      if (fam == "BE") expect_true(all(raw$data$.response > 0 & raw$data$.response < 1))
      expect_silent(ggplot2::ggplot_build(raw))
      expect_silent(ggplot2::ggplot_build(streamed))
      expect_silent(patchwork::patchworkGrob(appraise(m, ppcheck = TRUE,
        n_ppc = 3, seed = 11, scalable = constant)))
    }
  }
})

test_that("GJRM posterior checks retain coefficient covariance and both predictor offsets", {
  skip_if_not_installed("GJRM")
  original <- gjrm_response_distribution
  seen <- NULL
  local_mocked_bindings(gjrm_response_distribution = function(model,
      eta = NULL, sigma = NULL, ...) {
    if (!is.null(eta)) seen <<- list(eta = eta, sigma = sigma)
    original(model, eta = eta, sigma = sigma, ...)
  })
  for (fam in c("BE", "LO", "WEI", "IG", "GU", "rGU")) {
    m <- gjrm_additional_fixture(fam)
    # Known covariance includes uncertainty shared by the two equations.
    m$Vb <- diag(.01, length(m$coefficients)) + .002
    m$eta1 <- m$eta1 + seq(-.2, .2, length.out = length(m$y1))
    m$eta2 <- m$eta2 + seq(.1, -.1, length.out = length(m$y1))
    n1 <- ncol(m$X1)
    i1 <- seq_len(n1)
    i2 <- setdiff(seq_along(m$coefficients), i1)
    draws <- withr::with_seed(14, diagnostic_gaussian_draws(2, m$coefficients, m$Vb))
    eta <- as.vector(m$eta1) + drop(m$X1 %*% (draws[1, i1] - m$coefficients[i1]))
    eta_scale <- as.vector(m$eta2) + drop(m$X2 %*% (draws[1, i2] - m$coefficients[i2]))
    sigma <- if (fam == "BE") plogis(eta_scale) else exp(eta_scale)
    mu <- if (fam == "BE") plogis(eta) else if (fam %in% c("WEI", "IG")) exp(eta) else eta
    sampler <- withr::with_seed(14, diagnostic_posterior_sampler(m, n = 2))
    actual <- withr::with_seed(15, sampler(1)$.response)
    expect_equal(seen$eta, eta, info = fam)
    expect_equal(seen$sigma, sigma, info = fam)
    if (fam == "BE") {
      precision <- 1 / sigma^2 - 1
      expect_equal(actual, withr::with_seed(15,
        rbeta(length(mu), mu * precision, (1 - mu) * precision)), info = fam)
    } else if (fam %in% c("LO", "WEI")) {
      generator <- get(paste0("r", fam), asNamespace("GJRM"))
      expect_equal(actual, withr::with_seed(15, generator(length(mu), mu, sigma)), info = fam)
    }
  }
})
