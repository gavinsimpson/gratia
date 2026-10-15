# Matrix oracle independent of the streamed residual calculation.
dharma_matrix_oracle <- function(simulations, observed) {
  lo <- rowMeans(simulations < observed)
  hi <- rowMeans(simulations <= observed)
  if (any(lo != hi)) {
    random <- runif(length(lo), lo, hi)
    lo[lo != hi] <- random[lo != hi]
  }
  unname(lo)
}

dharma_test_proxy <- function(draw, y = c(0, 1, 2), independent = TRUE) {
  distribution <- new_response_distribution(mean = y, rd = draw,
    independent = independent, dependence_reason = if (!independent) "shared variation")
  new_diagnostic_proxy(y, y, seq_along(y), poisson(), distribution)
}

test_that("streamed PITs match a matrix calculation including ties and endpoints", {
  observed <- c(-1, 2, 3, 0, 9, 1.5)
  sims <- rbind(c(0, 1, 2, 3), c(1, 2, 2, 3), rep(3, 4),
    c(0, 0, 1, 1), c(1, 3, 5, 7), c(.5, 1, 1.5, 2))
  i <- 0L
  dist <- new_response_distribution(NULL, rd = function() {
    i <<- i + 1L
    sims[, i]
  })
  actual <- distribution_dharma_residuals(dist, observed, 4L, seed = 11)
  expected <- withr::with_seed(11, dharma_matrix_oracle(sims, observed))
  expect_equal(actual, expected)
  expect_identical(actual[c(1, 5)], c(0, 1))
  expect_equal(i, 4L)
  # No ties use the empirical grid and consume no extra random numbers.
  i <- 0L
  sims <- matrix(c(0, 1, 2, 3), nrow = 1)
  dist$rd <- function() { i <<- i + 1L; sims[, i] }
  expect_equal(distribution_dharma_residuals(dist, .5, 4L), .25)
})

test_that("invalid observations, simulations and counts have specific errors", {
  p <- dharma_test_proxy(function() c(0, 1, 2))
  for (n in list(0, -1, 1.5, NA_real_, Inf, c(1, 2), TRUE, "10"))
    expect_error(dharma_residuals(p, n = n), "integer")
  for (draw in list(function() 1:2, function() matrix(1:3), function() letters[1:3]))
    expect_error(dharma_residuals(dharma_test_proxy(draw), n = 2), "one numeric value")
  for (draw in list(function() c(0, NA, 2), function() c(0, Inf, 2)))
    expect_error(dharma_residuals(dharma_test_proxy(draw), n = 2), "finite")
  p$distribution$rd <- p$distribution$marginal_rd <- NULL
  expect_error(dharma_residuals(p), "no response generator")
  expect_error(distribution_dharma_residuals(new_response_distribution(NULL),
    matrix(1:3), 2), "scalar")
  expect_error(distribution_dharma_residuals(new_response_distribution(NULL),
    numeric(), 2), "scalar")
})

test_that("explicit seeds preserve RNG state, while NULL advances it", {
  withr::local_preserve_seed()
  p <- dharma_test_proxy(function() rpois(3, 1))
  set.seed(123)
  before <- .Random.seed
  a <- dharma_residuals(p, n = 20, seed = 42)
  expect_identical(.Random.seed, before)
  expect_equal(dharma_residuals(p, n = 20, seed = 42), a)
  dharma_residuals(p, n = 20)
  expect_false(identical(.Random.seed, before))
  rm(".Random.seed", envir = .GlobalEnv)
  dharma_residuals(p, n = 20, seed = 42)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  bad <- dharma_test_proxy(function() c(1, NA, 2))
  expect_error(dharma_residuals(bad, n = 2, seed = 42), "finite")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("extraction, QQ, worm and ordinary panels share the residual definition", {
  m <- glm(count ~ spray, family = poisson(), data = InsectSprays)
  r <- dharma_residuals(m, n = 30, seed = 21)
  q <- qq_plot(m, type = "dharma", n_dharma = 30, seed = 21)
  w <- worm_plot(m, type = "dharma", n_dharma = 30, seed = 21)
  expect_equal(q$data$residuals, sort(r))
  expect_equal(q$data$theoretical, ppoints(length(r)))
  expect_equal(q$data$lower, qbeta(.05, seq_along(r), rev(seq_along(r))))
  expect_equal(w$data$residuals, q$data$residuals - q$data$theoretical)
  expect_equal(w$data$lower, q$data$lower - q$data$theoretical)
  expect_match(q$labels$subtitle, "approximate pointwise")
  expect_match(q$labels$subtitle, "30 response simulations")
  expect_identical(q$labels$y, "DHARMa-style residuals")
  for (scalable in c(FALSE, TRUE)) {
    a <- appraise(m, type = "dharma", n_dharma = 30, seed = 21,
      scalable = scalable, density_method = "histogram")
    expect_equal(a[[1]]$data$residuals, q$data$residuals)
    if (!scalable) expect_equal(a[[2]]$data$residuals, r)
    expect_s3_class(a, "patchwork")
    expect_plot_render(a)
  }
  p <- residuals_linpred_plot(m, type = "dharma", n_dharma = 30, seed = 21, ylab = "Custom")
  expect_equal(p$data$residuals, r)
  expect_equal(p$layers[[1]]$data$yintercept, .5)
  expect_identical(p$labels$y, "Custom")
  h <- residuals_hist_plot(m, type = "dharma", n_dharma = 30, seed = 21)
  expect_equal(h$data$residuals, r)
  expect_identical(h$labels$x, "DHARMa-style residuals")
  s <- scalable_qq_plot(m, type = "dharma", n_dharma = 30, seed = 21)
  expect_equal(s$data$residuals, q$data$residuals)
  sw <- scalable_worm_plot(m, type = "dharma", n_dharma = 30, seed = 21)
  expect_equal(sw$data$residuals, w$data$residuals)
  expect_s3_class(residual_correlation_plot(m, type = "dharma", n_dharma = 30, seed = 21), "ggplot")
  c <- residual_correlation(m, type = "dharma", n_dharma = 30, seed = 21, lag_max = 3)
  expect_equal(c$correlation[2], cor(r[-length(r)], r[-1]))
  for (fun in list(qq_plot, worm_plot, appraise, scalable_qq_plot, scalable_worm_plot))
    expect_error(fun(m, type = "dharma", method = "simulate"), "method = NULL")
})

test_that("simulation counts and seeds invalidate cached DHARMa residuals", {
  draws <- 0L
  p <- dharma_test_proxy(function() { draws <<- draws + 1L; rpois(3, 1) })
  d <- prepare_diagnostic_data(p, "dharma", seed = 2, n_dharma = 7)
  expect_equal(draws, 7L)
  expect_equal(d$n_dharma, 7L)
  appraise(d, scalable = FALSE)
  expect_equal(draws, 7L)
  expect_equal(scalable_qq_plot(d)$data$residuals, sort(d$data$residuals))
  expect_equal(scalable_worm_plot(d)$data$residuals,
    sort(d$data$residuals) - ppoints(3))
  expect_equal(draws, 7L)
  d2 <- prepare_diagnostic_data(d, "dharma", seed = 2, n_dharma = 9)
  expect_equal(draws, 16L)
  expect_equal(d2$data$residuals, dharma_residuals(p, n = 9, seed = 2))
  before <- draws
  d3 <- prepare_diagnostic_data(d2, "dharma", seed = 3)
  expect_equal(draws - before, 9L)
  expect_equal(d3$data$residuals, dharma_residuals(p, n = 9, seed = 3))
  before <- draws
  appraise(p, type = "dharma", n_dharma = 11, seed = 1)
  expect_equal(draws - before, 11L)
})

test_that("missing rows are restored once and binomial encodings agree", {
  withr::local_seed(5)
  dat <- data.frame(x = seq_len(40), n = rep(10, 40), y = rbinom(40, 10, .4))
  dat$x[3] <- NA_real_
  m <- glm(cbind(y, n - y) ~ x, data = dat, family = binomial(), na.action = na.exclude)
  p <- glm(I(y / n) ~ x, weights = n, data = dat, family = binomial(), na.action = na.exclude)
  a <- dharma_residuals(m, n = 30, seed = 4)
  expect_length(a, 40L)
  expect_identical(which(is.na(a)), 3L)
  expect_equal(a, dharma_residuals(p, n = 30, seed = 4))
  h <- residuals_hist_plot(m, type = "dharma", n_dharma = 30, seed = 4)
  expect_equal(h$data$residuals, a)
  omitted <- update(m, na.action = na.omit)
  expect_equal(dharma_residuals(omitted, n = 30, seed = 4), a[-3])
  dat$binary <- factor(ifelse(dat$y > 4, "yes", "no"), levels = c("no", "yes"))
  b <- glm(binary ~ x, data = dat, family = binomial(), na.action = na.exclude)
  expect_length(dharma_residuals(b, n = 10, seed = 3), 40L)
})

test_that("generators need neither CDFs nor response moments", {
  p <- dharma_test_proxy(function() rpois(3, 1))
  expect_null(p$distribution$cdf)
  expect_s3_class(qq_plot(p, type = "dharma", n_dharma = 10, seed = 2), "ggplot")
  p$distribution$mean <- NULL
  p$distribution$mean_available <- p$distribution$variance_available <- FALSE
  expect_length(dharma_residuals(p, n = 10, seed = 2), 3L)
  expect_s3_class(appraise(p, type = "dharma", n_dharma = 10, seed = 2), "patchwork")
  # A real lm fit exercises the adapter without a proxy.
  m <- lm(dist ~ speed, data = cars)
  expect_equal(qq_plot(m, type = "dharma", n_dharma = 10, seed = 2)$data$residuals,
    sort(dharma_residuals(m, n = 10, seed = 2)))
})

test_that("dependent and marginal-only simulators omit independence-based bands", {
  p <- dharma_test_proxy(function() rep(rpois(1, 1), 3), independent = FALSE)
  q <- qq_plot(p, type = "dharma", n_dharma = 20, seed = 3)
  expect_false("lower" %in% names(q$data))
  expect_match(q$labels$subtitle, "bands omitted")
  expect_match(q$labels$subtitle, "shared variation")
  p$distribution$rd <- NULL
  p$distribution$joint_available <- FALSE
  expect_equal(q$data$residuals, sort(dharma_residuals(p, n = 20, seed = 3)))
  expect_s3_class(appraise(p, type = "dharma", n_dharma = 20, seed = 3), "patchwork")
})

test_that("grouped residuals retain component order, pooling and facets", {
  for (m in list(gfam_model, gfam_bam, gfam_missing_model)) {
    r <- dharma_residuals(m, n = 20, seed = 21)
    q <- qq_plot(m, type = "dharma", n_dharma = 20, seed = 21)
    expect_equal(sort(q$data$residuals), sort(r[is.finite(r)]))
    expect_true(".family" %in% names(q$data))
    expect_s3_class(q$facet, "FacetNull")
    f <- qq_plot(m, type = "dharma", n_dharma = 20, seed = 21, by_family = TRUE)
    expect_s3_class(f$facet, "FacetWrap")
    h <- residuals_hist_plot(m, type = "dharma", n_dharma = 20, seed = 21)
    expect_equal(h$data$residuals, r)
    expect_s3_class(appraise(m, type = "dharma", n_dharma = 20, seed = 21), "patchwork")
  }
})

test_that("multivariate and censored response encodings are rejected", {
  p <- dharma_test_proxy(function() 1:3)
  p$y <- matrix(1:6, nrow = 3)
  expect_error(dharma_residuals(p, n = 3), "multivariate")
  p$y <- structure(1:3, censor = c(-Inf, 2, 3))
  expect_error(dharma_residuals(p, n = 3), "censored")
})

test_that("glmmTMB adapters use DHARMa residuals before conventional calculations", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  withr::local_seed(32)
  dat <- data.frame(x = runif(60), g = factor(rep(1:6, each = 10)))
  dat$y <- rpois(60, exp(dat$x + rep(rnorm(6), each = 10)))
  m <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = dat, family = poisson())
  for (mode in c("conditional", "marginal")) {
    r <- dharma_residuals(m, n = 15, seed = 8, conditioning = mode)
    q <- qq_plot(m, type = "dharma", n_dharma = 15, seed = 8, conditioning = mode)
    expect_equal(q$data$residuals, sort(r))
    expect_identical("lower" %in% names(q$data), mode == "conditional")
    expect_s3_class(appraise(m, type = "dharma", n_dharma = 15, seed = 8,
      conditioning = mode), "patchwork")
  }
})

test_that("gamm and native mixed-model adapters retain their conditioning defaults", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  withr::local_seed(14)
  dat <- data.frame(x = runif(60), g = factor(rep(1:6, each = 10)))
  dat$y <- dat$x + rep(rnorm(6), each = 10) + rnorm(60)
  gm <- mgcv::gamm(y ~ s(x, k = 5), random = list(g = ~1), data = dat)
  mer <- lme4::lmer(y ~ x + (1 | g), data = dat)
  for (m in list(gm, gm$lme, mer, gamm4_gaussian, gamm4_binomial)) {
    r <- dharma_residuals(m, n = 12, seed = 9)
    expect_equal(r, dharma_residuals(m, n = 12, seed = 9, conditioning = "conditional"))
    expect_equal(qq_plot(m, type = "dharma", n_dharma = 12, seed = 9)$data$residuals, sort(r))
    expect_s3_class(appraise(m, type = "dharma", n_dharma = 12, seed = 9), "patchwork")
  }
})

test_that("family generators extend support without requiring analytic CDFs", {
  # An unknown family name prevents the analytic-family fallback from helping.
  m <- glm(count ~ spray, family = poisson(), data = InsectSprays)
  m$family$family <- "Custom count distribution"
  m$family$rd <- function(mu, wt, scale) rpois(length(mu), mu)
  expect_length(dharma_residuals(m, n = 20, seed = 8), nrow(InsectSprays))
  expect_s3_class(appraise(m, type = "dharma", n_dharma = 20, seed = 8), "patchwork")
  for (m in gfam_censored_models)
    expect_length(dharma_residuals(m, n = 12, seed = 8), length(m$y))
  m <- gfam_model
  # Change the active component to a family with no simulation hook.
  getfl <- m$family$getfl
  fl <- getfl()
  fl[[2]]$family <- "No generator"
  fl[[2]]$rd <- NULL
  m$family$getfl <- function() fl
  expect_error(dharma_residuals(m, n = 4), "grouped component 2 <No generator>")
  expect_error(dharma_residuals(m_censor, n = 4), "censored")
})

test_that("scalable summaries retain endpoint residuals and labels", {
  p <- dharma_test_proxy(function() c(1, 1, 1))
  r <- dharma_residuals(p, n = 5, seed = 1)
  expect_identical(r[c(1, 3)], c(0, 1))
  q <- scalable_qq_plot(p, type = "dharma", n_dharma = 5, seed = 1, qq_bins = 3)
  expect_equal(range(q$data$residuals), c(0, 1))
  h <- scalable_residuals_hist_plot(p, type = "dharma", n_dharma = 5, seed = 1)
  expect_equal(sum(h$data$count), 3)
  expect_identical(h$labels$x, "DHARMa-style residuals")
  l <- scalable_residuals_linpred_plot(p, type = "dharma", n_dharma = 5,
    seed = 1, density_method = "histogram")
  expect_equal(sum(l$data$count), 3)
  expect_identical(l$labels$y, "DHARMa-style residuals")
  expect_equal(l$layers[[2]]$data$yintercept, .5)
  q <- qq_plot(p, type = "dharma", n_dharma = 5, seed = 1,
    xlab = "X", ylab = "Y", subtitle = "S", title = "T", caption = "C")
  expect_identical(q$labels$x, "X")
  expect_identical(q$labels$y, "Y")
  expect_identical(q$labels$subtitle, "S")
  expect_identical(q$labels$title, "T")
  expect_identical(q$labels$caption, "C")
})

test_that("sdmTMB component and field options reach the response generator", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.1.0")
  withr::local_seed(25)
  d <- data.frame(x = seq(-1, 1, length.out = 50))
  d$y <- rbinom(50, 1, .65) * rgamma(50, 2, scale = .8)
  m <- sdmTMB::sdmTMB(y ~ x, data = d, spatial = "off", family = sdmTMB::delta_gamma())
  for (component in c("response", "positive", "binomial")) {
    r <- dharma_residuals(m, n = 10, seed = 4, component = component, field_method = "mle-eb")
    q <- qq_plot(m, type = "dharma", n_dharma = 10, seed = 4,
      component = component, field_method = "mle-eb")
    expect_equal(q$data$residuals, sort(r[is.finite(r)]))
    expect_s3_class(appraise(m, type = "dharma", n_dharma = 10, seed = 4,
      component = component, field_method = "mle-eb"), "patchwork")
  }
  for (mode in c("conditional", "marginal")) {
    r <- dharma_residuals(m, n = 10, seed = 4, conditioning = mode)
    q <- scalable_qq_plot(m, type = "dharma", n_dharma = 10, seed = 4, conditioning = mode)
    expect_equal(q$data$residuals, sort(r[is.finite(r)]))
  }
})

test_that("GJRM diagnostics use the final fitted response distribution", {
  skip_if_not_installed("GJRM")
  withr::local_seed(7)
  d <- data.frame(x = runif(80))
  d$y <- rnorm(80, d$x)
  m <- GJRM::gamlss(list(y ~ x, ~1), data = d, family = "N")
  r <- dharma_residuals(m, n = 15, seed = 6)
  expect_equal(qq_plot(m, type = "dharma", n_dharma = 15, seed = 6)$data$residuals, sort(r))
  a <- appraise(m, type = "dharma", n_dharma = 15, seed = 6, scalable = FALSE)
  expect_equal(a[[2]]$data$residuals, r)
})

test_that("extended and distributional mgcv families use their own generators", {
  models <- list(m_gaulss, m_betar, m_gammals, m_gumbls, m_scat, m_nb, m_tw,
    m_ocat, m_ziplss, m_twlss)
  for (m in models) {
    r <- dharma_residuals(m, n = 10, seed = 12)
    expect_length(r, NROW(m$model))
    expect_true(all(is.finite(r) & r >= 0 & r <= 1))
    expect_equal(qq_plot(m, type = "dharma", n_dharma = 10, seed = 12)$data$residuals,
      sort(r), info = m$family$family)
  }
})

test_that("abbreviated DHARMa types retain the requested simulation count", {
  m <- glm(count ~ spray, family = poisson(), data = InsectSprays)
  r <- dharma_residuals(m, n = 7, seed = 2)
  q <- qq_plot(m, type = "dharma", n_dharma = 7, seed = 2)
  for (fun in list(qq_plot, scalable_qq_plot)) {
    p <- fun(m, type = "dh", n_dharma = 7, seed = 2)
    expect_equal(p$data$residuals, sort(r))
    expect_equal(p$data, q$data)
  }
  for (fun in list(worm_plot, scalable_worm_plot)) {
    p <- fun(m, type = "dh", n_dharma = 7, seed = 2)
    expect_equal(p$data$residuals, sort(r) - ppoints(length(r)))
  }
  for (fun in list(residuals_hist_plot, residuals_linpred_plot))
    expect_equal(fun(m, type = "dh", n_dharma = 7, seed = 2)$data$residuals, r)
  for (scalable in c(FALSE, TRUE)) {
    a <- appraise(m, type = "dh", n_dharma = 7, seed = 2, scalable = scalable)
    expect_equal(a[[1]]$data$residuals, sort(r))
  }
  for (fun in list(scalable_residuals_hist_plot, scalable_residuals_linpred_plot)) {
    a <- fun(m, type = "dh", n_dharma = 7, seed = 2)
    b <- fun(m, type = "dharma", n_dharma = 7, seed = 2)
    expect_equal(a$data, b$data)
  }
  expect_equal(residual_correlation(m, type = "dh", n_dharma = 7, seed = 2),
    residual_correlation(m, type = "dharma", n_dharma = 7, seed = 2))
})

test_that("ordered diagnostics reuse prepared DHARMa residuals by default", {
  draws <- 0L
  p <- dharma_test_proxy(function() { draws <<- draws + 1L; rpois(3, 1) })
  d <- prepare_diagnostic_data(p, "dharma", seed = 2, n_dharma = 7)
  before <- draws
  c <- residual_correlation(d, lag_max = 1)
  expect_identical(attr(c, "residual_type"), "dharma")
  expect_match(attr(c, "description"), "DHARMa-style residuals")
  expect_equal(draws, before)
  expect_s3_class(residual_correlation_plot(d, lag_max = 1), "ggplot")
  expect_s3_class(residual_correlation_plot(d, type = "dharma", lag_max = 1), "ggplot")
  expect_equal(draws, before)
  # An explicit change of count still creates a new vector.
  residual_correlation_plot(d, n_dharma = 9, lag_max = 1)
  expect_equal(draws - before, 9L)
})

test_that("DHARMa appraisal retains conditioning for posterior predictive panels", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  withr::local_seed(32)
  dat <- data.frame(x = runif(60), g = factor(rep(1:6, each = 10)))
  dat$y <- rpois(60, exp(dat$x + rep(rnorm(6), each = 10)))
  fit <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = dat, family = poisson())
  for (mode in c("conditional", "marginal")) {
    for (scalable in c(FALSE, TRUE)) {
      expected <- posterior_predictive_plot(fit, n = 3, seed = 2,
        conditioning = mode, scalable = scalable)
      a <- appraise(fit, type = "dharma", n_dharma = 7, seed = 2,
        conditioning = mode, ppcheck = TRUE, n_ppc = 3, scalable = scalable)
      expect_equal(a[[3]]$data, expected$data)
      expect_identical(a[[3]]$labels$subtitle, expected$labels$subtitle)
      d <- prepare_diagnostic_data(fit, "dharma", seed = 2,
        conditioning = mode, n_dharma = 7)
      a <- appraise(d, seed = 2, ppcheck = TRUE, n_ppc = 3, scalable = scalable)
      expect_equal(a[[3]]$data, expected$data)
    }
  }
  # The native adapter must also normalise abbreviated types before preparing
  # residuals, otherwise it requests conventional residuals from glmmTMB.
  expect_equal(qq_plot(fit, type = "dh", n_dharma = 7, seed = 2)$data$residuals,
    sort(dharma_residuals(fit, n = 7, seed = 2)))
})
