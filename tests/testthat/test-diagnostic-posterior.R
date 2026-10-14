test_that("ordinary posterior diagnostics respect weights, trials and offsets", {
  withr::local_seed(1)
  d <- data.frame(x = runif(120), wt = rep(c(1, 4), 60), off = runif(120))
  d$y <- 2 + d$x + d$off + rnorm(120, sd = 0.5 / sqrt(d$wt))
  m <- lm(y ~ x + offset(off), data = d, weights = wt)
  sims <- posterior_samples(m, n = 400, seed = 3)
  values <- matrix(sims$.response, nrow = nrow(d))
  expect_lt(max(abs(rowMeans(values) - fitted(m))), 0.2)
  deviations <- sweep(values, 1L, fitted(m))
  expect_gt(mean(deviations[d$wt == 1, ]^2) / mean(deviations[d$wt == 4, ]^2), 3)

  d$trials <- rep(c(3, 8), 60)
  d$success <- rbinom(120, d$trials, plogis(d$x + d$off))
  d$failure <- d$trials - d$success
  m <- glm(cbind(success, failure) ~ x + offset(off), data = d, family = binomial())
  p <- posterior_predictive_plot(m, n = 3, seed = 2)
  expect_equal(p$data$.response[p$data$.draw == 0L], d$success / d$trials)
  expect_true(all(p$data$.response >= 0 & p$data$.response <= 1))
  expect_silent(ggplot2::ggplot_build(p))
  expect_error(posterior_predictive_plot(glm(success ~ x, data = d,
    family = quasipoisson()), n = 2), "quasi families")
})

test_that("streamed PPC summaries equal retained draws for every batch size", {
  for (model in list(m_lm, m_glm, m_tiny_eg1, b_pois, m_mvn, m_multinom, m_gaulss, m_twlss, gfam_bam)) {
    raw <- posterior_predictive_plot(model, n = 4, seed = 7, scalable = FALSE)
    for (size in c(1L, 3L, 8L)) {
      streamed <- posterior_predictive_plot(model, n = 4, seed = 7,
        scalable = TRUE, batch_size = size)
      expect_equal(lapply(streamed$layers, `[[`, "data"),
        lapply(raw$layers, `[[`, "data"), ignore_attr = TRUE)
      expect_false(".row" %in% names(streamed$data))
      expect_silent(ggplot2::ggplot_build(streamed))
    }
  }
  expect_error(posterior_predictive_plot(m_lm, batch_size = 0), "positive integer")
  expect_error(posterior_predictive_plot(m_lm, scalable = NA), "TRUE, FALSE, or NULL")
})

test_that("new coefficient adapters preserve absent and existing RNG state", {
  withr::local_seed(33)
  before <- .Random.seed
  posterior_samples(m_lm, n = 3, seed = 2)
  expect_identical(.Random.seed, before)
  posterior_predictive_plot(m_lm, n = 3, seed = 2, scalable = TRUE)
  expect_identical(.Random.seed, before)
  rm(".Random.seed", envir = .GlobalEnv)
  posterior_predictive_plot(m_lm, n = 3, seed = 2, scalable = TRUE)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("distributional posterior draws use one joint coefficient draw", {
  # Replacing covariance by zero provides an independent native-RNG oracle:
  # all distributional predictors must retain their fitted values.
  m <- m_gaulss
  m$Vp[] <- 0
  sims <- diagnostic_posterior_samples(m, n = 1, seed = 11, mvn_method = "mgcv")
  native <- withr::with_seed(11, {
    sample.int(.Machine$integer.max, 1L)
    fix_family_rd(family(m))$rd(mu = fitted(m), wt = m$prior.weights, scale = m$sig2)
  })
  expect_equal(sims$.response, native)
  for (m in list(m_gaulss, m_twlss, gfam_bam)) {
    p <- posterior_predictive_plot(m, n = 2, seed = 1)
    expect_s3_class(p, "ggplot")
    expect_true(all(is.finite(p$data$.response)))
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("joint glmmTMB posterior diagnostics include all predictors", {
  skip_if_not_installed("glmmTMB")
  withr::local_seed(23)
  d <- data.frame(x = runif(120), group = factor(rep(1:12, each = 10)))
  d$y <- rpois(120, exp(0.5 + d$x))
  d$y[seq(1, 120, 4)] <- 0
  m <- glmmTMB::glmmTMB(y ~ x + (1 | group), ziformula = ~ x,
    data = d, family = poisson())
  p <- posterior_predictive_plot(m, n = 3, seed = 2)
  streamed <- posterior_predictive_plot(m, n = 3, seed = 2, scalable = TRUE)
  expect_equal(lapply(p$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"))
  expect_silent(ggplot2::ggplot_build(p))
  expect_true(all(p$data$.response >= 0))
  marginal <- posterior_predictive_plot(m, conditioning = "marginal", n = 2, seed = 3)
  expect_silent(ggplot2::ggplot_build(marginal))
  streamed <- posterior_predictive_plot(m, conditioning = "marginal", n = 2,
    seed = 3, scalable = TRUE)
  expect_equal(lapply(marginal$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"))
})


test_that("sparse joint precision draws marginalise other parameters", {
  covariance <- matrix(c(1, 0.4, -0.2, 0.4, 2, 0.3, -0.2, 0.3, 0.8), 3L)
  mean <- setNames(c(1, -2, 0.5), c("beta", "b", "theta"))
  precision <- Matrix::Matrix(solve(covariance), sparse = TRUE)
  dimnames(precision) <- list(names(mean), names(mean))
  sdr <- list(pdHess = TRUE, jointPrecision = precision)
  draws <- withr::with_seed(42, diagnostic_joint_parameter_draws(mean, 20000L,
    sdr, report = function() stop("not needed"), label = "test", index = 1:2))
  expect_equal(colMeans(draws), unname(mean[1:2]), tolerance = 0.03, ignore_attr = TRUE)
  expect_equal(stats::cov(draws), covariance[1:2, 1:2], tolerance = 0.04, ignore_attr = TRUE)
})

test_that("GJRM posterior responses draw final mean and scale coefficients jointly", {
  skip_if_not_installed("GJRM")
  withr::local_seed(27)
  d <- data.frame(x = runif(200))
  for (fam in c("N", "LN", "GA", "P", "NBI", "NBII")) {
    d$y <- GJRM::sim.resp(fam, nrow(d), eta = d$x / 4, sigma2 = 0.5, nu = 1)
    m <- GJRM::gamlss(if (fam == "P") list(y ~ x) else list(y ~ x, ~x),
      data = d, family = fam)
    p <- posterior_predictive_plot(m, n = 2, seed = 11)
    streamed <- posterior_predictive_plot(m, n = 2, seed = 11, scalable = TRUE)
    expect_equal(lapply(p$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"), info = fam)
    expect_equal(p$data$.response[p$data$.draw == 0], d$y, info = fam)
    expect_silent(ggplot2::ggplot_build(p))
  }
  # An implicit constant scale is still a final model coefficient.
  m <- GJRM::gamlss(list(y ~ x), data = d, family = "N")
  expect_s3_class(posterior_predictive_plot(m, n = 2, seed = 1), "ggplot")
  m$univar.gamlss <- FALSE
  expect_error(posterior_predictive_plot(m, n = 2), "univariate")
})

test_that("gamm4 full posterior diagnostics retain joint effect uncertainty", {
  skip_if_not_installed("gamm4")
  withr::local_seed(23)
  d <- data.frame(x = runif(80), group = factor(rep(1:8, each = 10)))
  d$y <- 1 + d$x + rnorm(8)[d$group] + rnorm(80, sd = 0.3)
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | group), data = d)
  class(m) <- c("gamm4", "list")
  for (mode in c("conditional", "marginal")) {
    p <- posterior_predictive_plot(m, n = 3, seed = 2, conditioning = mode)
    streamed <- posterior_predictive_plot(m, n = 3, seed = 2,
      conditioning = mode, scalable = TRUE)
    expect_equal(lapply(p$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"))
    expect_silent(ggplot2::ggplot_build(p))
  }
})


test_that("GAM posterior checks retain fitting-argument offsets", {
  withr::local_seed(3)
  d <- data.frame(x = runif(80), off = runif(80))
  d$y <- 1 + d$x + d$off + rnorm(80, sd = 0.3)
  m <- mgcv::gam(y ~ s(x, k = 5), data = d, offset = off)
  m$Vp[] <- 0
  samples <- posterior_samples(m, n = 1, seed = 11, mvn_method = "mgcv")
  expected <- withr::with_seed(11, {
    sample.int(.Machine$integer.max, 1L)
    rnorm(nrow(d), mean = fitted(m), sd = sqrt(m$sig2))
  })
  expect_equal(samples$.response, unname(expected))
  p <- posterior_predictive_plot(m, n = 2, seed = 11, mvn_method = "mgcv")
  streamed <- posterior_predictive_plot(m, n = 2, seed = 11,
    mvn_method = "mgcv", scalable = TRUE)
  expect_equal(lapply(p$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"))
})


test_that("streamed discrete summaries bound very large response support", {
  observed <- tibble::tibble(.row = 1:1500, .draw = 0L,
    .parameter = "response", .response = seq(0, 1, length.out = 1500))
  sampler <- function(i) {
    out <- observed
    out$.draw <- i
    out$.response <- out$.response * (1 + i / 10)
    out
  }
  summary <- posterior_predictive_stream(observed, sampler, n = 3L,
    discrete = c(response = TRUE), batch_size = 2L)
  expect_true(summary$binned)
  expect_equal(nrow(summary$data), 4L * 512L)
  expect_equal(as.numeric(tapply(summary$data$.density, summary$data$.draw, sum)), rep(1, 4))
  expect_equal(summary$data, posterior_predictive_stream(observed, sampler, 3L,
    discrete = c(response = TRUE), batch_size = 1L)$data)
})


test_that("new posterior samplers retain matrix dimensions for one draw", {
  for (m in list(m_lm, m_glm)) {
    samples <- posterior_samples(m, n = 1L, seed = 2)
    expect_equal(nrow(samples), nrow(model.frame(m)))
    expect_equal(unique(samples$.draw), 1L)
    expect_silent(ggplot2::ggplot_build(posterior_predictive_plot(m, n = 1, seed = 2)))
  }
})


test_that("new Gaussian posterior adapters reject unsupported requests", {
  for (m in list(m_lm, m_glm)) {
    expect_error(posterior_samples(m, data = data.frame(x = 1)), "fitting observations")
    expect_error(posterior_samples(m, newdata = data.frame(x = 1)), "fitting observations")
    for (method in c("mh", "inla", "user")) {
      expect_error(posterior_samples(m, method = method), "only method = 'gaussian'", fixed = TRUE)
    }
    expect_error(posterior_samples(m, draws = matrix(1)), "User coefficient draws")
  }
})

test_that("delta posterior checks retain selected components and rows", {
  skip_if_not_installed("sdmTMB")
  withr::local_seed(345)
  d <- data.frame(x = seq(-1, 1, length.out = 80))
  d$y <- rbinom(80, 1, 0.65) * rgamma(80, shape = 2, scale = 0.8)
  m <- sdmTMB::sdmTMB(y ~ x, data = d, spatial = "off",
    family = sdmTMB::delta_gamma())
  for (component in c("response", "positive", "binomial")) {
    p <- posterior_predictive_plot(m, n = 2, seed = 7, component = component)
    streamed <- posterior_predictive_plot(m, n = 2, seed = 7,
      component = component, scalable = TRUE)
    proxy <- sdmtmb_diagnostic_model(m, component = component)
    expected <- proxy$y
    expect_equal(p$data$.response[p$data$.draw == 0L], expected)
    expect_equal(p$data$.row[p$data$.draw == 0L], which(!is.na(model_row_map(proxy))))
    expect_equal(lapply(p$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"))
    expect_silent(ggplot2::ggplot_build(p))
    appraisal <- appraise(m, component = component, ppcheck = TRUE, n_ppc = 2,
      seed = 7, scalable = FALSE)
    expect_equal(appraisal[[3L]]$data, p$data)
  }
})
