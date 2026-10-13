test_that("heatmap counts and conditional densities use all observations", {
  x <- c(0, 0.1, 0.2, 1, 1)
  y <- c(0, 0, 1, 0, 1)
  d <- diagnostic_grid(x, y, c(2, 2), "conditional")
  expect_equal(sum(d$count), length(x))
  expect_lte(nrow(d), 4L)
  mass <- d$value * (d$ymax - d$ymin)
  expect_equal(as.vector(tapply(mass, d$xmin, sum)), c(1, 1))
  expect_equal(d$count, c(2, 1, 1, 1))
  counts <- diagnostic_grid(x, y, 2, "count")
  expect_equal(counts$value, counts$count)
  empty <- diagnostic_grid(c(0, 1), c(0, 1), c(4, 4), "conditional")
  expect_equal(nrow(empty), 2L)
  constant <- diagnostic_grid(rep(0, 10), rep(2, 10), 4, "count")
  expect_equal(sum(constant$count), 10)
  expect_equal(nrow(constant), 1L)
  expect_warning(diagnostic_grid(c(0, Inf), c(1, 2), 3, "count"), "Dropped 1")
  expect_error(diagnostic_grid(NA_real_, 1, 3, "count"), "No finite")
})

test_that("QQ arc-length bins preserve endpoints, intervals and detrending", {
  df <- tibble(
    theoretical = seq(-2, 2, length.out = 100),
    residuals = seq(-2, 2, length.out = 100)^3,
    lower = theoretical - 0.4,
    upper = theoretical + 0.4
  )
  ref <- list(data = df, center = df$theoretical)
  b <- bin_diagnostic_qq(ref, 12)
  expect_lte(nrow(b), 12L)
  expect_equal(b[1, ], df[1, ])
  expect_equal(b[nrow(b), ], df[nrow(df), ])
  expect_equal(b$upper - b$lower, rep(0.8, nrow(b)))
  expect_true(all(diff(b$theoretical) > 0))
  w <- bin_diagnostic_qq(ref, 12, worm = TRUE)
  expect_equal(w$residuals, b$residuals - b$theoretical)
  expect_equal(w$lower, b$lower - b$theoretical)
  expect_equal(w$upper, b$upper - b$theoretical)
  ties <- list(
    data = tibble(theoretical = rep(0, 100), residuals = rep(0, 100)),
    center = rep(0, 100)
  )
  expect_lte(nrow(bin_diagnostic_qq(ties, 10)), 3L)
  small <- list(data = df[1:2, ], center = df$theoretical[1:2])
  expect_equal(bin_diagnostic_qq(small, 3), small$data)
  bad <- ref
  bad$data$residuals[20] <- Inf
  expect_warning(bin_diagnostic_qq(bad, 12), "Dropped 1")
})

test_that("scalable QQ references agree with the original seeded calculations", {
  d <- diagnostic_data(m_tiny_eg1)
  for (method in c("normal", "uniform", "simulate")) {
    old <- qq_plot(
      m_tiny_eg1,
      method = method,
      n_uniform = 2,
      n_simulate = 3,
      seed = 731
    )
    ref <- withr::with_seed(
      731,
      diagnostic_qq_data(d$source, d, method, n_uniform = 2, n_simulate = 3)
    )
    expect_equal(ref$data, old$data, ignore_attr = TRUE)
    p <- scalable_qq_plot(
      m_tiny_eg1,
      method = method,
      n_uniform = 2,
      n_simulate = 3,
      seed = 731,
      qq_bins = 20
    )
    expect_equal(p$data, bin_diagnostic_qq(ref, 20))
    w <- scalable_worm_plot(
      m_tiny_eg1,
      method = method,
      n_uniform = 2,
      n_simulate = 3,
      seed = 731,
      qq_bins = 20
    )
    expect_equal(w$data, bin_diagnostic_qq(ref, 20, worm = TRUE))
  }
  before <- get0(".Random.seed", envir = .GlobalEnv)
  scalable_qq_plot(m_tiny_eg1, seed = 4)
  expect_identical(get0(".Random.seed", envir = .GlobalEnv), before)
  scalable_worm_plot(m_tiny_eg1, seed = 4)
  expect_identical(get0(".Random.seed", envir = .GlobalEnv), before)
})

test_that("histograms retain original break selection and zero centering", {
  for (bins in list("sturges", "scott", "fd", 17L)) {
    old <- ggplot_build(residuals_hist_plot(m_tiny_eg1, n_bins = bins))$data[[
      1
    ]]
    p <- scalable_residuals_hist_plot(m_tiny_eg1, n_bins = bins)
    expect_equal(p$data$xmin, old$xmin)
    expect_equal(p$data$xmax, old$xmax)
    expect_equal(p$data$count, old$count)
    expect_equal(sum(p$data$count), nrow(model.frame(m_tiny_eg1)))
  }
})

test_that("appraise switches using retained fitting rows and respects overrides", {
  n <- nrow(model.frame(m_tiny_eg1))
  old <- appraise(m_tiny_eg1, method = "normal", scalable_threshold = n)
  auto <- appraise(m_tiny_eg1, method = "normal", scalable_threshold = n - 1)
  forced <- appraise(
    m_tiny_eg1,
    method = "normal",
    scalable = TRUE,
    qq_bins = 20
  )
  original <- appraise(
    m_tiny_eg1,
    method = "normal",
    scalable = FALSE,
    scalable_threshold = 1
  )
  expect_true("eta" %in% names(old[[2]]$data))
  expect_true("value" %in% names(auto[[2]]$data))
  expect_lte(nrow(forced[[1]]$data), 20L)
  expect_true("eta" %in% names(original[[2]]$data))
  expect_s3_class(forced, "patchwork")
  expect_s3_class(
    appraise(m_tiny_eg1, scalable = TRUE, use_worm = TRUE, method = "normal"),
    "patchwork"
  )
  dat <- data.frame(y = c(1:20, NA), x = 1:21)
  m <- lm(y ~ x, data = dat, na.action = na.exclude)
  expect_equal(diagnostic_data(m)$n_obs, 20L)
  expect_equal(nrow(diagnostic_data(m)$data), 20L)
  expect_true(
    "eta" %in%
      names(appraise(m, method = "normal", scalable_threshold = 20)[[2]]$data)
  )
})

test_that("model adapters preserve weights, binomial proportions and multivariate rows", {
  dat <- data.frame(y = c(1, 3, 2, 4, 1, 2), n = c(2, 4, 5, 6, 2, 5), x = 1:6)
  m <- glm(cbind(y, n - y) ~ x, data = dat, family = binomial())
  d <- diagnostic_data(m)
  expect_equal(d$data$observed, dat$y / dat$n)
  expect_equal(d$data$fitted, unname(fitted(m)))
  expect_equal(d$data$residuals, unname(residuals(m, type = "deviance")))
  expect_s3_class(
    scalable_qq_plot(m, n_simulate = 3, method = "simulate"),
    "ggplot"
  )
  expect_equal(sum(scalable_observed_fitted_plot(m)$data$count), 6)
  expect_s3_class(
    scalable_qq_plot(lm(mpg ~ wt, mtcars), method = "normal"),
    "ggplot"
  )
  mv <- diagnostic_data(m_mvn)
  expect_equal(nrow(mv$data), length(residuals(m_mvn)))
  expect_equal(mv$data$residuals, as.vector(residuals(m_mvn)))
  expect_equal(mv$data$.row, rep(seq_len(mv$n_obs), 2))
  expect_s3_class(
    appraise(m_mvn, method = "simulate", n_simulate = 3, scalable = TRUE),
    "patchwork"
  )
  expect_s3_class(
    appraise(m_twlss, method = "simulate", n_simulate = 3, scalable = TRUE),
    "patchwork"
  )
})

test_that("bin controls and unsupported objects fail clearly", {
  for (bad in list(0, -1, NA, Inf, 1.5, TRUE, "30", numeric())) {
    expect_error(
      scalable_observed_fitted_plot(m_tiny_eg1, density_bins = bad),
      "density_bins"
    )
    expect_error(scalable_qq_plot(m_tiny_eg1, qq_bins = bad), "qq_bins")
  }
  expect_error(scalable_qq_plot(m_tiny_eg1, qq_bins = 2), "qq_bins")
  expect_error(appraise(m_tiny_eg1, scalable = NA), "scalable")
  expect_error(
    appraise(m_tiny_eg1, scalable_threshold = 0),
    "scalable_threshold"
  )
  expect_error(scalable_qq_plot(m_tiny_eg1, level = NA), "Level")
  expect_error(scalable_qq_plot(m_tiny_eg1, n_simulate = 0), "n_simulate")
  expect_error(scalable_qq_plot(data.frame(x = 1)), "Diagnostic data")
})

test_that("PIT and quantile heatmaps and histograms are seeded", {
  for (type in c("pit", "quantile")) {
    p <- scalable_residuals_linpred_plot(b_pois, type = type, seed = 4)
    expect_equal(
      p$data,
      scalable_residuals_linpred_plot(b_pois, type = type, seed = 4)$data
    )
    expect_equal(sum(p$data$count), nrow(model.frame(b_pois)))
    expect_equal(
      sum(
        scalable_residuals_hist_plot(b_pois, type = type, seed = 4)$data$count
      ),
      nrow(model.frame(b_pois))
    )
  }
})

test_that("scalable plot objects contain bounded data and build without original closures", {
  p <- scalable_observed_fitted_plot(m_tiny_eg1, density_bins = c(5, 6))
  expect_lte(nrow(p$data), 30L)
  expect_silent(ggplot_build(p))
  expect_true(all(vapply(
    p$layers,
    function(l) !is.function(l$data),
    logical(1)
  )))
  expect_silent(ggplot_build(scalable_qq_plot(m_tiny_eg1, method = "normal")))
  vdiffr::expect_doppelganger(
    "scalable-appraise",
    appraise(
      m_tiny_eg1,
      scalable = TRUE,
      method = "normal",
      qq_bins = 30,
      density_bins = c(15, 15)
    )
  )
})

test_that("gamm4 scalable diagnostics retain conditional working residual semantics", {
  for (m in list(
    gamm4_gaussian,
    gamm4_binomial,
    gamm4_poisson,
    gamm4_missing
  )) {
    g <- gamm4_gam(m)
    d <- diagnostic_data(m)
    expect_equal(
      d$data$residuals,
      as.vector(residuals(model_used_rows(g), type = "deviance"))
    )
    expect_equal(
      sum(scalable_observed_fitted_plot(m)$data$count),
      nrow(model.frame(g))
    )
    expect_equal(d$n_obs, nrow(model.frame(g)))
    expect_s3_class(
      appraise(
        m,
        scalable = TRUE,
        n_simulate = 3,
        method = "simulate",
        seed = 5
      ),
      "patchwork"
    )
  }
})

test_that("glmmTMB scalable diagnostics use native residuals and conditional references", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.9000")
  dat <- withr::with_seed(
    3,
    data.frame(y = rnorm(60), x = runif(60), g = factor(rep(1:6, each = 10)))
  )
  dat$x[3] <- NA_real_
  m <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = dat, na.action = na.exclude)
  d <- diagnostic_data(m)
  expect_equal(
    d$data$residuals,
    as.vector(na.omit(residuals(m, type = "pearson")))
  )
  expect_equal(d$n_obs, 59L)
  expect_equal(sum(scalable_observed_fitted_plot(m)$data$count), 59L)
  old <- qq_plot(m, n_simulate = 3, seed = 7)
  ref <- withr::with_seed(7, diagnostic_qq_data(d$source, d, n_simulate = 3))
  expect_equal(ref$data, old$data, ignore_attr = TRUE)
  expect_s3_class(
    appraise(m, scalable = TRUE, n_simulate = 3, seed = 7),
    "patchwork"
  )
  expect_error(scalable_qq_plot(m, method = "uniform"), "simulate")
  dat$x[3] <- 0.5
  dat$n <- rep(c(4, 7), 30)
  dat$y <- withr::with_seed(7, rbinom(60, dat$n, plogis(dat$x)))
  m <- glmmTMB::glmmTMB(cbind(y, n - y) ~ x, data = dat, family = binomial())
  expect_equal(diagnostic_data(m)$data$observed, dat$y / dat$n)
  expect_s3_class(scalable_qq_plot(m, n_simulate = 3), "ggplot")
})

test_that("new models can reuse all renderers through the adapter contract", {
  d <- diagnostic_data(m_tiny_eg1)
  object <- structure(list(), class = "diagnostic_extension_test")
  registerS3method(
    "diagnostic_data",
    "diagnostic_extension_test",
    function(model, type = NULL, seed = NULL) d,
    envir = asNamespace("gratia")
  )
  withr::defer(rm(
    "diagnostic_data.diagnostic_extension_test",
    envir = get(".__S3MethodsTable__.", envir = asNamespace("gratia"))
  ))
  expect_equal(
    scalable_qq_plot(object, method = "normal")$data,
    scalable_qq_plot(m_tiny_eg1, method = "normal")$data
  )
  expect_s3_class(
    appraise(object, scalable = TRUE, method = "normal"),
    "patchwork"
  )
  p <- scalable_qq_plot(object, method = "normal")
  expect_identical(rlang::get_env(p$mapping$x), asNamespace("gratia"))
  expect_identical(p$plot_env, asNamespace("gratia"))
})

test_that("explicit seeds preserve an absent RNG state", {
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  scalable_qq_plot(m_tiny_eg1, seed = 45)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  scalable_worm_plot(m_tiny_eg1, seed = 45)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  appraise(m_tiny_eg1, scalable = TRUE, seed = 45)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
