covariate_plot_fixture <- function(n = 240L) {
  withr::local_seed(912)
  d <- data.frame(x = runif(n, .1, 2), z = runif(n),
    f = factor(rep(c("a", "b", "c"), length.out = n)),
    g = factor(rep(seq_len(12), length.out = n)), exposure = runif(n, 1, 2))
  d$y <- 3 * (d$x - 1)^2 + as.numeric(d$f) + rnorm(n, sd = .3)
  list(data = d, model = mgcv::gam(y ~ x + s(z, k = 5) + f,
    data = d, method = "REML"))
}

test_that("covariate selection preserves order and excludes special variables", {
  f <- covariate_plot_fixture()
  m <- mgcv::gam(y ~ x + s(z, k = 5) + s(g, bs = "re") +
    offset(log(exposure)), data = f$data, method = "REML")
  expect_equal(residual_default_covariates(residual_covariate_context(m)), c("x", "z"))
  p <- residuals_covariate_plot(m, covariates = c("z", "x"), smooth = FALSE)
  expect_s3_class(p, "patchwork")
  expect_equal(p[[1]]$labels$x, "z")
  expect_equal(p[[2]]$labels$x, "x")
  expect_s3_class(residuals_covariate_plot(m, covariates = "g"), "ggplot")
  expect_s3_class(residuals_covariate_plot(m, covariates = "exposure", data = f$data), "ggplot")
})

test_that("terms accept only exact mgcv smooth labels", {
  f <- covariate_plot_fixture()
  m <- mgcv::gam(y ~ f + te(x, z, k = c(4, 4)) + s(z, by = f, k = 4),
    data = f$data, method = "REML")
  expect_true("te(x,z)" %in% smooths(m))
  for (fun in list(residuals_covariate_plot, scalable_residuals_covariate_plot)) {
    p <- fun(m, terms = "te(x,z)", smooth = FALSE)
    expect_equal(p[[1]]$labels$x, "x")
    expect_equal(p[[2]]$labels$x, "z")
    p <- fun(m, terms = "s(z):fb", smooth = FALSE)
    expect_equal(p[[1]]$labels$x, "z")
    expect_equal(p[[2]]$labels$x, "f")
    for (bad in c("te(x, z)", "f", "s(z)", "te")) {
      expect_error(fun(m, terms = bad), "smooths\\(model\\)")
    }
    expect_error(fun(m, terms = c("te(x,z)", "missing")), "missing")
    for (bad in list(1, TRUE, NA_character_, character())) {
      expect_error(fun(m, terms = bad), "'terms'")
    }
    expect_error(fun(m, terms = "te(x,z)", covariates = "x"), "only one")
  }
})

test_that("trends reveal a curved mean fitted as linear", {
  withr::local_seed(47)
  d <- data.frame(x = seq(-1, 1, length.out = 400))
  d$y <- 4 * d$x^2 + rnorm(400, sd = .1)
  linear <- mgcv::gam(y ~ x, data = d, method = "REML")
  curved <- mgcv::gam(y ~ s(x, k = 15), data = d, method = "REML")
  p <- residuals_covariate_plot(linear, covariates = "x", type = "pearson")
  q <- residuals_covariate_plot(curved, covariates = "x", type = "pearson")
  trend <- p$layers[[3]]$data
  expect_gt(stats::cor(trend$residuals, trend$x^2), .99)
  expect_gt(stats::sd(trend$residuals), .5)
  expect_lt(stats::sd(q$layers[[3]]$data$residuals), .05)
  s <- scalable_residuals_covariate_plot(linear, covariates = "x", type = "pearson")
  expect_equal(s$layers[[3]]$data, trend)
  expect_silent(ggplot2::ggplot_build(p))
  expect_silent(ggplot2::ggplot_build(s))
})

test_that("automatic trends respect eligible group sizes and explicit overrides", {
  withr::local_seed(519)
  d <- data.frame(x = runif(20201))
  d$y <- sin(2 * pi * d$x) + rnorm(nrow(d), sd = .2)
  d$group <- factor(c(rep("large", 20001), rep("small", 200)))
  d$available <- d$x
  d$available[seq_len(201)] <- NA_real_
  m <- mgcv::gam(y ~ x, data = d)
  boundary <- mgcv::gam(y ~ x, data = d, subset = seq_len(20000))
  trends <- list()
  for (fun in list(residuals_covariate_plot, scalable_residuals_covariate_plot)) {
    expect_null(formals(fun)$smooth)
    automatic <- fun(m, covariates = "x")
    expect_length(automatic$layers, 2L)
    expect_equal(fun(m, covariates = "x", smooth = NULL)$data, automatic$data)
    forced <- fun(m, covariates = "x", smooth = TRUE)
    expect_length(forced$layers, 3L)
    expect_equal(nrow(forced$layers[[3]]$data), 200L)
    trends[[length(trends) + 1L]] <- forced$layers[[3]]$data
    expect_length(fun(m, covariates = "x", smooth = FALSE)$layers, 2L)
    expect_length(fun(boundary, covariates = "x")$layers, 3L)
    expect_length(fun(boundary, covariates = "x", smooth = FALSE)$layers, 2L)
    grouped <- fun(m, covariates = "x", data = d, by = "group")
    expect_equal(nrow(grouped$layers[[3]]$data), 200L)
    expect_match(as.character(unique(grouped$layers[[3]]$data$.panel)), "small")
    grouped_forced <- fun(m, covariates = "x", data = d, by = "group", smooth = TRUE)
    expect_equal(nrow(grouped_forced$layers[[3]]$data), 400L)
    expect_warning(eligible <- fun(m, covariates = "available", data = d), "Dropped 201")
    expect_equal(nrow(eligible$layers[[3]]$data), 200L)
    expect_silent(ggplot2::ggplot_build(automatic))
    expect_silent(ggplot2::ggplot_build(grouped))
    for (bad in list(NA, logical(), c(TRUE, FALSE), 1, "auto")) {
      expect_error(fun(m, smooth = bad), "'smooth' must be NULL, TRUE or FALSE")
    }
  }
  expect_equal(trends[[1]], trends[[2]])
})

test_that("joint maps reveal an interaction missed by marginal trends", {
  d <- expand.grid(x = seq(-1, 1, length.out = 30), z = seq(-1, 1, length.out = 30))
  d$y <- 3 * d$x * d$z
  m <- mgcv::gam(y ~ x + z, data = d)
  marginal <- residuals_covariate_plot(m, covariates = c("x", "z"))
  expect_lt(max(abs(marginal[[1]]$layers[[3]]$data$residuals)), 1e-6)
  expect_lt(max(abs(marginal[[2]]$layers[[3]]$data$residuals)), 1e-6)
  p <- residuals_covariate_plot(m, covariates = character(),
    pairs = list(c("x", "z")), bins = 6)
  s <- scalable_residuals_covariate_plot(m, covariates = character(),
    pairs = list(c("x", "z")), bins = 6)
  expect_equal(p$data, s$data)
  expect_equal(sum(s$data$count), nrow(d))
  expect_gt(diff(range(s$data$value)), 3)
  expect_gt(stats::cor(s$data$value,
    (s$data$xmin + s$data$xmax) * (s$data$ymin + s$data$ymax)), .99)
  expect_length(p$layers, 2L)
  expect_length(s$layers, 1L)
  expect_silent(ggplot2::ggplot_build(p))
  expect_silent(ggplot2::ggplot_build(s))
  sparse <- scalable_residuals_covariate_plot(m, covariates = character(),
    pairs = list(c("x", "z")), bins = 6, min_n = 26)
  expect_true(all(is.na(sparse$data$value)))
  sd <- scalable_residuals_covariate_plot(m, covariates = character(),
    pairs = list(c("x", "z")), bins = 6, statistic = "sd")
  rows <- d$x <= sort(unique(d$x))[5] & d$z <= sort(unique(d$z))[5]
  expect_equal(sd$data$value[1], stats::sd(residuals(m)[rows]))
})

test_that("categorical summaries agree and grouping preserves observations", {
  f <- covariate_plot_fixture()
  p <- residuals_covariate_plot(f$model, covariates = "f")
  s <- scalable_residuals_covariate_plot(f$model, covariates = "f")
  expect_equal(p$data, s$data)
  expect_equal(sum(s$data$count), nrow(f$data))
  expect_equal(s$data$middle, as.numeric(tapply(residuals(f$model), f$data$f, stats::median)))
  expect_silent(ggplot2::ggplot_build(p))
  expect_silent(ggplot2::ggplot_build(s))
  p <- residuals_covariate_plot(f$model, covariates = "x", by = "f")
  expect_equal(nlevels(p$data$.panel), 3L)
  expect_equal(length(unique(p$layers[[3]]$data$.panel)), 3L)
  s <- scalable_residuals_covariate_plot(f$model, covariates = "x", by = "f",
    density_method = "histogram")
  expect_equal(sum(s$data$count), nrow(f$data))
  expect_equal(nrow(ggplot2::ggplot_build(s)$layout$layout), 3L)
  joint <- scalable_residuals_covariate_plot(f$model, covariates = character(),
    pairs = list(c("x", "z")), by = "f", bins = 3)
  expect_equal(sum(joint$data$count), nrow(f$data))
  expect_equal(nrow(ggplot2::ggplot_build(joint)$layout$layout), 3L)
})

test_that("raw transformed and omitted covariates use verified fitting data", {
  f <- covariate_plot_fixture()
  d <- f$data
  m <- mgcv::gam(y ~ log(x) + s(z, k = 5), data = d, method = "REML")
  environment(m$formula) <- environment()
  p <- residuals_covariate_plot(m, covariates = "x", smooth = FALSE)
  expect_equal(p$data$x, d$x)
  m2 <- mgcv::gam(y ~ s(log(x), k = 5), data = d, method = "REML")
  p <- residuals_covariate_plot(m2, terms = "s(log(x))", data = d, smooth = FALSE)
  expect_equal(p$data$x, d$x)
  p <- residuals_covariate_plot(m, covariates = "f", data = d)
  expect_equal(sum(p$data$count), nrow(d))
  unavailable <- m
  unavailable$call$data <- quote(no_such_fitting_data)
  expect_error(residuals_covariate_plot(unavailable, covariates = "x"), "Supply original")
  expect_s3_class(residuals_covariate_plot(unavailable, covariates = "x", data = d), "ggplot")
  changed <- d
  changed$x <- rev(changed$x)
  expect_error(residuals_covariate_plot(m, covariates = "x", data = changed), "do not agree")
  changed <- d[sample.int(nrow(d)), ]
  rownames(changed) <- NULL
  expect_error(residuals_covariate_plot(m, covariates = "x", data = changed), "do not agree")
})

test_that("subsetting and both missing-value actions preserve row alignment", {
  f <- covariate_plot_fixture()
  d <- f$data
  d$x[c(3, 19, 111)] <- NA
  for (action in list(stats::na.exclude, stats::na.omit)) {
    m <- mgcv::gam(y ~ x + s(z, k = 5), data = d,
      subset = seq_len(nrow(d)) %% 2 == 1, na.action = action)
    frame <- stats::model.frame(m)
    original <- as.integer(rownames(frame))
    for (supplied in list(d, d[original, ], tibble::as_tibble(d[original, ]))) {
      p <- residuals_covariate_plot(m, covariates = "exposure", data = supplied, smooth = FALSE)
      expect_equal(p$data$x, d$exposure[original])
      expect_equal(p$data$residuals, as.numeric(residuals(model_used_rows(m))))
    }
    p <- residuals_covariate_plot(m, covariates = "x", smooth = FALSE)
    expect_equal(p$data$x, d$x[original])
  }
})

test_that("multivariate responses and grouped families are separated", {
  f <- covariate_plot_fixture()
  d <- f$data
  d$y2 <- d$x + stats::rnorm(nrow(d))
  m <- mgcv::gam(list(y ~ s(x, k = 5), y2 ~ s(x, k = 5)),
    data = d, family = mgcv::mvn(d = 2))
  p <- residuals_covariate_plot(m, covariates = "x", smooth = FALSE)
  expect_equal(p$data$x, rep(d$x, 2))
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  s <- scalable_residuals_covariate_plot(m, covariates = "x", smooth = FALSE,
    density_method = "histogram")
  expect_equal(sum(s$data$count), 2 * nrow(d))
  skip_if_not(exists("gfam", asNamespace("mgcv"), inherits = FALSE))
  family_index <- rep(1:2, length.out = nrow(d))
  d$Y <- cbind(d$y, family_index)
  m <- mgcv::gam(Y ~ s(x, k = 5), family = mgcv::gfam(list(stats::gaussian(),
    stats::gaussian())), data = d, method = "REML")
  p <- residuals_covariate_plot(m, covariates = "x", smooth = FALSE, seed = 1)
  expect_equal(nrow(p$data), nrow(d))
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  expect_equal(p$data$residuals, diagnostic_data(m, seed = 1)$data$residuals)
})

test_that("randomised residuals are shared across panels and preserve RNG state", {
  f <- covariate_plot_fixture()
  d <- f$data
  d$count <- withr::with_seed(1, rpois(nrow(d), exp(d$z)))
  m <- mgcv::gam(count ~ z, data = d, family = stats::poisson())
  withr::local_seed(93)
  before <- .Random.seed
  for (type in c("quantile", "pit", "dharma")) {
    p <- residuals_covariate_plot(m, covariates = c("x", "z"), data = d, type = type,
      seed = 42, n_dharma = 20, smooth = FALSE)
    expect_equal(p[[1]]$data$residuals, p[[2]]$data$residuals)
    expect_identical(.Random.seed, before)
    s <- scalable_residuals_covariate_plot(m, covariates = "x", data = d, type = type,
      seed = 42, n_dharma = 20, smooth = FALSE, density_method = "histogram")
    expect_equal(sum(s$data$count), nrow(d))
    expect_equal(s$layers[[2]]$data$yintercept,
      if (type %in% c("pit", "dharma")) .5 else 0)
    expect_identical(.Random.seed, before)
  }
  prepared <- diagnostic_data(m, type = "quantile", seed = 42)
  p <- residuals_covariate_plot(prepared, covariates = "z", smooth = FALSE)
  expect_equal(p$data$residuals, prepared$data$residuals)
})

test_that("scalable displays use all observations and retain compact data", {
  f <- covariate_plot_fixture(12000L)
  p <- scalable_residuals_covariate_plot(f$model, covariates = "x",
    density_bins = c(12, 15), density_method = "histogram")
  expect_equal(sum(p$data$count), 12000L)
  expect_lte(nrow(p$data), 12 * 15)
  expect_equal(nrow(p$layers[[3]]$data), 200L)
  expect_lt(as.numeric(object.size(p)), as.numeric(object.size(f$model)))
  expect_equal(p$plot_env, environment(compact_diagnostic_plot))
  ordinary <- residuals_covariate_plot(f$model, covariates = "x")
  expect_equal(p$layers[[3]]$data, ordinary$layers[[3]]$data)
  kde <- scalable_residuals_covariate_plot(f$model, covariates = "x",
    smooth = FALSE, density_bins = c(12, 15))
  expect_equal(sum(kde$data$count), 12000L)
  mass <- with(kde$data, tapply(value * (ymax - ymin), xmin, sum))
  expect_equal(as.numeric(mass), rep(1, length(mass)))
})

test_that("constant covariates, missing plotting values and invalid requests are handled", {
  f <- covariate_plot_fixture()
  d <- f$data
  d$constant <- 2
  p <- residuals_covariate_plot(f$model, covariates = "constant", data = d)
  expect_length(p$layers, 2L)
  expect_silent(ggplot2::ggplot_build(p))
  s <- scalable_residuals_covariate_plot(f$model, covariates = "constant", data = d)
  expect_equal(sum(s$data$count), nrow(d))
  d$extra <- seq_len(nrow(d))
  d$extra[1] <- NA
  expect_warning(p <- residuals_covariate_plot(f$model, covariates = "extra", data = d), "Dropped 1")
  expect_equal(nrow(p$data), nrow(d) - 1L)
  for (fun in list(residuals_covariate_plot, scalable_residuals_covariate_plot)) {
    expect_error(fun(f$model, covariates = character()), "No covariates")
    expect_error(fun(f$model, covariates = "missing"), "missing")
    expect_error(fun(f$model, pairs = c("x", "z")), "list")
    expect_error(fun(f$model, pairs = list(c("x", "x"))), "distinct")
    expect_error(fun(f$model, pairs = list(c("x", "f"))), "numeric")
    expect_error(fun(f$model, by = "x"), "categorical")
    expect_error(fun(f$model, by = c("f", "g")), "one categorical")
    expect_error(fun(f$model, smooth = NA), "'smooth'")
    expect_error(fun(f$model, min_n = 0), "min_n")
    expect_error(fun(f$model, bins = -1), "bins")
  }
})

test_that("supplied data can be reordered when row identities and values agree", {
  f <- covariate_plot_fixture()
  d <- f$data[rev(seq_len(nrow(f$data))), ]
  p <- residuals_covariate_plot(f$model, covariates = "exposure", data = d,
    smooth = FALSE)
  expect_equal(p$data$x, f$data$exposure)
  expect_equal(p$data$residuals, as.numeric(residuals(f$model)))
  expect_error(residuals_covariate_plot(f$model,
    pairs = list(c("x", "z", "x"))), "distinct")
  d <- f$data
  d$f[1] <- NA
  # Use a grouping covariate absent from the fitted mean to preserve its rows.
  m <- mgcv::gam(y ~ x, data = d)
  expect_warning(p <- residuals_covariate_plot(m, covariates = "x", by = "f",
    data = d), "Dropped 1")
  expect_equal(nrow(p$data), nrow(d) - 1L)
})

test_that("distributional predictors contribute covariates without extra residuals", {
  f <- covariate_plot_fixture()
  m <- mgcv::gam(list(y ~ s(x, k = 5), ~ z), family = mgcv::gaulss(),
    data = f$data, method = "REML")
  expect_equal(residual_default_covariates(residual_covariate_context(m)), c("z", "x"))
  p <- residuals_covariate_plot(m, covariates = "z", smooth = FALSE)
  expect_equal(nrow(p$data), nrow(f$data))
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 1L)
})

test_that("GAM mixed models retain smooth selection and complete-model residuals", {
  skip_if_not_installed("nlme")
  f <- covariate_plot_fixture()
  m <- mgcv::gamm(y ~ s(x, k = 5) + z, random = list(g = ~1), data = f$data)
  p <- residuals_covariate_plot(m, terms = "s(x)", type = "pearson",
    smooth = FALSE)
  expect_equal(p$data$x, f$data$x)
  expect_equal(p$data$residuals, diagnostic_data(m, type = "pearson")$data$residuals)
  direct <- residuals_covariate_plot(m$lme, covariates = "x", smooth = FALSE)
  expect_equal(nrow(direct$data), nrow(f$data))
})

test_that("direct lme4 models use fixed predictors by default", {
  skip_if_not_installed("lme4")
  f <- covariate_plot_fixture()
  m <- suppressMessages(lme4::lmer(y ~ x + (1 | g), data = f$data))
  p <- residuals_covariate_plot(m, smooth = FALSE)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "x")
  expect_equal(p$data$residuals, diagnostic_data(m)$data$residuals)
  expect_error(residuals_covariate_plot(m, terms = "x"), "smooths\\(model\\)")
  slope <- suppressMessages(lme4::lmer(y ~ 1 + (x | g), data = f$data))
  expect_equal(residual_default_covariates(residual_covariate_context(slope)), "x")
})

test_that("glmmTMB diagnostics use raw covariates and exact smooth labels", {
  skip_if_not_installed("glmmTMB")
  f <- covariate_plot_fixture()
  m <- suppressWarnings(glmmTMB::glmmTMB(y ~ s(x, k = 5) + z,
    data = f$data, family = stats::gaussian(), REML = TRUE))
  p <- residuals_covariate_plot(m, terms = smooths(m), smooth = FALSE)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$x, f$data$x)
  expect_equal(p$data$residuals, diagnostic_data(m)$data$residuals)
})

test_that("GJRM smooth labels expand covariates across equations", {
  skip_if_not_installed("GJRM")
  f <- covariate_plot_fixture()
  m <- GJRM::gamlss(list(y ~ s(x, k = 5), ~ s(z, k = 5)),
    data = f$data, family = "N")
  p <- residuals_covariate_plot(m, terms = smooths(m)[2], data = f$data,
    smooth = FALSE)
  expect_equal(p$labels$x, "z")
  expect_equal(p$data$x, f$data$z)
  expect_equal(p$data$residuals, diagnostic_data(m)$data$residuals)
})

test_that("sdmTMB components keep their fitted observation rows", {
  skip_if_not_installed("sdmTMB")
  f <- covariate_plot_fixture()
  m <- sdmTMB::sdmTMB(y ~ s(x, k = 5), data = f$data, spatial = "off")
  p <- residuals_covariate_plot(m, terms = smooths(m), smooth = FALSE,
    field_method = "mle-eb")
  expect_equal(p$data$x, f$data$x)
  d <- f$data
  d$y <- rep(c(0, 1, 2, 3), length.out = nrow(d))
  m <- sdmTMB::sdmTMB(y ~ x, data = d, spatial = "off",
    family = sdmTMB::delta_gamma())
  p <- residuals_covariate_plot(m, covariates = "x", component = "positive",
    field_method = "mle-eb", smooth = FALSE)
  expect_equal(p$data$x, d$x[d$y > 0])
  p <- residuals_covariate_plot(m, covariates = "z", data = d,
    component = "positive", field_method = "mle-eb", smooth = FALSE)
  expect_equal(p$data$x, d$z[d$y > 0])
})

test_that("random-slope covariates remain default diagnostic axes", {
  f <- covariate_plot_fixture()
  m <- mgcv::gam(y ~ s(x, g, bs = "re") + s(z, k = 5),
    data = f$data, method = "REML")
  axes <- residual_default_covariates(residual_covariate_context(m))
  expect_setequal(axes, c("x", "z"))
  expect_false("g" %in% axes)
})
