test_that("conditional utilities agree with independent rejection simulation", {
  withr::local_seed(816)
  mu <- c(0, .4, -.6)
  # The oracle samples unconditional independent Gumbel utilities and retains
  # only those with the specified winner. It does not use the maximum identity.
  oracle <- matrix(rep(mu, each = 250000L), ncol = 3L) -
    log(-log(matrix(runif(750000L), ncol = 3L)))
  for (winner in 0:2) {
    accepted <- oracle[max.col(oracle) == winner + 1L, , drop = FALSE]
    r <- multinom_surrogate_draw(matrix(rep(mu, each = 25000L), ncol = 3L),
      rep(winner, 25000L))
    utility <- sweep(r - digamma(1), 2L, mu, `+`)
    expect_true(all(max.col(utility) == winner + 1L))
    expect_equal(colMeans(utility), colMeans(accepted), tolerance = .035)
    expect_equal(cov(utility), cov(accepted), tolerance = .05)
    for (j in 1:3) expect_equal(unname(quantile(utility[, j], c(.1, .5, .9))),
      unname(quantile(accepted[, j], c(.1, .5, .9))), tolerance = .06)
    # The winning maximum is Gumbel(log(sum(exp(mu))), 1), regardless of label.
    expect_equal(mean(utility[, winner + 1L]), log(sum(exp(mu))) - digamma(1), tolerance = .025)
  }
})

test_that("the known-parameter joint null has centred Gumbel components", {
  withr::local_seed(817)
  n <- 60000L
  x <- runif(n, -2, 2)
  mu <- cbind(0, x, -.3 - x / 2)
  y <- max.col(mu - log(-log(matrix(runif(n * 3L), n)))) - 1L
  r <- multinom_surrogate_draw(mu, y)
  expect_equal(unname(colMeans(r)), rep(0, 3), tolerance = .025)
  expect_equal(unname(cov(r)), diag(pi^2 / 6, 3), tolerance = .045)
  p <- c(.05, .25, .5, .75, .95)
  for (j in 1:3) {
    expect_equal(unname(quantile(r[, j], p)), -log(-log(p)) + digamma(1), tolerance = .05)
    expect_lt(abs(cor(r[, j], x)), .02)
  }
  # A joint CDF check also detects dependence missed by marginal checks.
  expect_equal(mean(rowSums(r <= 0) == 3), exp(-3 * exp(digamma(1))), tolerance = .007)
  # In the binary representation both components are Gumbel; their difference
  # is logistic. It is not a single logistic residual returned twice.
  binary_mu <- cbind(0, x)
  binary_y <- max.col(binary_mu - log(-log(matrix(runif(n * 2L), n)))) - 1L
  b <- multinom_surrogate_draw(binary_mu, binary_y)
  expect_equal(unname(apply(b, 2L, var)), rep(pi^2 / 6, 2), tolerance = .05)
  expect_equal(var(b[, 2L] - b[, 1L]), pi^2 / 3, tolerance = .08)
  expect_equal(unname(quantile(b[, 2L] - b[, 1L], p)), qlogis(p), tolerance = .06)
})

test_that("rare winners, single rows and chunk sizes remain finite and reproducible", {
  mu <- rbind(c(0, -1000, 1000), c(0, 10000, -10000), c(0, -2000, -3000))
  y <- c(1L, 2L, 2L)
  r <- withr::with_seed(1, multinom_surrogate_draw(mu, y, chunk_size = 1L))
  expect_true(all(is.finite(r)))
  u <- r - digamma(1) + mu
  expect_equal(max.col(u) - 1L, y)
  for (chunk in c(2L, 7L, 10000L)) expect_identical(r,
    withr::with_seed(1, multinom_surrogate_draw(mu, y, chunk_size = chunk)))
  one <- withr::with_seed(1, multinom_surrogate_draw(mu[1L, , drop = FALSE], y[1L]))
  expect_equal(dim(one), c(1L, 3L))
  expect_identical(one[1L, ], r[1L, ])
})

test_that("actual multinomial GAMs preserve offsets, labels, rows and RNG", {
  m <- multinom_surrogate_fit()
  withr::local_seed(900)
  old <- .Random.seed
  r <- surrogate_residuals(m, seed = 29)
  expect_identical(.Random.seed, old)
  expect_identical(surrogate_residuals(m, seed = 29), r)
  surrogate_residuals(m)
  expect_false(identical(.Random.seed, old))
  expect_identical(dim(r), c(length(m$y), 3L))
  expect_identical(colnames(r), c("0", "1", "2"))
  mu <- cbind(0, predict(m, type = "link"))
  expect_equal(unname(multinom_utility_locations(m)), unname(mu))
  expect_equal(max.col(r - digamma(1) + mu) - 1L, m$y)
  probabilities <- exp(mu) / rowSums(exp(mu))
  d <- prepare_diagnostic_data(m, "surrogate", seed = 29)
  for (j in 1:3) {
    component <- multinom_diagnostic_component(d, j)
    expect_equal(component$data$fitted, unname(probabilities[, j]))
    expect_equal(component$data$eta, component$data$fitted)
    expect_equal(component$data$residuals, unname(r[, j]))
    expect_equal(component$data$observed, as.numeric(m$y == j - 1L))
  }
  omit <- multinom_surrogate_fit(missing = TRUE)
  exclude <- multinom_surrogate_fit(action = na.exclude, missing = TRUE)
  a <- surrogate_residuals(omit, seed = 29)
  b <- surrogate_residuals(exclude, seed = 29)
  expect_equal(dim(b), c(150L, 3L))
  expect_true(all(is.na(b[c(3, 9), ])))
  expect_equal(b[-c(3, 9), ], a)
  expect_identical(colnames(b), c("0", "1", "2"))
  # A family retains its full choice set even if a category is absent.
  absent <- m
  absent$y[absent$y == 2L] <- 0L
  expect_identical(colnames(surrogate_residuals(absent, seed = 2)), c("0", "1", "2"))
  binary <- multinom_surrogate_fit(k = 1L)
  expect_identical(dim(surrogate_residuals(binary, seed = 29)), c(150L, 2L))
  expect_error(surrogate_residuals(m, conditioning = "marginal"), "conditional")
})

test_that("ordinary and scalable references retain categories and observation identities", {
  m <- multinom_surrogate_fit(action = na.exclude, missing = TRUE)
  d <- prepare_diagnostic_data(m, "surrogate", seed = 29)
  for (worm in c(FALSE, TRUE)) {
    ordinary <- if (worm) worm_plot else qq_plot
    scalable <- if (worm) scalable_worm_plot else scalable_qq_plot
    q <- ordinary(m, type = "surrogate", seed = 29)
    s <- scalable(m, type = "surrogate", seed = 29)
    expect_equal(q$data, s$data)
    expect_match(q$labels$x, "Gumbel")
    expect_match(q$labels$subtitle, "Gumbel")
    for (j in 1:3) {
      part <- q$data[as.integer(q$data$.family) == j, ]
      expected <- sort(d$residuals[, j])
      reference <- -log(-log(ppoints(d$n_obs))) + digamma(1)
      expect_equal(part$residuals, expected - if (worm) reference else 0)
      expect_equal(part$theoretical, reference)
      index <- match(part$.row, d$data$.row)
      expect_equal(d$residuals[index, j], expected)
      expect_equal(part$lower + if (worm) reference else 0,
        centred_gumbel_quantile(qbeta(.05, seq_len(d$n_obs), d$n_obs + 1L - seq_len(d$n_obs))))
    }
    binned <- scalable(m, type = "surrogate", seed = 29, qq_bins = 13)
    expect_true(all(table(binned$data$.family) <= 13))
    expect_identical(levels(binned$data$.family), paste("Category", 0:2))
    expect_plot_render(q)
    expect_plot_render(binned)
  }
  for (fun in list(qq_plot, worm_plot, scalable_qq_plot, scalable_worm_plot))
    expect_error(fun(m, type = "surrogate", method = "normal"), "method = NULL")
})

test_that("all appraisal panels share one draw and dispatch counts observations", {
  m <- multinom_surrogate_fit()
  for (worm in c(FALSE, TRUE)) for (scalable in c(FALSE, TRUE)) {
    withr::local_seed(902)
    expected <- surrogate_residuals(m)
    expected_state <- .Random.seed
    set.seed(902)
    a <- appraise(m, type = "surrogate", scalable = scalable, use_worm = worm,
      qq_bins = 200L, density_method = "histogram")
    expect_identical(.Random.seed, expected_state)
    reference <- centred_gumbel_quantile(ppoints(nrow(expected)))
    for (j in 1:3) {
      q <- a[[1]]$data
      expect_equal(q$residuals[as.integer(q$.family) == j],
        sort(expected[, j]) - if (worm) reference else 0)
      if (!scalable) {
        p <- a[[2]]$data
        expect_equal(p$residuals[as.integer(p$.family) == j], unname(expected[, j]))
      }
      h <- a[[3]]$data
      expect_equal(sum(h$count[as.integer(h$.family) == j]), nrow(expected))
      expected_histogram <- diagnostic_histogram(expected[, j], "sturges")
      expect_equal(h[as.integer(h$.family) == j, c("xmin", "xmax", "count")],
        expected_histogram)
      if (scalable) {
        p <- a[[2]]$data
        mu <- cbind(0, m$linear.predictors)
        probability <- exp(mu[, j]) / rowSums(exp(mu))
        expected_grid <- diagnostic_grid(probability, expected[, j], c(50L, 50L), "conditional")
        expect_equal(p[as.integer(p$.family) == j, names(expected_grid)], expected_grid)
      }
    }
    expect_plot_render(a)
  }
  ordinary <- appraise(m, type = "surrogate", seed = 29, scalable = FALSE)
  below <- appraise(m, type = "surrogate", seed = 29, scalable_threshold = 151L)
  auto <- appraise(m, type = "surrogate", seed = 29, scalable_threshold = 150L)
  above <- appraise(m, type = "surrogate", seed = 29, scalable = TRUE)
  expect_equal(ordinary[[2]]$data, below[[2]]$data)
  expect_equal(auto[[2]]$data, above[[2]]$data)
  for (fun in list(residuals_linpred_plot, residuals_hist_plot,
    scalable_residuals_linpred_plot, scalable_residuals_hist_plot)) {
    p <- fun(m, type = "surrogate", seed = 29)
    expect_identical(levels(p$data$.family), paste("Category", 0:2))
    expect_plot_render(p)
  }
  expect_plot_render(residuals_covariate_plot(m, covariates = "x", type = "surrogate", seed = 29, smooth = FALSE))
  expect_plot_render(scalable_residuals_covariate_plot(m, covariates = "x", type = "surrogate", seed = 29, smooth = FALSE))
})

test_that("large scalable displays retain only component summaries", {
  m <- multinom_surrogate_fit()
  # Reuse a fitted model's covariates and coefficients with many observations;
  # this checks diagnostic memory growth without a large fitting cost.
  index <- rep(seq_len(nrow(m$model)), 150L)
  for (name in c("model", "linear.predictors", "fitted.values", "y", "prior.weights"))
    m[[name]] <- slice_observations(m[[name]], index)
  m$offset <- lapply(m$offset, function(x) x[index])
  d <- prepare_diagnostic_data(m, "surrogate", seed = 29)
  expect_equal(nrow(d$data), length(index))
  expect_equal(nrow(d$residuals), length(index))
  q <- scalable_qq_plot(d, qq_bins = 31L)
  p <- scalable_residuals_linpred_plot(d, density_bins = 15L, density_method = "histogram")
  h <- scalable_residuals_hist_plot(d, n_bins = 20L)
  expect_lte(nrow(q$data), 3L * 31L)
  expect_lte(nrow(p$data), 3L * 15L^2)
  expect_lte(nrow(h$data), 3L * 21L)
  expect_equal(sum(h$data$count), length(index) * 3L)
  expect_lt(as.numeric(object.size(p$data)), 100000)
})

test_that("prepared multinomial diagnostics reuse draws in every public path", {
  m <- multinom_surrogate_fit()
  withr::local_seed(903)
  d <- prepare_diagnostic_data(m, "surrogate", seed = 29)
  state <- .Random.seed
  reference <- diagnostic_qq_data(d$source, d)
  q <- qq_plot(d)
  expect_equal(q$data, reference$data)
  expect_equal(residuals_linpred_plot(d)$data$residuals, as.vector(d$residuals))
  expect_equal(qq_plot(d)$data, qq_plot(m, type = "surrogate", seed = 29)$data)
  expect_equal(appraise(d, scalable = FALSE)[[1]]$data, q$data)
  expect_equal(appraise(d, scalable = TRUE, qq_bins = 200L)[[1]]$data, q$data)
  expect_identical(.Random.seed, state)
  expect_error(qq_plot(d, type = "deviance"), "Prepared diagnostic data")
  expect_error(residuals_hist_plot(d, conditioning = "marginal"), "conditional")
  # The option controls grouped-family pooling only; utilities stay separate.
  expect_equal(qq_plot(d, by_family = FALSE)$data, q$data)
  expect_equal(scalable_qq_plot(d, by_family = FALSE)$data, q$data)
  augmented <- add_residuals(m$model, m, type = "surrogate", seed = 29)
  expect_equal(augmented$.residual, d$residuals)
  # One fitting observation still has all K+1 utility components.
  one <- m
  for (name in c("model", "linear.predictors", "fitted.values", "y", "prior.weights"))
    one[[name]] <- slice_observations(one[[name]], 1L)
  one$offset <- lapply(one$offset, function(x) x[1L])
  expect_equal(dim(surrogate_residuals(one, seed = 29)), c(1L, 3L))
  expect_plot_render(qq_plot(one, type = "surrogate", seed = 29))
  expect_plot_render(appraise(one, type = "surrogate", seed = 29, scalable = TRUE))
})

test_that("fitted families retain categories absent from the fitting sample", {
  withr::local_seed(1)
  dat <- data.frame(y = sample(0:1, 100L, TRUE), x = runif(100L))
  # The unused category's fitted intercept can approach minus infinity; mgcv
  # can warn about convergence. The fitted locations here remain finite.
  m <- suppressWarnings(mgcv::gam(list(y ~ x, ~ x), data = dat,
    family = mgcv::multinom(K = 2)))
  expect_false(2L %in% m$y)
  r <- surrogate_residuals(m, seed = 29)
  expect_identical(colnames(r), c("0", "1", "2"))
  expect_true(all(is.finite(r)))
  expect_plot_render(appraise(m, type = "surrogate", seed = 29, scalable = TRUE))
})

test_that("categorical distribution hooks leave utility surrogates unchanged", {
  m <- multinom_surrogate_fit()
  seed <- 29L
  r <- surrogate_residuals(m, seed = seed)
  categorical <- quantile_residuals(m, type = "quantile", seed = seed)
  expect_length(categorical, nrow(r))
  expect_null(dim(categorical))
  expect_equal(dim(r), c(length(m$y), 3L))

  # The categorical CDF and inverse CDF operate on a scalar enumeration of
  # outcomes. Utility draws use the fitted locations and joint choice event.
  custom <- m
  custom$family$cdf <- function(...) stop("Categorical CDF was called")
  custom$family$qf <- function(...) stop("Categorical inverse CDF was called")
  expect_identical(surrogate_residuals(custom, seed = seed), r)
  for (scalable in c(FALSE, TRUE)) {
    original <- appraise(m, type = "surrogate", seed = seed, scalable = scalable,
      density_method = "histogram")
    supplied <- appraise(custom, type = "surrogate", seed = seed,
      scalable = scalable, density_method = "histogram")
    for (panel in seq_len(4L)) expect_equal(supplied[[panel]]$data, original[[panel]]$data)
  }
})
