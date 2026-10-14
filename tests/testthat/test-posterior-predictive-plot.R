test_that("posterior predictive curves retain the posterior draws", {
  p <- posterior_predictive_plot(m_tiny_eg1, n = 3, seed = 42)
  sims <- posterior_samples(m_tiny_eg1, n = 3, seed = 42)
  expect_s3_class(p, "ggplot")
  replicated <- p$data[p$data$.draw > 0L, ]
  expect_equal(
    replicated[names(sims)], tibble::as_tibble(sims), ignore_attr = TRUE
  )
  expect_equal(p$data$.response[p$data$.draw == 0L], m_tiny_eg1$y)
  curves <- p$layers[[1L]]$data
  observed <- p$layers[[2L]]$data
  expect_equal(sort(unique(curves$.draw)), 1:3)
  grids <- split(curves$.response, curves$.draw)
  expect_equal(grids[[1L]], observed$.response)
  expect_equal(grids[[2L]], observed$.response)
  estimate <- stats::density(m_tiny_eg1$y,
    bw = stats::bw.nrd0(m_tiny_eg1$y),
    from = min(observed$.response), to = max(observed$.response), n = 512
  )
  expect_equal(observed$.density, estimate$y)
  expect_silent(ggplot2::ggplot_build(p))

  # Sampling options must reach the existing posterior sampler.
  p <- posterior_predictive_plot(m_tiny_eg1,
    n = 2, seed = 5,
    unconditional = TRUE, mvn_method = "mgcv"
  )
  sims <- posterior_samples(m_tiny_eg1,
    n = 2, seed = 5,
    unconditional = TRUE, mvn_method = "mgcv"
  )
  expect_equal(p$data$.response[p$data$.draw > 0L], sims$.response)
})

test_that("discrete comparisons summarize replicated masses on shared support", {
  for (m in list(b_pois, m_nb, m_ocat, m_multinom)) {
    p <- posterior_predictive_plot(m, n = 3, seed = 12)
    expect_equal(p$labels$y, "Probability")
    intervals <- p$layers[[1L]]$data
    observed <- p$layers[[2L]]$data
    support <- sort(unique(p$data$.response))
    masses <- vapply(1:3, function(draw) {
      raw <- p$data$.response[p$data$.draw == draw]
      tabulate(match(raw, support), nbins = length(support)) / length(raw)
    }, numeric(length(support)))
    expect_equal(colSums(masses), rep(1, 3))
    expect_equal(intervals$.response, support)
    expect_equal(intervals$.density, apply(masses, 1, median))
    expect_equal(intervals$.lower,
      apply(masses, 1, quantile, probs = 0.05, names = FALSE))
    expect_equal(intervals$.upper,
      apply(masses, 1, quantile, probs = 0.95, names = FALSE))
    raw <- p$data$.response[p$data$.draw == 0]
    expect_equal(observed$.response, support)
    expect_equal(observed$.density,
      tabulate(match(raw, support), nbins = length(support)) / length(raw))
    expect_s3_class(p$layers[[1L]]$geom, "GeomPointrange")
    expect_s3_class(p$layers[[2L]]$geom, "GeomPoint")
    expect_silent(built <- ggplot2::ggplot_build(p))
    expect_equal(built$data[[1L]]$x, support)
    expect_equal(built$data[[2L]]$x, support)
  }
})

test_that("discrete intervals honor coverage and handle a single draw", {
  p <- posterior_predictive_plot(b_pois, n = 20, seed = 12, level = 0.5)
  masses <- posterior_predictive_curves(p$data[p$data$.draw > 0L, ], TRUE)
  counts <- split(masses$.density, masses$.response)
  expect_equal(p$layers[[1L]]$data$.lower,
    unname(vapply(counts, quantile, numeric(1), probs = 0.25)))
  expect_equal(p$layers[[1L]]$data$.upper,
    unname(vapply(counts, quantile, numeric(1), probs = 0.75)))
  p <- posterior_predictive_plot(b_pois, n = 1, seed = 12)
  intervals <- p$layers[[1L]]$data
  expect_equal(intervals$.lower, intervals$.density)
  expect_equal(intervals$.upper, intervals$.density)
  expect_silent(ggplot2::ggplot_build(p))

  m <- b_pois
  m$y[] <- 0
  testthat::local_mocked_bindings(posterior_samples = function(model, n, ...) {
    tibble::tibble(.row = rep(seq_along(model$y), n),
      .draw = rep(seq_len(n), each = length(model$y)), .response = 0)
  })
  p <- posterior_predictive_plot(m, n = 2)
  expect_equal(p$layers[[1L]]$data$.density, 1)
  expect_equal(p$layers[[1L]]$data$.lower, 1)
  expect_equal(p$layers[[1L]]$data$.upper, 1)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("binomial responses match the sampling scale", {
  withr::local_seed(15)
  d <- data.frame(x = runif(60), trials = rep(c(3, 8), 30))
  d$success <- rbinom(60, d$trials, plogis(d$x))
  d$failure <- d$trials - d$success
  counts <- gam(cbind(success, failure) ~ s(x, k = 5),
    data = d, family = binomial()
  )
  proportions <- gam(success / trials ~ s(x, k = 5),
    weights = trials,
    data = d, family = binomial()
  )
  for (m in list(counts, proportions)) {
    p <- posterior_predictive_plot(m, n = 3, seed = 2)
    expect_equal(p$data$.response[p$data$.draw == 0L], d$success / d$trials)
    sims <- posterior_samples(m, n = 3, seed = 2)
    expect_equal(p$data$.response[p$data$.draw > 0L], sims$.response)
    expect_true(all(p$data$.response >= 0 & p$data$.response <= 1))
    built <- ggplot2::ggplot_build(p)
    expect_equal(built$data[[1L]]$x, built$data[[2L]]$x)
    expect_equal(built$data[[1L]]$x, p$layers[[1L]]$data$.response)
  }
  d$binary <- rbinom(60, 1, 0.5)
  d$category <- factor(d$binary, labels = c("no", "yes"))
  for (response in c("binary", "category")) {
    m <- gam(reformulate("s(x, k = 5)", response = response),
      data = d, family = binomial()
    )
    p <- posterior_predictive_plot(m, n = 3, seed = 2)
    expect_equal(p$data$.response[p$data$.draw == 0L], as.numeric(d$binary))
    expect_true(all(p$data$.response %in% c(0, 1)))
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("missing fitting rows, subsets and offsets stay aligned", {
  withr::local_seed(2)
  d <- data.frame(x = runif(70), off = runif(70), y = rnorm(70))
  d$x[c(3, 15, 60)] <- NA
  d$y[20] <- NA
  for (action in list(na.omit, na.exclude)) {
    m <- gam(y ~ s(x, k = 5) + offset(off),
      data = d,
      subset = x < 0.9 | is.na(x), na.action = action
    )
    p <- posterior_predictive_plot(m, n = 3, seed = 10)
    rows <- !is.na(model_row_map(m))
    expect_equal(p$data$.row[p$data$.draw == 0L], which(rows))
    expect_equal(p$data$.response[p$data$.draw == 0L], m$y)
    sims <- posterior_samples(m, n = 3, seed = 10)
    sims <- sims[is.finite(sims$.response), ]
    expect_equal(p$data$.response[p$data$.draw > 0L], sims$.response)
    expect_equal(p$data$.row[p$data$.draw > 0L], sims$.row)
  }
})

test_that("multivariate responses have correctly aligned facets", {
  p <- posterior_predictive_plot(m_mvn, n = 2, seed = 18)
  sims <- posterior_samples(m_mvn, n = 2, seed = 18)
  expect_equal(p$data[p$data$.draw > 0L, names(sims)], tibble::as_tibble(sims))
  for (i in seq_len(ncol(m_mvn$y))) {
    observed <- p$data[p$data$.draw == 0L &
      p$data$.parameter == paste0("response", i), ]
    expect_equal(observed$.response, unname(m_mvn$y[, i]))
  }
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$layout$layout), ncol(m_mvn$y))
  expect_equal(length(unique(built$data[[1L]]$PANEL)), ncol(m_mvn$y))
})

test_that("posterior predictive plots support compatible model wrappers", {
  for (m in list(m_bam, m_scam, gamm4_gaussian)) {
    p <- posterior_predictive_plot(m, n = 2, seed = 4)
    expect_s3_class(p, "ggplot")
    sims <- posterior_samples(m, n = 2, seed = 4)
    expect_equal(p$data$.response[p$data$.draw > 0L], sims$.response)
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("seeded checks preserve RNG state and remain reproducible", {
  withr::local_seed(16)
  before <- .Random.seed
  p <- posterior_predictive_plot(m_tiny_eg1, n = 2, seed = 8)
  expect_identical(.Random.seed, before)
  expect_equal(
    p$data, posterior_predictive_plot(m_tiny_eg1, n = 2, seed = 8)$data
  )
  posterior_predictive_plot(m_tiny_eg1, n = 2)
  expect_false(identical(.Random.seed, before))
  rm(".Random.seed", envir = .GlobalEnv)
  posterior_predictive_plot(m_tiny_eg1, n = 2, seed = 8)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("invalid and unsupported posterior comparisons fail clearly", {
  for (n in list(0, -1, 1.5, NA, NaN, Inf, numeric(), c(1, 2), "2", TRUE)) {
    expect_error(posterior_predictive_plot(m_tiny_eg1, n = n),
      "'n' must be a positive integer",
      fixed = TRUE
    )
  }
  for (level in list(0, 1, -0.1, NA, NaN, Inf, numeric(), c(0.5, 0.9), "0.9")) {
    expect_error(posterior_predictive_plot(b_pois, level = level),
      "'level' must be a number strictly between zero and one", fixed = TRUE)
  }
  for (m in list(1)) {
    expect_error(posterior_predictive_plot(m), "not available for")
  }
  for (m in list(m_censor)) {
    expect_error(posterior_predictive_plot(m), "not available for family")
    expect_error(appraise(m, ppcheck = TRUE), "not available for family")
  }
  expect_error(
    posterior_predictive_plot(m_tiny_eg1, data = tiny_eg1),
    "fitting observations"
  )
  for (value in list(NA_real_, Inf)) {
    m <- m_tiny_eg1
    m$y[] <- value
    expect_error(posterior_predictive_plot(m), "finite observed responses")
  }
  m <- m_tiny_eg1
  m$y[] <- 1
  p <- posterior_predictive_plot(m, n = 1, seed = 1)
  expect_silent(ggplot2::ggplot_build(p))
  m$y[-1L] <- NA_real_
  expect_error(posterior_predictive_plot(m), "at least two observations")
})

test_that("invalid posterior draws cannot silently change the compared rows", {
  for (problem in c("nonfinite", "missing", "duplicate")) {
    testthat::local_mocked_bindings(posterior_samples = function(model, n, ...) {
      out <- tibble::tibble(
        .row = seq_along(model$y), .draw = 1L,
        .response = model$y
      )
      if (problem == "nonfinite") out$.response[1L] <- NA_real_
      if (problem == "missing") out <- out[-1L, ]
      if (problem == "duplicate") out$.row[1L] <- out$.row[2L]
      out
    })
    expect_error(
      posterior_predictive_plot(m_tiny_eg1, n = 1),
      "one finite response per fitting observation and draw"
    )
  }
})

test_that("appraise replaces only its histogram when ppcheck is enabled", {
  defaults <- appraise(m_tiny_eg1, seed = 4)
  explicit <- appraise(m_tiny_eg1, ppcheck = FALSE, seed = 4, n_ppc = 0)
  for (i in 1:4) {
    expect_equal(defaults[[i]]$data, explicit[[i]]$data)
    expect_equal(
      lapply(defaults[[i]]$layers, function(x) x$aes_params),
      lapply(explicit[[i]]$layers, function(x) x$aes_params)
    )
  }
  for (worm in c(FALSE, TRUE)) {
    p <- appraise(m_tiny_eg1,
      ppcheck = TRUE, n_ppc = 3,
      n_simulate = 2, method = "simulate", seed = 4, use_worm = worm,
      line_col = "navy", ci_col = "orange", ci_alpha = 0.3,
      ncol = 1, guides = "collect"
    )
    expect_length(p$patches$plots, 3L)
    check <- p$patches$plots[[3L]]
    expect_equal(sort(unique(check$data$.draw)), 0:3)
    expect_equal(check$layers[[1L]]$aes_params$colour, "orange")
    expect_equal(check$layers[[1L]]$aes_params$alpha, 0.3)
    expect_equal(check$layers[[2L]]$aes_params$colour, "navy")
    expect_equal(p$patches$layout$ncol, 1)
    expect_equal(p$patches$layout$guides, "collect")
    expect_equal(p$data, defaults$data)
    expect_equal(p$patches$plots[[2L]]$data, defaults$patches$plots[[2L]]$data)
    qq_fun <- if (worm) worm_plot else qq_plot
    reference <- qq_fun(m_tiny_eg1,
      method = "simulate", n_simulate = 2, seed = 4
    )
    expect_equal(p$patches$plots[[1L]]$data, reference$data)
  }
  for (bad in list(NA, 1, "yes", logical(), c(TRUE, FALSE))) {
    expect_error(
      appraise(m_tiny_eg1, ppcheck = bad), "'ppcheck' must be TRUE or FALSE",
      fixed = TRUE
    )
  }
  for (m in list(m_lm, m_glm)) {
    expect_s3_class(appraise(m, ppcheck = TRUE, n_ppc = 2, seed = 1), "patchwork")
  }
  expect_error(
    appraise(m_tiny_eg1, ppcheck = TRUE, n_ppc = 0), "positive integer"
  )
})

test_that("posterior predictive plot labels are customizable", {
  p <- posterior_predictive_plot(m_tiny_eg1,
    n = 2, seed = 4,
    xlab = "Outcome", ylab = "Frequency", title = "Check",
    subtitle = "Replicated data", caption = "Example"
  )
  expect_equal(p$labels$x, "Outcome")
  expect_equal(p$labels$y, "Frequency")
  expect_equal(p$labels$title, "Check")
  expect_equal(p$labels$subtitle, "Replicated data")
  expect_equal(p$labels$caption, "Example")
})

test_that("appraise forwards discrete posterior predictive interval settings", {
  p <- appraise(b_pois, ppcheck = TRUE, n_ppc = 20, seed = 12, level = 0.5)
  check <- posterior_predictive_plot(b_pois, n = 20, seed = 12, level = 0.5)
  expect_equal(p$patches$plots[[3L]]$layers[[1L]]$data, check$layers[[1L]]$data)
  expect_equal(p$patches$plots[[3L]]$layers[[1L]]$aes_params$alpha,
    check$layers[[1L]]$aes_params$alpha)
  p <- appraise(b_pois, ppcheck = TRUE, n_ppc = 2, seed = 12,
    ci_alpha = 0.45, method = "simulate", n_simulate = 2)
  check <- posterior_predictive_plot(b_pois, n = 2, seed = 12,
    replicated_alpha = 0.45)
  expect_equal(p$patches$plots[[3L]]$layers[[1L]]$aes_params$alpha,
    check$layers[[1L]]$aes_params$alpha)
  expect_equal(p$patches$plots[[1L]]$layers[[2L]]$aes_params$alpha, 0.45)
})

test_that("posterior predictive checks replace histograms in scalable appraisals", {
  expected <- posterior_predictive_plot(m_tiny_eg1, n = 2, seed = 12)
  for (scalable in list(FALSE, TRUE, NULL)) {
    p <- appraise(m_tiny_eg1, ppcheck = TRUE, n_ppc = 2, seed = 12,
      scalable = scalable, scalable_threshold = 1, qq_bins = 20,
      method = "normal", use_worm = TRUE)
    check <- posterior_predictive_plot(m_tiny_eg1, n = 2, seed = 12,
      scalable = scalable, scalable_threshold = 1)
    expect_equal(p[[3L]]$data, check$data)
    if (identical(scalable, FALSE)) {
      expect_true("eta" %in% names(p[[2L]]$data))
    } else {
      expect_true("value" %in% names(p[[2L]]$data))
      expect_lte(nrow(p[[1L]]$data), 20L)
    }
    expect_s3_class(p, "patchwork")
  }
})

test_that("posterior predictive plots have stable visual output", {
  expect_doppelganger(
    "posterior predictive continuous",
    posterior_predictive_plot(m_tiny_eg1, n = 10, seed = 42)
  )
  expect_doppelganger(
    "posterior predictive discrete",
    posterior_predictive_plot(m_nb, n = 10, seed = 42)
  )
  expect_doppelganger(
    "posterior predictive multivariate",
    posterior_predictive_plot(m_mvn, n = 10, seed = 42)
  )
  expect_doppelganger(
    "appraise posterior predictive check",
    appraise(m_tiny_eg1, ppcheck = TRUE, n_ppc = 10, seed = 42)
  )
})
