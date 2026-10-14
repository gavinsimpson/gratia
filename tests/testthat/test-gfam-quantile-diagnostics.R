# Independent ordinary-family Dunn--Smyth calculations. Randomize only the CDF
# jumps, in indexed-component order, using fitted scalar dispersion explicitly.
gfam_quantile_oracle <- function(model, type = "quantile") {
  model <- model_used_rows(model)
  y <- model$y
  mu <- model$fitted.values
  wt <- model$prior.weights
  fi <- attr(model$family$getfl(), "fi")
  scale <- exp(model$family$getTheta()[1L])
  out <- numeric(length(y))
  i <- which(fi == 1L)
  lower <- pbinom(round(y[i] * wt[i]) - 1, wt[i], mu[i])
  upper <- pbinom(round(y[i] * wt[i]), wt[i], mu[i])
  out[i] <- lower + runif(length(i)) * (upper - lower)
  i <- which(fi == 2L)
  out[i] <- pnorm(y[i], mu[i], sqrt(scale / wt[i]))
  i <- which(fi == 3L)
  lower <- ppois(y[i] - 1, mu[i]); upper <- ppois(y[i], mu[i])
  out[i] <- lower + runif(length(i)) * (upper - lower)
  if (type == "quantile") out <- qnorm(out)
  restore_model_rows(out, model)
}

test_that("grouped quantile and PIT residuals match independent row-aligned calculations", {
  for (model in list(gfam_model, gfam_bam, gfam_matrix_model, gfam_missing_model)) {
    for (type in c("pit", "quantile")) {
      expected <- withr::with_seed(267, gfam_quantile_oracle(model, type))
      actual <- quantile_residuals(model, type = type, seed = 267)
      expect_equal(actual, restore_model_rows(expected, model), tolerance = 1e-10)
    }
    expect_identical(diagnostic_type(model), "quantile")
  }
})

test_that("default calibrated QQ and worm use a fixed normal reference and pointwise bands", {
  for (model in list(gfam_model, gfam_bam, gfam_missing_model)) {
    r <- quantile_residuals(model, type = "quantile", seed = 23)
    included <- which(is.finite(r))
    n <- length(included); i <- seq_len(n)
    z <- qnorm(ppoints(n))
    qq <- qq_plot(model, seed = 23, level = .8)
    worm <- worm_plot(model, seed = 23, level = .8)
    expect_equal(qq$data$theoretical, z)
    expect_equal(qq$data$residuals, sort(r[included]))
    expect_equal(qq$data$lower, qnorm(qbeta(.1, i, n + 1 - i)))
    expect_equal(qq$data$upper, qnorm(qbeta(.9, i, n + 1 - i)))
    expect_equal(worm$data$residuals, qq$data$residuals - z)
    expect_equal(worm$data$lower, qq$data$lower - z)
    expect_equal(worm$data$upper, qq$data$upper - z)
    line <- ggplot2::ggplot_build(qq)$data[[1L]]
    expect_equal(line$slope, 1)
    expect_equal(line$intercept, 0)
    expect_equal(qq_plot(model, method = "normal", seed = 23)$data,
      qq_plot(model, seed = 23)$data)
    for (method in c("uniform", "simulate", "direct")) {
      expect_error(qq_plot(model, method = method), "type = \"deviance\"")
      expect_error(worm_plot(model, method = method), "type = \"deviance\"")
    }
    expect_equal(nrow(ggplot2::ggplot_build(qq)$layout$layout), 1L)
  }
})

test_that("component identity follows QQ sorting and indexed colours distinguish repeated families", {
  for (model in list(gfam_model, gfam_gaussian_model, gfam_rank_model)) {
    used <- model_used_rows(model)
    r <- quantile_residuals(used, type = "quantile", seed = 29)
    index <- attr(model$family$getfl(), "fi")
    qq <- qq_plot(model, seed = 29)
    expect_identical(as.integer(qq$data$.family), as.integer(index[order(r)]))
    points <- ggplot2::ggplot_build(qq)$data[[3L]]
    expect_equal(length(unique(points$colour)), length(unique(index)))
    faceted <- qq_plot(model, seed = 29, by_family = TRUE)
    expect_equal(nrow(ggplot2::ggplot_build(faceted)$layout$layout), length(unique(index)))
    for (i in unique(index)) {
      p <- faceted$data[as.integer(faceted$data$.family) == i, ]
      expect_equal(p$theoretical, qnorm(ppoints(sum(index == i))))
      expect_equal(p$residuals, sort(r[index == i]))
    }
  }
})

test_that("grouped diagnostics share stable Okabe-Ito component colours and fills", {
  for (model in list(gfam_model, gfam_gaussian_model, gfam_missing_model)) {
    labels <- gfam_diagnostic_labels(model)
    expected <- ggokabeito::palette_okabe_ito()[seq_along(labels)]
    plots <- list(qq_plot(model, seed = 267), worm_plot(model, seed = 267),
      residuals_linpred_plot(model, seed = 267), residuals_hist_plot(model, seed = 267),
      residuals_linpred_plot(model, type = "deviance", by_family = FALSE),
      residuals_hist_plot(model, type = "deviance", by_family = FALSE))
    for (p in plots) {
      aesthetic <- if (inherits(p$layers[[1L]]$geom, "GeomBar")) "fill" else "colour"
      built <- ggplot2::ggplot_build(p)
      scale <- built$plot$scales$get_scales(aesthetic)
      expect_equal(unname(scale$map(labels)), expected)
      expect_identical(scale$get_limits(), labels)
      # Remove an entire component and drop its factor level: the remaining
      # component identities must retain their colours across diagnostic panels.
      p$data <- p$data[!is.na(p$data$.family) & p$data$.family != labels[[1L]], ]
      p$data$.family <- droplevels(p$data$.family)
      subset <- ggplot2::ggplot_build(p)
      expect_equal(unname(subset$plot$scales$get_scales(aesthetic)$map(labels[-1L])),
        expected[-1L])
      layer <- subset$data[[length(p$layers)]]
      # Pooled histogram fills and outlines share the component palette.
      layer_aesthetic <- if (inherits(p$layers[[length(p$layers)]]$geom,
        "GeomStep")) "colour" else aesthetic
      expect_true(all(unique(layer[[layer_aesthetic]]) %in% expected[-1L]))
    }
  }
})

test_that("appraisal reuses one seeded residual vector across all residual panels", {
  for (model in list(gfam_model, gfam_missing_model, gfam_rank_model)) {
    set.seed(267)
    before <- .Random.seed
    r <- quantile_residuals(model_used_rows(model), type = "quantile", seed = 37)
    app <- appraise(model, seed = 37)
    expect_identical(.Random.seed, before)
    expect_equal(app[[1L]]$data$residuals, sort(r))
    expect_equal(app[[2L]]$data$residuals[!is.na(app[[2L]]$data$residuals)], r)
    expect_equal(app[[3L]]$data$residuals[!is.na(app[[3L]]$data$residuals)], r)
    expect_equal(nrow(ggplot2::ggplot_build(app[[4L]])$layout$layout),
      length(unique(attr(model$family$getfl(), "fi"))))
    # Unseeded appraisal must consume exactly one vector's randomisation.
    set.seed(37)
    quantile_residuals(model, type = "quantile")
    expected_seed <- .Random.seed
    set.seed(37)
    appraise(model)
    expect_identical(.Random.seed, expected_seed)
    expect_null(attr(model, "gratia.diagnostics"))
  }
})

test_that("pooled predictor ranks are within-component and respect included rows and ties", {
  model <- gfam_rank_model
  used <- model_used_rows(model)
  index <- attr(model$family$getfl(), "fi")
  expected <- numeric(length(index))
  for (i in unique(index)) {
    rows <- which(index == i)
    expected[rows] <- (rank(used$linear.predictors[rows], ties.method = "average") - .5) /
      length(rows)
  }
  p <- residuals_linpred_plot(model, seed = 31)
  expect_equal(p$data$eta[!is.na(p$data$eta)], expected)
  expect_true(all(p$data$eta[as.integer(p$data$.family) == 3L & !is.na(p$data$.family)] == .5))
  expect_identical(p$labels$x, "Within-component linear predictor rank")
  expect_equal(length(unique(ggplot2::ggplot_build(p)$data[[2L]]$colour)), 3L)
  expect_equal(residuals_linpred_plot(model, seed = 31, rank = FALSE)$data$eta,
    restore_model_rows(used$linear.predictors, model))
  expect_equal(residuals_linpred_plot(model, seed = 31, by_family = TRUE)$data$eta,
    used$linear.predictors)
  expect_equal(residuals_linpred_plot(model, seed = 31, by_family = TRUE, rank = TRUE)$data$eta,
    expected)
  for (type in c("deviance", "pearson", "response")) {
    p <- residuals_linpred_plot(model, type = type, by_family = FALSE)
    expect_equal(p$data$eta[!is.na(p$data$eta)], expected)
    expect_equal(residuals_linpred_plot(model, type = type)$data$eta, used$linear.predictors)
  }
  data <- data.frame(eta = c(1, 1, 2, NA, 100, 100, 100),
    residuals = c(0, 0, Inf, 0, 0, NA, 0),
    .family = factor(c(1, 1, 1, 1, 2, 2, 2)))
  expect_equal(diagnostic_predictor_data(data, model, FALSE)$eta,
    c(.5, .5, NA, NA, .5, NA, .5))
  expect_error(residuals_linpred_plot(model, rank = c(TRUE, FALSE)), "'rank'")
})

test_that("pooled histograms use common bin edges and component-specific density", {
  p <- residuals_hist_plot(gfam_rank_model, seed = 43, n_bins = 8)
  built <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_identical(p$labels$y, "Density")
  edges <- split(built[c("xmin", "xmax")], built$group)
  expect_true(all(vapply(edges[-1L], function(x)
    identical(as.numeric(as.matrix(x)), as.numeric(as.matrix(edges[[1L]]))), logical(1))))
  integrals <- tapply(built$density * (built$xmax - built$xmin), built$group, sum)
  expect_equal(as.numeric(integrals), rep(1, 3))
  expect_equal(length(unique(built$fill)), 3L)
  expect_equal(nrow(ggplot2::ggplot_build(residuals_hist_plot(gfam_rank_model,
    type = "response"))$layout$layout), 3L)
})

test_that("unsupported quantile configurations fallback consistently or error when requested", {
  expect_identical(diagnostic_type(gfam_ordinal_model), "quantile")
  expect_true(all(is.finite(quantile_residuals(gfam_ordinal_model, type = "quantile"))))
  expect_s3_class(qq_plot(gfam_ordinal_model, type = "quantile"), "ggplot")
  model <- gfam_gamma_model
  model$prior.weights[attr(model$family$getfl(), "fi") == 1L] <- 2
  expect_identical(diagnostic_type(model), "quantile")
  expect_true(all(is.finite(quantile_residuals(model))))
  model <- gfam_model
  model$prior.weights[1L] <- 1.5
  expect_error(qq_plot(model), "integer trial counts")
  model <- gfam_model
  model$y[1L] <- .2
  expect_error(qq_plot(model), "valid proportions")
  model <- gfam_model
  model$prior.weights[1L] <- NA_real_
  expect_error(qq_plot(model), "Invalid grouped-family weights")
  families <- gfam_model$family$getfl()
  families[[2L]]$cdf <- function(q, mu, wt, scale, log_p = FALSE) rep(NaN, length(mu))
  model <- gfam_model
  model$family$getfl <- function() families
  expect_error(qq_plot(model), "invalid probabilities")
  families[[2L]]$cdf <- function(q, mu, wt, scale, log_p = FALSE) rep(1, length(mu))
  expect_error(qq_plot(model), "Non-finite quantile residuals.*component 2")
})

test_that("unit-weight Gamma, beta and scaled-t use fitted distribution parameters", {
  model <- gfam_gamma_model
  theta <- model$family$getTheta()
  i <- which(attr(model$family$getfl(), "fi") == 1L)
  expected <- qnorm(pgamma(model$y[i], shape = 1 / exp(theta[1L]),
    scale = model$fitted.values[i] * exp(theta[1L])))
  expect_equal(quantile_residuals(model, type = "quantile", seed = 7)[i], expected)
  for (name in names(gfam_custom_models)) {
    model <- gfam_custom_models[[name]]
    i <- which(attr(model$family$getfl(), "fi") == 1L)
    theta <- model$family$getTheta()
    mu <- model$fitted.values[i]
    expected <- if (name == "betar") {
      qnorm(pbeta(model$y[i], mu * exp(theta[1L]), (1 - mu) * exp(theta[1L])))
    } else qnorm(pt((model$y[i] - mu) / exp(theta[2L]), df = 3 + exp(theta[1L])))
    expect_equal(quantile_residuals(model, type = "quantile", seed = 7)[i], expected,
      tolerance = 1e-9)
  }
})

test_that("Tweedie zero atom is randomized using its independent compound-Poisson mass", {
  mu <- c(.8, 1.2, 2, 1.5)
  phi <- .7; power <- 1.5
  y <- c(0, 0, 1, 0)
  lambda <- mu^(2 - power) / (phi * (2 - power))
  zeros <- which(y == 0)
  expected <- withr::with_seed(91, runif(length(zeros))) * exp(-lambda[zeros])
  family <- mgcv::Tweedie(p = power)
  pit <- withr::with_seed(91, do_quantile_residuals(y, mu, rep(1, 4), phi, family, "pit"))
  expect_equal(pit[zeros], expected, tolerance = 1e-12)
  quant <- withr::with_seed(91, do_quantile_residuals(y, mu, rep(1, 4), phi, family, "quantile"))
  expect_equal(quant[zeros], qnorm(expected), tolerance = 1e-12)
  # Sum the positive-response compound-Poisson/Gamma mixture independently.
  n <- 1:100
  alpha <- (2 - power) / (power - 1)
  beta <- phi * (power - 1) * mu[3L]^(power - 1)
  positive <- exp(-lambda[3L]) + sum(dpois(n, lambda[3L]) * pgamma(y[3L], n * alpha, scale = beta))
  expect_equal(pit[3L], positive, tolerance = 1e-12)
})

test_that("quantile defaults preserve absent RNG states as well as ordinary-model behaviour", {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) {
    old <- .Random.seed
    on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
    rm(".Random.seed", envir = .GlobalEnv)
  }
  for (fun in list(qq_plot, worm_plot, appraise, residuals_linpred_plot, residuals_hist_plot)) {
    fun(gfam_model, seed = 6)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
  expect_identical(diagnostic_type(m_tiny_eg1), "deviance")
  expect_identical(diagnostic_by_family(m_tiny_eg1, "deviance"), FALSE)
  expect_equal(residuals_linpred_plot(m_tiny_eg1)$data,
    residuals_linpred_plot(m_tiny_eg1, type = "deviance", rank = FALSE)$data)
})

test_that("default quantile and explicit traditional displays have visual coverage", {
  vdiffr::expect_doppelganger("grouped-family calibrated QQ", qq_plot(gfam_model, seed = 267))
  vdiffr::expect_doppelganger("grouped-family calibrated worm", worm_plot(gfam_model, seed = 267))
  vdiffr::expect_doppelganger("grouped-family calibrated appraisal", appraise(gfam_model, seed = 267))
  vdiffr::expect_doppelganger("grouped-family rank pooled appraisal", appraise(gfam_rank_model, seed = 267))
})


test_that("negative-binomial fitted shapes and Tweedie components have independent CDF oracles", {
  model <- gfam_extended_model
  index <- attr(model$family$getfl(), "fi")
  theta <- model$family$getTheta()
  mu <- model$fitted.values
  expected <- numeric(length(model$y))
  expected <- withr::with_seed(53, {
    for (i in c(1L, 3L)) {
      rows <- which(index == i)
      size <- exp(theta[if (i == 1L) 1L else 3L])
      lower <- pnbinom(model$y[rows] - 1, size = size, mu = mu[rows])
      upper <- pnbinom(model$y[rows], size = size, mu = mu[rows])
      expected[rows] <- lower + runif(length(rows)) * (upper - lower)
    }
    rows <- which(index == 2L)
    expected[rows] <- pnorm(model$y[rows], mu[rows], sqrt(exp(theta[2L])))
    rows <- which(index == 4L)
    power <- 1.01 + (1.99 - 1.01) * plogis(theta[4L])
    phi <- exp(theta[5L])
    lambda <- mu[rows]^(2 - power) / (phi * (2 - power))
    alpha <- (2 - power) / (power - 1)
    beta <- phi * (power - 1) * mu[rows]^(power - 1)
    values <- vapply(seq_along(rows), function(j) {
      n <- 1:300
      exp(-lambda[j]) + sum(dpois(n, lambda[j]) *
        pgamma(model$y[rows[j]], n * alpha, scale = beta[j]))
    }, numeric(1))
    zero <- which(model$y[rows] == 0)
    expect_gt(length(zero), 0L)
    values[zero] <- runif(length(zero)) * exp(-lambda[zero])
    expected[rows] <- values
    qnorm(expected)
  })
  expect_equal(quantile_residuals(model, type = "quantile", seed = 53), expected,
    tolerance = 1e-8)
})

test_that("scalar censored-family CDFs and recoverable censor bounds are preserved", {
  for (name in names(gfam_censored_models)) {
    model <- gfam_censored_models[[name]]
    index <- attr(model$family$getfl(), "fi")
    rows <- which(index == 1L)
    mu <- model$fitted.values[rows]
    sigma <- if (name == "cpois") 1 else exp(model$family$getTheta()[1L])
    expected <- withr::with_seed(71, {
      if (name == "cnorm") qnorm(pnorm(model$y[rows], mu, sigma))
      else if (name == "clog") qnorm(plogis(model$y[rows], mu, sigma))
      else {
        lower <- ppois(model$y[rows] - 1, mu)
        upper <- ppois(model$y[rows], mu)
        qnorm(lower + runif(length(rows)) * (upper - lower))
      }
    })
    expect_equal(quantile_residuals(model, type = "quantile", seed = 71)[rows], expected,
      tolerance = 1e-10)
    expect_identical(diagnostic_type(model), "quantile")
  }
  model <- gfam_censored_models[["cnorm"]]
  rows <- which(attr(model$family$getfl(), "fi") == 1L)
  censor <- model$y
  censor[rows[1L]] <- Inf
  attr(model$y, "censor") <- censor
  context <- gfam_diagnostic_context(model)
  expect_identical(attr(context$y, "censor"), censor)
  sigma <- exp(model$family$getTheta()[1L])
  lower <- pnorm(model$y[rows[1L]], model$fitted.values[rows[1L]], sigma)
  expected <- withr::with_seed(19, qnorm(lower + runif(1L) * (1 - lower)))
  expect_equal(quantile_residuals(model, type = "quantile", seed = 19)[rows[1L]], expected)
})

test_that("faceted calibrated intervals and worms use each component's sample size", {
  model <- gfam_rank_model
  qq <- qq_plot(model, seed = 17, level = .8, by_family = TRUE)
  worm <- worm_plot(model, seed = 17, level = .8, by_family = TRUE)
  for (component in levels(qq$data$.family)) {
    q <- qq$data[qq$data$.family == component, ]
    w <- worm$data[worm$data$.family == component, ]
    n <- nrow(q); i <- seq_len(n); z <- qnorm(ppoints(n))
    expect_equal(q$lower, qnorm(qbeta(.1, i, n + 1 - i)))
    expect_equal(q$upper, qnorm(qbeta(.9, i, n + 1 - i)))
    expect_equal(w$residuals, q$residuals - z)
    expect_equal(w$lower, q$lower - z)
    expect_equal(w$upper, q$upper - z)
  }
})


test_that("traditional reference fallback is identified and never automatically normal", {
  model <- gfam_model
  families <- model$family$getfl()
  families[[2L]]$family <- "No generator"
  families[[2L]]$rd <- NULL
  families[[2L]]$qf <- function(p, mu, wt, scale) qnorm(p, mu, sqrt(scale / wt))
  model$family$getfl <- function() families
  expect_message(expect_identical(diagnostic_method(model), "uniform"),
    "component 2 <No generator>")
  expect_message(expect_s3_class(qq_plot(model, type = "deviance", seed = 3), "ggplot"),
    "inverse-CDF")
  families[[2L]]$qf <- NULL
  expect_error(qq_plot(model, type = "deviance"), "explicit approximation")
  expect_identical(diagnostic_method(model, "normal"), "normal")
  families[[2L]]$cdf <- function(q, mu, wt, scale, log_p = FALSE)
    pnorm(q, mu, sqrt(scale / wt), log.p = log_p)
  expect_message(expect_identical(diagnostic_type(model), "deviance"),
    "no validated response-CDF bounds")
  expect_error(qq_plot(model, type = "quantile"), "no validated response-CDF bounds")
})

test_that("grouped log-tail residuals retain finite extreme-tail information", {
  model <- gfam_model
  row <- which(attr(model$family$getfl(), "fi") == 2L)[1L]
  sigma <- sqrt(exp(model$family$getTheta()[1L]) / model$prior.weights[row])
  model$y[row] <- model$fitted.values[row] + 50 * sigma
  expect_equal(quantile_residuals(model, type = "quantile", seed = 3)[row], 50,
    tolerance = 1e-10)
})


test_that("constant grouped observed-fit facets have a single readable x tick", {
  p <- observed_fitted_plot(gfam_rank_model)
  built <- ggplot2::ggplot_build(p)
  panels <- built$layout$panel_params
  expect_length(panels[[3L]]$x$breaks[is.finite(panels[[3L]]$x$breaks)], 1L)
  for (i in 1:2) {
    ticks <- panels[[i]]$x$breaks[is.finite(panels[[i]]$x$breaks)]
    expect_gte(length(ticks), 2L)
    expect_lte(length(ticks), 4L)
  }
  # Axis layout must not alter the underlying response or fitted values.
  used <- model_used_rows(gfam_rank_model)
  expect_equal(p$data$observed, used$y)
  expect_equal(p$data$fitted, as.numeric(gfam_response_means(
    matrix(used$linear.predictors, ncol = 1L), gfam_diagnostic_context(used))))
  ordinary <- ggplot2::ggplot_build(observed_fitted_plot(m_tiny_eg1))
  expect_gte(sum(is.finite(ordinary$layout$panel_params[[1L]]$x$breaks)), 2L)
})


test_that("constant-axis detection does not collapse a variable component with the same centre", {
  data <- data.frame(.family = factor(c(1, 1, 2, 2)), fitted = c(0, 0, -1, 1))
  breaks <- gfam_observed_breaks(data)
  expect_equal(breaks(c(-.05, .05)), 0)
  expect_gte(length(breaks(c(-1.1, 1.1))), 2L)
})
