# Independent oracles for the binomial / Gaussian / Poisson shared fixture.
# Neither component context nor gratia residual helpers enter these calculations.
gfam_diagnostic_oracle <- function(model, y = model$y, type = "deviance") {
  index <- attr(model$family$getfl(), "fi")
  mu <- as.numeric(model$fitted.values)
  w <- model$prior.weights
  scale <- exp(model$family$getTheta()[1L])
  r <- numeric(length(y))
  families <- list(binomial(), gaussian(), poisson())
  for (i in seq_along(families)) {
    rows <- which(index == i)
    f <- families[[i]]
    r[rows] <- switch(type,
      response = y[rows] - mu[rows],
      pearson = (y[rows] - mu[rows]) * sqrt(w[rows] / f$variance(mu[rows])),
      deviance = sign(y[rows] - mu[rows]) *
        sqrt(pmax(f$dev.resids(y[rows], mu[rows], w[rows]), 0) /
          if (i == 2L) scale else 1))
  }
  r
}

gfam_reference_oracle <- function(model, n, method) {
  index <- attr(model$family$getfl(), "fi")
  mu <- as.numeric(model$fitted.values)
  w <- model$prior.weights
  scale <- exp(model$family$getTheta()[1L])
  nr <- length(mu)
  vapply(seq_len(n), function(j) {
    p <- if (method == "uniform") sample((seq_len(nr) - .5) / nr, nr) else NULL
    y <- numeric(nr)
    b <- which(index == 1L)
    g <- which(index == 2L)
    z <- which(index == 3L)
    if (method == "uniform") {
      y[b] <- qbinom(p[b], w[b], mu[b]) / w[b]
      y[g] <- qnorm(p[g], mu[g], sqrt(scale / w[g]))
      y[z] <- qpois(p[z], mu[z])
    } else {
      y[b] <- rbinom(length(b), w[b], mu[b]) / w[b]
      y[g] <- rnorm(length(g), mu[g], sqrt(scale / w[g]))
      y[z] <- rpois(length(z), mu[z])
    }
    y
  }, numeric(nr))
}

test_that("grouped observed residuals use component scales and genuine Pearson residuals", {
  for (model in list(gfam_model, gfam_bam, gfam_missing_model)) {
    context <- gfam_diagnostic_context(model)
    compact <- model_used_rows(model)
    for (type in c("deviance", "response", "pearson")) {
      expected <- gfam_diagnostic_oracle(compact, type = type)
      expect_equal(gfam_diagnostic_residuals(context, type = type), expected)
      expect_equal(diagnostic_residuals(model, type), restore_model_rows(expected, model))
      p <- residuals_linpred_plot(model, type = type, by_family = FALSE)
      expect_equal(p$data$residuals, restore_model_rows(expected, model))
      p <- residuals_hist_plot(model, type = type, by_family = FALSE)
      expect_equal(p$data$residuals, restore_model_rows(expected, model))
    }
  }
})

test_that("grouped QQ and worm references match independent weighted calculations", {
  for (model in list(gfam_model, gfam_bam, gfam_missing_model)) {
    compact <- model_used_rows(model)
    for (method in c("uniform", "simulate")) {
      ys <- withr::with_seed(267, gfam_reference_oracle(compact, 7, method))
      for (type in c("deviance", "response", "pearson")) {
        sims <- apply(ys, 2L, function(y) sort(gfam_diagnostic_oracle(compact, y, type)))
        nr <- nrow(sims)
        expected <- if (method == "uniform") rowMeans(sims) else
          as.numeric(quantile(sims, (seq_len(nr) - .5) / nr))
        p <- qq_plot(model, method = method, type = type, by_family = FALSE, n_uniform = 7,
          n_simulate = 7, seed = 267, level = .8)
        expect_equal(p$data$theoretical, expected)
        expect_equal(p$data$residuals, sort(gfam_diagnostic_oracle(compact, type = type)))
        worm <- worm_plot(model, method = method, type = type, by_family = FALSE, n_uniform = 7,
          n_simulate = 7, seed = 267, level = .8)
        expect_equal(worm$data$theoretical, expected)
        expect_equal(worm$data$residuals, p$data$residuals - expected)
        if (method == "simulate") {
          bounds <- apply(sims, 1L, quantile, probs = c(.1, .9))
          expect_equal(p$data$lower, unname(bounds[1L, ]))
          expect_equal(p$data$upper, unname(bounds[2L, ]))
          expect_equal(worm$data$lower, p$data$lower - expected)
          expect_equal(worm$data$upper, p$data$upper - expected)
        }
      }
    }
  }
})

test_that("grouped defaults choose available reference methods including Tweedie", {
  expect_identical(diagnostic_method(gfam_model), "simulate")
  expect_identical(diagnostic_method(gfam_extended_model), "simulate")
  expect_identical(diagnostic_method(gfam_extended_model, "uniform"), "uniform")
  expect_identical(diagnostic_method(gfam_extended_model, "normal"), "normal")
  expect_equal(qq_plot(gfam_extended_model, type = "deviance", n_simulate = 3, seed = 11)$labels$subtitle,
    "Method: simulate")
  expect_equal(worm_plot(gfam_extended_model, type = "deviance", n_simulate = 3, seed = 11)$labels$subtitle,
    "Method: simulate")
  expect_message(expect_identical(diagnostic_method(gfam_model, "direct"), "uniform"),
    "deprecated")
  model <- gfam_model
  families <- model$family$getfl()
  families[[2]]$family <- "Unsupported diagnostic family"
  model$family$getfl <- function() families
  expect_error(diagnostic_method(model), "component 2 <Unsupported diagnostic family>")
  expect_message(expect_s3_class(qq_plot(model, method = "normal", seed = 2), "ggplot"),
    "Using deviance residuals")
  expect_identical(qq_plot(model, type = "deviance", method = "normal")$labels$subtitle,
    "Method: normal (approximation)")
})

test_that("full grouped appraisal uses consistent residual and mean panels", {
  for (model in list(gfam_model, gfam_bam, gfam_missing_model, gfam_gaussian_model)) {
    for (type in c("deviance", "pearson", "response")) {
      expect_s3_class(appraise(model, type = type, seed = 2), "patchwork")
    }
    p <- observed_fitted_plot(model, by_family = FALSE)
    expect_length(p$data$observed, length(model_row_map(model)))
    expect_equal(p$data$observed, restore_model_rows(model_used_rows(model)$y, model))
    expect_equal(p$data$fitted, restore_model_rows(model_used_rows(model)$fitted.values, model))
  }
})

test_that("multiple Gaussian scales and fitted extended theta are retained", {
  model <- gfam_gaussian_model
  context <- gfam_diagnostic_context(model)
  scales <- exp(model$family$getTheta())
  index <- attr(model$family$getfl(), "fi")
  expected <- (model$y - model$fitted.values) *
    sqrt(model$prior.weights / scales[index])
  expect_equal(gfam_diagnostic_residuals(context), expected)
  model <- gfam_extended_model
  context <- gfam_diagnostic_context(model)
  # Packed order: first NB theta; Gaussian log(scale); second NB theta;
  # Tweedie power theta and log(scale).
  theta <- model$family$getTheta()
  expect_equal(context$families[[1]]$getTheta(), theta[1L])
  expect_equal(context$families[[3]]$getTheta(), theta[3L])
  expect_equal(context$components[[2]]$scale, exp(theta[2L]))
  expect_equal(context$components[[4]]$scale, exp(theta[5L]))
  expect_true(all(is.finite(qq_plot(model, seed = 37, n_simulate = 4)$data$theoretical)))
})

test_that("ordinal deviance diagnostics retain native component signs and limitations", {
  model <- gfam_ordinal_model
  context <- gfam_diagnostic_context(model)
  rows <- which(context$index == 1L)
  f <- context$families[[1L]]
  d <- f$dev.resids(model$y[rows], model$fitted.values[rows], model$prior.weights[rows])
  expected <- sqrt(pmax(d, 0)) * attr(d, "sign")
  expect_equal(gfam_diagnostic_residuals(context)[rows], as.numeric(expected))
  expect_identical(diagnostic_method(model), "simulate")
  expect_s3_class(qq_plot(model, seed = 5, n_simulate = 4), "ggplot")
  expect_s3_class(worm_plot(model, seed = 5, n_simulate = 4), "ggplot")
  expect_error(qq_plot(model, type = "pearson"), "only deviance")
  expect_error(qq_plot(model, type = "response"), "only deviance")
  expect_error(appraise(model), "observed-versus-fitted")
  expect_error(observed_fitted_plot(model), "observed-versus-fitted")
  context$families[[2]]$variance <- NULL
  context$components[[2]]$family$variance <- NULL
  expect_error(gfam_diagnostic_residuals(context, type = "pearson"), "only deviance")
  context <- gfam_diagnostic_context(gfam_model)
  context$components[[3]]$family$variance <- NULL
  expect_error(gfam_diagnostic_residuals(context, type = "pearson"),
    "component 3 <poisson>")
})

test_that("grouped diagnostics preserve RNG and fitted parameter closures", {
  for (model in list(gfam_model, gfam_extended_model, gfam_ordinal_model)) {
    theta <- model$family$getTheta()
    families <- serialize(model$family$getfl(), NULL)
    set.seed(283)
    before <- .Random.seed
    for (fun in list(qq_plot, worm_plot)) {
      p <- fun(model, seed = 5, n_uniform = 3, n_simulate = 3)
      expect_identical(.Random.seed, before)
      expect_equal(fun(model, seed = 5, n_uniform = 3, n_simulate = 3)$data, p$data)
    }
    expect_identical(model$family$getTheta(), theta)
    expect_identical(serialize(model$family$getfl(), NULL), families)
  }
})

test_that("grouped diagnostics draw representative QQ, worm and appraisal plots", {
  vdiffr::expect_doppelganger("grouped-family QQ uniform", qq_plot(gfam_model, type = "deviance", method = "uniform", seed = 267))
  vdiffr::expect_doppelganger("grouped-family worm simulation",
    worm_plot(gfam_model, type = "deviance", method = "simulate", seed = 267))
  vdiffr::expect_doppelganger("grouped-family appraisal", appraise(gfam_model, type = "deviance", method = "uniform", seed = 267))
})


test_that("single fitting rows and invalid sample counts have defined behavior", {
  model <- gfam_model
  model$model <- model$model[1L, , drop = FALSE]
  model$offset <- model$offset[1L]
  for (nm in c("y", "fitted.values", "linear.predictors", "residuals",
      "weights", "prior.weights")) model[[nm]] <- model[[nm]][1L]
  families <- model$family$getfl()
  attr(families, "fi") <- 1L
  model$family$getfl <- function() families
  for (n in c(1, 3)) {
    for (method in c("uniform", "simulate")) {
      p <- qq_plot(model, type = "deviance", method = method, n_uniform = n, n_simulate = n, seed = 3, by_family = FALSE)
      expect_equal(nrow(p$data), 1L)
      expect_true(all(is.finite(unlist(p$data[vapply(p$data, is.numeric, logical(1))]))))
    }
  }
  for (n in list(0, -1, 1.5, NA_real_, Inf, c(2, 3), "3")) {
    expect_error(qq_plot(gfam_model, type = "deviance", method = "uniform", n_uniform = n), "positive integer")
    expect_error(worm_plot(gfam_model, type = "deviance", method = "simulate", n_simulate = n),
      "positive integer")
  }
  model$model <- model$model[FALSE, , drop = FALSE]
  for (nm in c("y", "fitted.values", "linear.predictors", "residuals",
      "weights", "prior.weights")) model[[nm]] <- numeric()
  attr(families, "fi") <- integer()
  expect_error(qq_plot(model), "at least one fitted observation")
})

test_that("ordinal quantile fallback names the unavailable component", {
  expect_message(p <- qq_plot(gfam_ordinal_model, method = "uniform", seed = 3,
    n_simulate = 2), "component 1 <Ordered Categorical")
  expect_identical(p$labels$subtitle, "Method: simulate")
})

test_that("an explicit seed restores the absence of a caller RNG state", {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old <- get(".Random.seed", envir = .GlobalEnv)
    on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
    rm(".Random.seed", envir = .GlobalEnv)
  }
  for (fun in list(qq_plot, worm_plot)) {
    fun(gfam_model, seed = 267, n_uniform = 3)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
})


test_that("scalar censored-family components have latent reference diagnostics", {
  for (name in names(gfam_censored_models)) {
    model <- gfam_censored_models[[name]]
    for (method in c("uniform", "simulate")) {
      p <- qq_plot(model, type = "deviance", method = method, seed = 7, n_uniform = 3, n_simulate = 3)
      expect_identical(p$labels$subtitle, paste("Method:", method))
      expect_true(all(is.finite(unlist(p$data[vapply(p$data, is.numeric, logical(1))]))))
      expect_s3_class(appraise(model, type = "deviance", method = method, seed = 7,
        n_uniform = 3, n_simulate = 3), "patchwork")
    }
    index <- attr(model$family$getfl(), "fi")
    i <- which(index == 1L)
    f <- get(name, asNamespace("mgcv"))()
    if (name != "cpois") f$putTheta(model$family$getTheta()[1L])
    dev <- f$dev.resids(model$y[i], model$fitted.values[i], model$prior.weights[i])
    sign <- attr(dev, "sign")
    if (is.null(sign)) sign <- sign(model$y[i] - model$fitted.values[i])
    expected <- sign * sqrt(pmax(dev, 0))
    expect_equal(gfam_diagnostic_residuals(gfam_diagnostic_context(model))[i],
      as.numeric(expected))
  }
})

test_that("normal grouped references use the same observed residuals", {
  for (type in c("deviance", "response", "pearson")) {
    p <- qq_plot(gfam_model, method = "normal", type = type, by_family = FALSE)
    expect_equal(p$data$theoretical, qnorm(ppoints(length(gfam_model$y))))
    expect_equal(p$data$residuals, sort(gfam_diagnostic_oracle(gfam_model, type = type)))
  }
})


test_that("beta and scaled-t diagnostics use native fitted component residuals", {
  for (name in names(gfam_custom_models)) {
    model <- gfam_custom_models[[name]]
    index <- attr(model$family$getfl(), "fi")
    rows <- which(index == 1L)
    theta <- model$family$getTheta()
    f <- get(name, asNamespace("mgcv"))()
    nth <- if (name == "betar") 1L else 2L
    f$putTheta(theta[seq_len(nth)])
    # Construct the native component object independently, without the grouped
    # context helper or grouped residual engine under test.
    proxy <- model
    proxy$family <- f
    proxy$model <- model$model[rows, , drop = FALSE]
    for (nm in c("y", "linear.predictors", "fitted.values", "prior.weights",
        "weights", "residuals")) proxy[[nm]] <- model[[nm]][rows]
    context <- gfam_diagnostic_context(model)
    for (type in c("deviance", "response", "pearson")) {
      expect_equal(gfam_diagnostic_residuals(context, type = type)[rows],
        as.numeric(residuals(proxy, type = type)))
      for (method in c("uniform", "simulate")) {
        expect_true(all(is.finite(qq_plot(model, type = type, method = method,
          seed = 3, n_uniform = 3, n_simulate = 3)$data$theoretical)))
      }
    }
    # Independently evaluate component deviance on a generated response sample;
    # this catches missing beta saturated log-likelihood corrections.
    ys <- withr::with_seed(3, {
      replicate(2, {
        values <- numeric(length(model$y))
        values[rows] <- f$rd(model$fitted.values[rows], model$prior.weights[rows], 1)
        others <- which(index == 2L)
        values[others] <- rnorm(length(others), model$fitted.values[others],
          sqrt(exp(theta[nth + 1L]) / model$prior.weights[others]))
        values
      }, simplify = FALSE)
    })
    for (j in seq_along(ys)) {
      proxy$y <- ys[[j]][rows]
      expect_equal(gfam_diagnostic_residuals(context, ys[[j]])[rows],
        as.numeric(residuals(proxy, type = "deviance")))
    }
  }
})


# Manual component references are calibrated independently, including the
# sample probabilities and sorting, rather than splitting a pooled QQ curve.
gfam_component_reference_oracle <- function(model, component, n, method, type) {
  index <- attr(model$family$getfl(), "fi")
  rows <- which(index == component)
  mu <- as.numeric(model$fitted.values)[rows]
  wt <- model$prior.weights[rows]
  scale <- if (component == 2L) exp(model$family$getTheta()[1L]) else 1
  family <- list(binomial(), gaussian(), poisson())[[component]]
  residual <- function(y) switch(type,
    response = y - mu,
    pearson = (y - mu) * sqrt(wt / family$variance(mu)),
    deviance = sign(y - mu) * sqrt(pmax(family$dev.resids(y, mu, wt), 0) / scale))
  nr <- length(rows)
  sims <- replicate(n, {
    if (method == "uniform") {
      p <- sample((seq_len(nr) - .5) / nr, nr)
      y <- switch(component, qbinom(p, wt, mu) / wt,
        qnorm(p, mu, sqrt(scale / wt)), qpois(p, mu))
    } else {
      y <- switch(component, rbinom(nr, wt, mu) / wt,
        rnorm(nr, mu, sqrt(scale / wt)), rpois(nr, mu))
    }
    sort(residual(y))
  })
  list(theoretical = if (method == "uniform") rowMeans(sims) else
    as.numeric(quantile(sims, (seq_len(nr) - .5) / nr)),
    observed = sort(residual(model$y[rows])),
    bounds = apply(sims, 1L, quantile, probs = c(.1, .9)))
}

test_that("family-faceted QQ and worm calibrate each component independently", {
  for (model in list(gfam_model, gfam_bam, gfam_missing_model)) {
    compact <- model_used_rows(model)
    for (method in c("uniform", "simulate")) {
      for (type in c("deviance", "response", "pearson")) {
        expected <- withr::with_seed(267, lapply(1:3, function(i)
          gfam_component_reference_oracle(compact, i, 4, method, type)))
        qq <- qq_plot(model, method = method, type = type, seed = 267,
          n_uniform = 4, n_simulate = 4, level = .8)
        worm <- worm_plot(model, method = method, type = type, seed = 267,
          n_uniform = 4, n_simulate = 4, level = .8)
        for (i in 1:3) {
          q <- qq$data[as.integer(qq$data$.family) == i, ]
          w <- worm$data[as.integer(worm$data$.family) == i, ]
          expect_equal(q$theoretical, expected[[i]]$theoretical)
          expect_equal(q$residuals, expected[[i]]$observed)
          expect_equal(w$residuals, q$residuals - q$theoretical)
          if (method == "simulate") {
            expect_equal(q$lower, unname(expected[[i]]$bounds[1L, ]))
            expect_equal(q$upper, unname(expected[[i]]$bounds[2L, ]))
            expect_equal(w$lower, q$lower - q$theoretical)
            expect_equal(w$upper, q$upper - q$theoretical)
          }
        }
      }
    }
  }
})

test_that("every diagnostic facets by indexed family with free axes", {
  for (model in list(gfam_model, gfam_missing_model, gfam_gaussian_model)) {
    labels <- paste0(seq_along(model$family$getfl()), ": ",
      vapply(model$family$getfl(), `[[`, character(1), "family"))
    plots <- list(qq_plot(model, type = "deviance", seed = 6), worm_plot(model, type = "deviance", seed = 6),
      residuals_linpred_plot(model, type = "deviance"), residuals_hist_plot(model, type = "deviance"), observed_fitted_plot(model))
    for (p in plots) {
      built <- ggplot2::ggplot_build(p)
      panels <- built$layout$layout
      expect_identical(as.character(panels$.family), labels)
      expect_equal(length(unique(panels$SCALE_X)), length(labels))
      expect_equal(length(unique(panels$SCALE_Y)), length(labels))
      expect_false(anyNA(panels$.family))
      for (layer in built$data) expect_true(all(as.integer(layer$PANEL) %in% seq_along(labels)))
    }
    # appraise propagates the selection to all four constituent plots.
    for (by_family in c(TRUE, FALSE)) {
      app <- appraise(model, seed = 6, by_family = by_family)
      for (j in 1:4) {
        panels <- ggplot2::ggplot_build(app[[j]])$layout$layout
        expect_equal(nrow(panels), if (by_family || j == 4L) length(labels) else 1L)
      }
    }
  }
})

test_that("normal QQ reference lines and worm bands are component-specific", {
  model <- gfam_gaussian_model
  for (type in c("deviance", "response", "pearson")) {
    p <- qq_plot(model, method = "normal", type = type)
    w <- worm_plot(model, method = "normal", type = type)
    context <- gfam_diagnostic_context(model)
    for (i in 1:2) {
      r <- sort(gfam_diagnostic_residuals(context, type = type)[context$index == i])
      q <- p$data[as.integer(p$data$.family) == i, ]
      ww <- w$data[as.integer(w$data$.family) == i, ]
      theo <- qnorm(ppoints(length(r)))
      med <- median(r) + theo * IQR(r) / 1.349
      expect_equal(q$theoretical, theo)
      expect_equal(q$residuals, r)
      expect_equal(q$.qq_intercept, rep(median(r), length(r)))
      expect_equal(q$.qq_slope, rep(IQR(r) / 1.349, length(r)))
      expect_equal(ww$residuals, r - med)
      expect_equal(ww$lower, q$lower - med)
      expect_equal(ww$upper, q$upper - med)
    }
    pooled <- worm_plot(model, method = "normal", type = type, by_family = FALSE)
    expect_false(".family" %in% names(pooled$data))
  }
})

test_that("pooled grouped diagnostics remain an explicit option", {
  vdiffr::expect_doppelganger("grouped-family pooled appraisal",
    appraise(gfam_model, type = "deviance", method = "uniform", seed = 267, by_family = FALSE))
  expect_false(".family" %in% names(qq_plot(gfam_model, type = "deviance", by_family = FALSE)$data))
})
