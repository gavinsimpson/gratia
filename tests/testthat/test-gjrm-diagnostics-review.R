skip_if_not_installed("GJRM", minimum_version = "0.2-6")

test_that("selecting a response preserves its seeded randomised residuals", {
  withr::local_seed(902)
  before <- .Random.seed
  for (kind in c("binary", "count_cont", "trivariate")) {
    m <- gjrm_joint_fixture(kind)
    for (type in c("pit", "quantile", "dharma")) {
      all <- diagnostic_data(m, type = type, seed = 19, n_dharma = 13)
      for (i in seq_along(m$margins)) {
        selected <- diagnostic_data(m, type = type, seed = 19, response = i,
          n_dharma = 13)
        expect_equal(selected$data$residuals,
          all$data$residuals[all$data$.response == i], info = paste(kind, type, i))
      }
      cached <- diagnostic_data(all$source, type = type, seed = 19)
      expect_equal(cached$data$residuals, all$data$residuals)
      changed <- diagnostic_data(all$source, type = type, seed = 20, n_dharma = 13)
      fresh <- diagnostic_data(m, type = type, seed = 20, n_dharma = 13)
      expect_equal(changed$data$residuals, fresh$data$residuals)
    }
  }
  expect_identical(.Random.seed, before)
})

test_that("DHARMa simulations and caches retain their requested count", {
  m <- gjrm_joint_fixture("binary")
  d <- diagnostic_data(m, type = "dharma", seed = 14, n_dharma = 13)
  expect_identical(d$n_dharma, 13L)
  cached <- diagnostic_data(d$source, type = "dharma", seed = 14)
  expect_equal(cached$data$residuals, d$data$residuals)
  expect_identical(cached$n_dharma, 13L)
  changed <- diagnostic_data(d$source, type = "dharma", seed = 14, n_dharma = 23)
  fresh <- diagnostic_data(m, type = "dharma", seed = 14, n_dharma = 23)
  expect_equal(changed$data$residuals, fresh$data$residuals)
  expect_identical(changed$n_dharma, 23L)
  for (scalable in c(FALSE, TRUE)) {
    p <- appraise(m, type = "dharma", seed = 14, n_dharma = 13, scalable = scalable)
    expect_length(p, 4L)
    expect_silent(patchwork::patchworkGrob(p))
    reference <- qq_plot(m, type = "dharma", seed = 14, n_dharma = 13)
    expect_equal(reference$data$residuals,
      unlist(lapply(split(d$data$residuals, d$data$.response), sort), use.names = FALSE))
    if (!scalable) expect_equal(p[[2]]$data$residuals, d$data$residuals)
  }
})

test_that("traditional QQ references stay separate and stable under response selection", {
  m <- gjrm_joint_fixture("normal")
  for (type in c("response", "pearson")) {
    for (method in c("uniform", "simulate", "normal", "marginal")) {
      all <- qq_plot(m, type = type, method = method, n_uniform = 3,
        n_simulate = 4, seed = 98)
      for (i in 1:2) {
        selected <- qq_plot(m, type = type, method = method, response = i,
          n_uniform = 3, n_simulate = 4, seed = 98)
        first <- all$data[as.integer(all$data$.family) == i, , drop = FALSE]
        columns <- intersect(c("theoretical", "residuals", "lower", "upper",
          ".qq_intercept", ".qq_slope"), names(first))
        expect_equal(as.data.frame(first[columns]), as.data.frame(selected$data[columns]),
          ignore_attr = TRUE, info = paste(type, method, i))
      }
      expect_silent(ggplot2::ggplot_build(all))
    }
  }
})

test_that("joint predictor ranks use each response in ordinary and scalable plots", {
  m <- gjrm_joint_fixture("normal")
  raw <- diagnostic_data(m, seed = 5)
  expected <- unlist(lapply(split(raw$data$eta, raw$data$.response), function(x)
    (rank(x) - .5) / length(x)), use.names = FALSE)
  ordinary <- residuals_linpred_plot(m, rank = TRUE, seed = 5)
  expect_equal(ordinary$data$eta, expected)
  expect_match(ordinary$labels$x, "rank")
  p <- appraise(m, rank = TRUE, seed = 5, scalable = FALSE)
  expect_equal(p[[2]]$data$eta, expected)
  expect_match(p[[2]]$labels$x, "rank")
  scalable <- scalable_residuals_linpred_plot(m, rank = TRUE, seed = 5)
  expect_match(scalable$labels$x, "rank")
  expect_silent(ggplot2::ggplot_build(scalable))
  expect_error(appraise(m, by_family = FALSE), "separate responses")
  expect_error(appraise(m, by_family = NA), "by_family")
  expect_error(appraise(m, rank = 1, scalable = FALSE), "rank")
  expect_error(appraise(m, rank = 1, scalable = TRUE), "rank")
})

test_that("binary margins use GJRM's fitted probability limits", {
  m <- gjrm_joint_fixture("binary")
  m$VC$min.pr <- .02
  m$VC$max.pr <- .98
  for (link in c("probit", "logit", "cloglog")) {
    m$margins[2] <- link
    m$eta2 <- rep(c(-100, -1, 1, 100), length.out = length(m$y2))
    reference <- GJRM:::probm(m$eta2, link, min.dn = m$VC$min.dn,
      min.pr = m$VC$min.pr, max.pr = m$VC$max.pr)$pr
    d <- response_distribution(m, response = 2)
    expect_equal(d$mean, as.vector(reference), info = link)
    expect_equal(d$variance, as.vector(reference * (1 - reference)), info = link)
  }
})

gjrm_review_family_fixture <- local({
  models <- list()
  function(margin, weighted = FALSE) {
    key <- paste(margin, weighted)
    if (is.null(models[[key]])) {
      d <- withr::with_seed(913, {
        n <- 200L
        d <- data.frame(x = runif(n, -1, 1), z = runif(n, -1, 1))
        d$y1 <- GJRM::sim.resp(margin, n, eta = .2 + .3 * d$x,
          sigma2 = if (margin == "BE") .3 else .65, nu = 1)
        d$y2 <- .2 + .2 * d$z + rnorm(n)
        d$weight <- if (weighted) rep(c(.5, 1, 2, 3), length.out = n) else 1
        d
      })
      models[[key]] <<- withr::with_seed(914,
        do.call(GJRM::gjrm, list(formula = list(y1 ~ x, y2 ~ z), data = d, weights = d$weight,
          margins = c(margin, "N"), model = "B")))
    }
    models[[key]]
  }
})

test_that("actual joint fits retain each supported continuous margin's parameters", {
  for (margin in c("LN", "GA", "BE", "LO", "WEI", "IG", "GU", "rGU")) {
    m <- gjrm_review_family_fixture(margin)
    eta <- as.vector(m$eta1)
    sigma <- rep_len(as.vector(m$sigma21), length(m$y1))
    y <- as.vector(m$y1)
    mu <- exp(eta)
    expected <- switch(margin,
      LN = plnorm(y, meanlog = eta, sdlog = sigma),
      GA = pgamma(y, shape = 1 / sigma^2, scale = mu * sigma^2),
      BE = pbeta(y, plogis(eta) * (1 / sigma^2 - 1),
        (1 - plogis(eta)) * (1 / sigma^2 - 1)),
      LO = plogis(y, eta, sigma),
      WEI = pweibull(y, shape = sigma, scale = mu),
      IG = get("pIG", asNamespace("GJRM"))(y, mu, sigma),
      GU = -expm1(-exp((y - eta) / sigma)),
      rGU = exp(-exp(-(y - eta) / sigma)))
    distribution <- response_distribution(m, response = 1)
    expect_equal(distribution$cdf(y), expected, tolerance = 1e-10, info = margin)
    expect_equal(quantile_residuals(m, response = 1, type = "pit"), expected,
      tolerance = 1e-10, info = margin)
    # The second normal margin has its own scale, irrespective of the first.
    normal <- response_distribution(m, response = 2)
    expect_equal(normal$variance, rep_len(as.vector(m$sigma22)^2, length(m$y2)), info = margin)
  }
})

test_that("a supported response can be checked beside an unsupported fitted margin", {
  m <- gjrm_review_family_fixture("FISK")
  expect_error(diagnostic_data(m), "do not support these margins")
  expect_error(response_distribution(m, response = 1), "do not support these margins")
  d <- diagnostic_data(m, response = 2)
  expected <- pnorm(m$y2, as.vector(m$eta2), as.vector(m$sigma22))
  expect_equal(d$data$residuals, qnorm(as.vector(expected)))
  expect_identical(unique(d$data$.response), 2L)
  expect_silent(patchwork::patchworkGrob(appraise(m, response = 2)))
})

test_that("GJRM likelihood weights do not become extra response precision", {
  m <- gjrm_review_family_fixture("N", weighted = TRUE)
  d <- diagnostic_data(m, type = "pearson")
  for (i in 1:2) {
    y <- as.vector(m[[paste0("y", i)]])
    eta <- as.vector(m[[paste0("eta", i)]])
    sigma <- rep_len(as.vector(m[[paste0("sigma2", i)]]), length(y))
    distribution <- response_distribution(m, response = i)
    expect_equal(distribution$variance, sigma^2)
    expect_equal(distribution$cdf(y), pnorm(y, eta, sigma))
    expect_equal(d$data$residuals[d$data$.response == i], (y - eta) / sigma)
  }
})

test_that("all diagnostic entry points reject silently pooling joint responses", {
  m <- gjrm_joint_fixture("binary")
  d <- diagnostic_data(m, seed = 4)
  for (fun in list(qq_plot, worm_plot, residuals_linpred_plot, observed_fitted_plot,
      residuals_hist_plot, scalable_qq_plot, scalable_residuals_linpred_plot,
      scalable_observed_fitted_plot, scalable_residuals_hist_plot, appraise)) {
    expect_error(fun(m, by_family = FALSE), "separate responses")
    expect_error(fun(d, by_family = FALSE), "separate responses")
  }
})

test_that("original joint fits and prepared sources use identical QQ adapters", {
  m <- gjrm_joint_fixture("binary_cont")
  for (type in c("quantile", "response", "dharma")) {
    d <- diagnostic_data(m, type = type, seed = 22, n_dharma = 11)
    direct <- withr::with_seed(23, diagnostic_qq_data(m, d, n_simulate = 4))
    prepared <- withr::with_seed(23, diagnostic_qq_data(d$source, d, n_simulate = 4))
    expect_equal(direct, prepared, info = type)
    expect_true(".family" %in% names(direct$data))
    if (type != "response") expect_true(all(c("lower", "upper") %in% names(direct$data)))
    expect_true(direct$by_family)
  }
})

test_that("joint lognormal checks use response means, variances and family labels", {
  m <- gjrm_review_family_fixture("LN")
  eta <- as.vector(m$eta1)
  sigma <- rep_len(as.vector(m$sigma21), length(eta))
  mu <- exp(eta + sigma^2 / 2)
  variance <- mu^2 * expm1(sigma^2)
  d <- diagnostic_data(m, response = 1, type = "response")
  expect_identical(unname(d$family), "lognormal")
  expect_equal(d$data$fitted, mu)
  expect_equal(d$data$residuals, as.vector(m$y1) - mu)
  expect_equal(diagnostic_data(m, response = 1, type = "pearson")$data$residuals,
    (as.vector(m$y1) - mu) / sqrt(variance))
  p <- appraise(m, response = 1)
  expect_identical(p[[2]]$labels$subtitle, "Family: lognormal")
  expect_error(appraise(m, response = 1, type = "deviance"), "does not define")
  for (type in c("response", "pearson")) {
    for (method in c("normal", "uniform", "simulate", "marginal")) {
      q <- qq_plot(m, response = 1, type = type, method = method,
        n_uniform = 3, n_simulate = 4, seed = 18)
      expect_silent(ggplot2::ggplot_build(q))
    }
  }
})

test_that("recursive response predictors require copula-conditioned diagnostics", {
  d <- withr::with_seed(325, {
    n <- 220L
    x <- runif(n, -1, 1)
    z <- runif(n, -1, 1)
    e1 <- rnorm(n)
    e2 <- .5 * e1 + sqrt(.75) * rnorm(n)
    y1 <- as.numeric(.2 + .7 * x + e1 > 0)
    data.frame(x, z, binary = y1, continuous = .2 + .7 * x + e1,
      y2 = -.3 + .8 * y1 + .5 * z + e2)
  })
  formulas <- list(
    list(binary ~ x, y2 ~ binary + z),
    list(continuous ~ x, y2 ~ continuous + z),
    list(binary ~ x, y2 ~ z, ~ binary, ~ 1))
  for (i in seq_along(formulas)) {
    margins <- if (i == 2L) c("N", "N") else c("probit", "N")
    m <- withr::with_seed(326, do.call(GJRM::gjrm,
      list(formula = formulas[[i]], data = d, margins = margins, model = "B")))
    expect_error(diagnostic_data(m), "conditioned through the fitted copula")
    expect_error(quantile_residuals(m), "conditioned through the fitted copula")
    expect_error(response_distribution(m, response = 1), "conditioned through the fitted copula")
    expect_error(appraise(m), "conditioned through the fitted copula")
    # This restriction belongs to diagnostic references, not smooth plotting.
    expect_no_error(assemble(m))
  }
})
