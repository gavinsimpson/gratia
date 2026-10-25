skip_if_not_installed("GJRM", minimum_version = "0.2-6")

test_that("joint normal diagnostics use both responses' final marginal distributions", {
  m <- gjrm_joint_fixture()
  n <- length(m$y1)
  d <- diagnostic_data(m, seed = 75)
  expect_s3_class(d, "gratia_diagnostics")
  expect_equal(nrow(d$data), 2L * n)
  expect_identical(d$data$.response, rep(1:2, each = n))
  expect_equal(d$data$.row, rep(seq_len(n), 2))
  expect_identical(levels(d$data$.family), c("y1 (N)", "y2 (N)"))
  for (i in 1:2) {
    y <- as.vector(m[[paste0("y", i)]])
    eta <- as.vector(m[[paste0("eta", i)]])
    sigma <- as.vector(m[[paste0("sigma2", i)]])
    rows <- d$data$.response == i
    expected_pit <- pnorm(y, eta, sigma)
    expect_equal(d$data$observed[rows], y)
    expect_equal(d$data$fitted[rows], eta)
    expect_equal(d$data$eta[rows], eta)
    expect_equal(d$data$residuals[rows], qnorm(expected_pit))
    distribution <- response_distribution(m, response = i)
    expect_equal(distribution$mean, eta)
    expect_equal(distribution$variance, sigma^2)
    expect_equal(distribution$cdf(y), expected_pit)
    expect_equal(as.vector(quantile_residuals(m, type = "pit", response = i)),
      expected_pit)
    expect_equal(diagnostic_data(m, type = "response", response = i)$data$residuals,
      y - eta)
    expect_equal(diagnostic_data(m, type = "pearson", response = i)$data$residuals,
      (y - eta) / sigma)
    selected <- diagnostic_data(m, seed = 75, response = i)
    expect_identical(unique(selected$data$.response), i)
    expect_equal(selected$data$residuals, d$data$residuals[rows])
  }
  q <- quantile_residuals(m, type = "quantile", seed = 75)
  expect_equal(dim(q), c(n, 2L))
  expect_equal(as.vector(q), d$data$residuals)
  expect_identical(colnames(q), c("y1 (N)", "y2 (N)"))
  aggregate <- response_distribution(m)
  expect_false(aggregate$independent)
  expect_null(aggregate$rd)
  expect_equal(aggregate$mean, d$data$fitted)
  # None of the intermediate fits or stored working probabilities may leak in.
  altered <- m
  altered$gam1$coefficients[] <- altered$gam2$coefficients[] <- 100
  altered$gam1$fitted.values[] <- altered$gam2$fitted.values[] <- 100
  altered$fit$p[] <- .5
  expect_equal(diagnostic_data(altered, seed = 75)$data, d$data)
})

test_that("binary and mixed joint diagnostics randomise within the correct probability masses", {
  for (kind in c("binary", "binary_cont", "trivariate")) {
    m <- gjrm_joint_fixture(kind)
    withr::local_seed(99)
    before <- .Random.seed
    d <- diagnostic_data(m, type = "pit", seed = 28)
    expect_identical(.Random.seed, before)
    expect_identical(diagnostic_data(m, type = "pit", seed = 28)$data, d$data)
    q <- quantile_residuals(m, type = "quantile", seed = 28)
    expect_equal(as.vector(q), qnorm(d$data$residuals))
    expect_equal(dim(q), c(length(m$y1), length(m$margins)))
    for (i in seq_along(m$margins)) {
      eta <- as.vector(m[[paste0("eta", i)]])
      y <- as.vector(m[[paste0("y", i)]])
      rows <- d$data$.response == i
      if (m$margins[i] == "N") {
        expect_equal(d$data$residuals[rows], pnorm(y, eta, as.vector(m$sigma2)))
      } else {
        p <- if (m$margins[i] == "probit") pnorm(eta) else plogis(eta)
        lower <- pbinom(y - 1, size = 1, prob = p)
        upper <- pbinom(y, size = 1, prob = p)
        expect_true(all(d$data$residuals[rows] >= lower))
        expect_true(all(d$data$residuals[rows] <= upper))
        expect_equal(response_distribution(m, response = i)$mean, p)
      }
    }
    expect_identical(.Random.seed, before)
  }
})

test_that("joint appraisals keep four panels and facet every panel by response", {
  for (kind in test_cases_for_cran(c("normal", "binary_cont", "trivariate"),
    "binary_cont")) {
    m <- gjrm_joint_fixture(kind)
    for (scalable in c(FALSE, TRUE)) {
      plots <- appraise(m, seed = 17, scalable = scalable, qq_bins = 20)
      expect_s3_class(plots, "patchwork")
      expect_length(plots, 4L)
      for (i in 1:4) {
        p <- plots[[i]]
        expect_s3_class(p, "ggplot")
        expect_s3_class(p$facet, "FacetWrap")
        built <- ggplot2::ggplot_build(p)
        expect_equal(nrow(built$layout$layout), length(m$margins))
      }
      expect_silent(patchwork::patchworkGrob(plots))
      selected <- appraise(m, response = 2, seed = 17, scalable = scalable)
      expect_length(selected, 4L)
      expect_equal(nrow(ggplot2::ggplot_build(selected[[1]])$layout$layout), 1L)
    }
  }
})

test_that("joint standalone diagnostics and prepared data retain response facets", {
  m <- gjrm_joint_fixture("binary_cont")
  d <- diagnostic_data(m, seed = 18)
  for (fun in list(qq_plot, worm_plot, residuals_linpred_plot,
    observed_fitted_plot, residuals_hist_plot, scalable_qq_plot,
    scalable_residuals_linpred_plot, scalable_observed_fitted_plot,
    scalable_residuals_hist_plot)) {
    p <- fun(m, seed = 18)
    expect_s3_class(p, "ggplot")
    expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
    single <- fun(m, response = 2, seed = 18)
    expect_equal(nrow(ggplot2::ggplot_build(single)$layout$layout), 1L)
  }
  prepared <- appraise(d, seed = 18, scalable = FALSE)
  expect_length(prepared, 4L)
  for (i in 1:4) {
    expect_equal(nrow(ggplot2::ggplot_build(prepared[[i]])$layout$layout), 2L)
  }
})

test_that("joint diagnostics reject unsupported models and invalid response choices", {
  m <- gjrm_joint_fixture()
  for (response in list(0, 3, 1.5, Inf, NA_integer_, TRUE, "y1", 1:2)) {
    expect_error(diagnostic_data(m, response = response), "response")
    expect_error(quantile_residuals(m, response = response), "response")
  }
  expect_error(diagnostic_data(gjrm_joint_fixture("ordinal")), "non-ordinal")
  expect_error(diagnostic_data(gjrm_joint_fixture("selection")), "bivariate B")
  expect_error(diagnostic_data(gjrm_joint_fixture("fixed")), "fixed-margin")
  expect_error(simulate(m, seed = 1), "copula")
  expect_error(appraise(m, ppcheck = TRUE), "posterior predictive checks")
  expect_error(response_distribution(m, conditioning = "marginal"), "conditional")
  altered <- m
  altered$margins[2] <- "DAGUM"
  expect_error(diagnostic_data(altered), "do not support these margins")
  altered <- m
  altered$surv <- TRUE
  expect_error(diagnostic_data(altered), "uncensored")
  altered <- m
  altered$eta2 <- altered$eta2[-1]
  expect_error(diagnostic_data(altered), "not aligned")
})

test_that("joint count and continuous margins use their own final scale parameters", {
  m <- gjrm_joint_fixture("count_cont")
  mu <- exp(as.vector(m$eta1))
  dispersion <- as.vector(m$sigma21)
  sigma <- as.vector(m$sigma22)
  d <- diagnostic_data(m, type = "pit", seed = 69)
  rows <- d$data$.response == 1L
  expect_equal(d$data$fitted[rows], mu)
  expect_equal(response_distribution(m, response = 1)$mean, mu)
  expect_equal(response_distribution(m, response = 1)$variance,
    mu + dispersion * mu^2)
  lower <- pnbinom(m$y1 - 1, mu = mu, size = 1 / dispersion)
  upper <- pnbinom(m$y1, mu = mu, size = 1 / dispersion)
  expect_true(all(d$data$residuals[rows] >= lower))
  expect_true(all(d$data$residuals[rows] <= upper))
  expect_equal(response_distribution(m, response = 1)$cdf(m$y1), as.vector(upper))
  expect_equal(d$data$residuals[!rows],
    as.vector(pnorm(m$y2, as.vector(m$eta2), sigma)))
  expect_equal(response_distribution(m, response = 2)$variance, sigma^2)
  expect_silent(patchwork::patchworkGrob(appraise(m, seed = 69)))
})

test_that("joint diagnostic rows follow the retained fitting observations after omissions", {
  m <- gjrm_joint_fixture("omitted")
  expect_equal(length(m$y1), 178L)
  d <- diagnostic_data(m, seed = 71)
  for (i in 1:2) {
    rows <- d$data$.response == i
    frame <- m[[paste0("gam", i)]]$model
    original <- m$call$data
    retained <- match(rownames(frame), rownames(original))
    expect_false(anyNA(retained))
    expect_equal(d$data$observed[rows],
      as.vector(original[[paste0("y", i)]][retained]))
    expect_equal(d$data$observed[rows], as.vector(m[[paste0("y", i)]]))
    expect_equal(d$data$.row[rows], seq_len(nrow(frame)))
    selected <- diagnostic_data(m, response = i)
    expect_equal(selected$data$observed, d$data$observed[rows])
  }
  expect_equal(dim(quantile_residuals(m)), c(178L, 2L))
  expect_silent(patchwork::patchworkGrob(appraise(m, seed = 71)))
})

test_that("joint residual plots against covariates retain responses and fitting rows", {
  m <- gjrm_joint_fixture("omitted")
  for (fun in list(residuals_covariate_plot, scalable_residuals_covariate_plot)) {
    p <- fun(m, covariates = "z", smooth = FALSE, seed = 82)
    expect_s3_class(p, "ggplot")
    expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
    selected <- fun(m, covariates = "z", response = 2, smooth = FALSE, seed = 82)
    expect_equal(nrow(ggplot2::ggplot_build(selected)$layout$layout), 1L)
  }
})
