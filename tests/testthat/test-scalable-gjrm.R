test_that("GJRM adapters use final distributional means and residuals for each family", {
  skip_if_not_installed("GJRM")
  ns <- asNamespace("GJRM")
  set.seed(27)
  dat <- data.frame(x = runif(200, -1, 1))
  for (fam in c("N", "LN", "GA", "GAi", "P", "NBI", "NBII")) {
    dat$y <- GJRM::sim.resp(
      fam,
      nrow(dat),
      eta = if (fam == "GAi") 2 + dat$x / 4 else dat$x / 4,
      sigma2 = 0.5,
      nu = 1
    )
    f <- if (fam == "P") list(y ~ x) else list(y ~ x, ~x)
    # Current GJRM exposes GAi in distribution helpers but rejects it in
    # gamlss(). Represent the equivalent identity-mean Gamma parameterization
    # using the final fitted distribution from a supported GA fit.
    m <- GJRM::gamlss(f, data = dat, family = if (fam == "GAi") "GA" else fam)
    if (fam == "GAi") {
      m$margins[] <- "GAi"
      m$VC$margins[] <- "GAi"
      m$eta1 <- exp(m$eta1)
    }
    d <- diagnostic_data(m, seed = 87)
    mu <- as.vector(m$eta1)
    expected_mean <- if (fam %in% c("N", "GAi")) {
      mu
    } else if (fam == "LN") {
      exp(mu + as.vector(m$sigma2)^2 / 2)
    } else {
      exp(mu)
    }
    expect_equal(d$data$fitted, expected_mean, info = fam)
    expect_equal(d$data$eta, mu, info = fam)
    expect_equal(d$data$observed, dat$y, info = fam)
    cdf <- as.vector(m$fit$p)
    if (fam %in% c("P", "NBI", "NBII")) {
      cdf <- withr::with_seed(
        87,
        runif(length(cdf), pmax(0, cdf - as.vector(m$fit$pdf2)), cdf)
      )
    }
    expect_equal(d$data$residuals, qnorm(cdf), info = fam)
    # Check fitted probabilities against the response distribution independently.
    pfun <- get(
      switch(
        fam,
        N = "pNO",
        LN = "pLOGNO",
        GA = "pGA",
        GAi = "pGA",
        P = "pPO",
        NBI = "pNBI",
        NBII = "pNBII"
      ),
      envir = ns
    )
    args <- list(
      q = dat$y,
      mu = if (fam %in% c("N", "LN", "GAi")) mu else exp(mu)
    )
    if (fam != "P") {
      args$sigma <- as.vector(m$sigma2)
    }
    expect_equal(
      as.vector(m$fit$p),
      do.call(pfun, args),
      tolerance = 1e-6,
      info = fam
    )
    before <- get0(".Random.seed", envir = .GlobalEnv)
    a <- appraise(m, scalable = TRUE, seed = 87, qq_bins = 20)
    expect_identical(get0(".Random.seed", envir = .GlobalEnv), before)
    expect_equal(sum(a[[2]]$data$count), nrow(dat))
    expect_equal(sum(a[[3]]$data$count), nrow(dat))
    expect_equal(a[[1]]$data, scalable_qq_plot(d, qq_bins = 20)$data)
    expect_silent(ggplot2::ggplot_build(a[[2]]))
    ordinary <- appraise(m, scalable = FALSE, seed = 87)
    expect_equal(ordinary[[2]]$data$residuals, d$data$residuals)
    expect_equal(nrow(ordinary[[1]]$data), nrow(dat))
    expect_equal(qq_plot(m, seed = 87)$data$residuals, sort(d$data$residuals))
    expect_equal(
      worm_plot(m, seed = 87)$data$residuals,
      sort(d$data$residuals) - qnorm(ppoints(nrow(dat)))
    )
    expect_error(scalable_qq_plot(m, method = "simulate"), "method = 'normal'")
    expect_error(
      scalable_residuals_hist_plot(m, type = "pearson"),
      "type = 'quantile'"
    )
  }
})

test_that("GJRM class collisions and unsupported fits are rejected", {
  expect_error(
    diagnostic_data(structure(list(), class = "gamlss")),
    "Only univariate"
  )
  dummy <- structure(
    list(
      univar.gamlss = TRUE,
      margins = "WEI",
      cens = 1,
      VC = list(surv = FALSE)
    ),
    class = c("gamlss", "SemiParBIV")
  )
  expect_error(diagnostic_data(dummy), "uncensored")
  dummy$margins <- "N"
  dummy$cens <- 0
  expect_error(diagnostic_data(dummy), "uncensored")
  dummy$univar.gamlss <- FALSE
  expect_error(diagnostic_data(dummy), "Only univariate")
})

test_that("GJRM standard-normal bands and worm detrending are aligned", {
  skip_if_not_installed("GJRM")
  dat <- withr::with_seed(2, data.frame(y = rnorm(100), x = runif(100)))
  m <- GJRM::gamlss(list(y ~ x, ~1), data = dat)
  d <- diagnostic_data(m)
  ref <- diagnostic_qq_data(m, d, level = 0.9)
  expect_equal(ref$data$lower, qnorm(qbeta(0.05, 1:100, 100:1)))
  expect_equal(ref$data$upper, qnorm(qbeta(0.95, 1:100, 100:1)))
  expect_equal(ref$intercept, 0)
  expect_equal(ref$slope, 1)
  expect_equal(
    scalable_worm_plot(m, qq_bins = 15)$data,
    bin_diagnostic_qq(ref, 15, worm = TRUE)
  )
})
