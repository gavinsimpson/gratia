test_that("censored family helpers describe the weighted latent distribution", {
  mu <- c(1, 2, 3, 4)
  wt <- c(0.5, 1, 2, 4)
  p <- c(0.1, 0.3, 0.7, 0.9)
  sigma <- 2.3
  families <- list(mgcv::cnorm(theta = sigma), mgcv::clog(theta = sigma),
    mgcv::cpois())
  for (i in seq_along(families)) {
    fam <- families[[i]] |> fix_family_rd() |> fix_family_cdf() |>
      fix_family_qf()
    q <- switch(i,
      qnorm(p, mu, sigma / sqrt(wt)),
      qlogis(p, mu, sigma / sqrt(wt)),
      qpois(p, mu)
    )
    prob <- switch(i,
      pnorm(q, mu, sigma / sqrt(wt)),
      plogis(q, mu, sigma / sqrt(wt)),
      ppois(q, mu)
    )
    # Deliberately use a dispersion different from the family scale.
    expect_equal(fam$qf(p, mu, wt, scale = 17), q)
    expect_equal(fam$qf(log(p), mu, wt, scale = 17, log_p = TRUE), q)
    expect_equal(fam$cdf(q, mu, wt, scale = 17), prob)
    expect_equal(fam$cdf(q, mu, wt, scale = 17, log_p = TRUE), log(prob))
    expect_equal(fam$cdf(c(-Inf, Inf), mu = 2, wt = 1, scale = 17), c(0, 1))
    expected <- withr::with_seed(42, switch(i,
      rnorm(length(mu), mu, sigma / sqrt(wt)),
      rlogis(length(mu), mu, sigma / sqrt(wt)),
      rpois(length(mu), mu)
    ))
    expect_equal(withr::with_seed(42, fam$rd(mu, wt, scale = 17)), expected)
    expect_identical(fix_family_rd(fam)$rd, fam$rd)
    expect_identical(fix_family_cdf(fam)$cdf, fam$cdf)
    expect_identical(fix_family_qf(fam)$qf, fam$qf)
  }
})

test_that("censored residuals use exact, left, right and interval probabilities", {
  # Include reversed finite bounds, as supported by mgcv.
  y <- cbind(c(0, 2, 2.5, 3.5, 1.5, 5.5), c(0, 2, -Inf, Inf, 4.5, 2.5))
  mu <- rep(3, nrow(y))
  wt <- c(1, 2, 1, 0.5, 2, 1)
  for (fam in list(mgcv::cnorm(theta = 2), mgcv::clog(theta = 2), mgcv::cpois())) {
    ft <- family_type(fam)
    lower <- c(0, 2, -Inf, 3.5, 1.5, 2.5)
    upper <- c(0, 2, 2.5, Inf, 4.5, 5.5)
    if (ft == "cpois") {
      lower[1:2] <- lower[1:2] - 1
    }
    cdf <- switch(ft,
      cnorm = function(q) pnorm(q, mu, 2 / sqrt(wt)),
      clog = function(q) plogis(q, mu, 2 / sqrt(wt)),
      cpois = function(q) ppois(q, mu)
    )
    expected <- withr::with_seed(12, runif(nrow(y), cdf(lower), cdf(upper)))
    for (response in list(y, structure(y[, 1], censor = y[, 2]))) {
      pit <- withr::with_seed(12,
        do_quantile_residuals(response, mu, wt, 1, fam, type = "pit"))
      expect_equal(pit, expected)
      rq <- withr::with_seed(12,
        do_quantile_residuals(response, mu, wt, 1, fam, type = "quantile"))
      expect_equal(rq, qnorm(expected))
    }
    plain <- c(0, 1, 2, 3, 4, 5)
    expected <- if (ft == "cpois") {
      withr::with_seed(12, runif(length(plain), cdf(plain - 1), cdf(plain)))
    } else {
      cdf(plain)
    }
    expect_equal(withr::with_seed(12,
      do_quantile_residuals(plain, mu, wt, 1, fam, type = "pit")), expected)
  }
})

test_that("fitted censored GAMs use estimated theta and preserve the seed", {
  withr::local_seed(42)
  n <- 120
  x <- runif(n)
  wt <- rep(c(0.5, 1, 2), length.out = n)
  for (ft in c("cnorm", "clog", "cpois")) {
    y <- switch(ft,
      cnorm = rnorm(n, 2 + x, 1.7 / sqrt(wt)),
      clog = rlogis(n, 2 + x, 1.7 / sqrt(wt)),
      cpois = rpois(n, exp(1 + x))
    )
    # Censor using fixed thresholds, and round remaining observations into
    # intervals on every third row, retaining some exact observations.
    lo <- hi <- y
    left <- y < 0.5
    right <- y > 5.5
    interval <- !left & !right & seq_len(n) %% 3 == 0
    lo[left] <- 0.5
    hi[left] <- -Inf
    lo[right] <- 5.5
    hi[right] <- Inf
    lo[interval] <- round(y[interval]) - 0.5
    hi[interval] <- round(y[interval]) + 0.5
    dat <- data.frame(x, wt)
    dat$y <- cbind(lo, hi)
    fam <- get(ft, envir = asNamespace("mgcv"))()
    m <- mgcv::gam(y ~ s(x, k = 5), data = dat, weights = wt,
      family = fam, method = "REML")
    expect_equal(attr(m$y, "censor"), hi)
    state <- .Random.seed
    pit <- quantile_residuals(m, type = "pit", seed = 19)
    expect_identical(.Random.seed, state)
    expect_length(pit, n)
    expect_true(all(is.finite(pit) & pit >= 0 & pit <= 1))
    expect_equal(quantile_residuals(m, type = "quantile", seed = 19), qnorm(pit))
    lower <- pmin(lo, hi)
    upper <- pmax(lo, hi)
    if (ft == "cpois") {
      lower[lo == hi] <- lo[lo == hi] - 1
    }
    sigma <- if (ft != "cpois") m$family$getTheta(TRUE) else NULL
    cdf <- switch(ft,
      cnorm = function(q) pnorm(q, fitted(m), sigma / sqrt(wt)),
      clog = function(q) plogis(q, fitted(m), sigma / sqrt(wt)),
      cpois = function(q) ppois(q, fitted(m))
    )
    expected <- withr::with_seed(19, runif(n, cdf(lower), cdf(upper)))
    expect_equal(pit, expected)
  }
})
