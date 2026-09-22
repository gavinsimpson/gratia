test_that("both Tweedie family factories use mixtures and preserve log tails", {
  for (fam in list(mgcv::tw(theta = 1.5), mgcv::Tweedie(p = 1.5))) {
    qf <- fix_family_qf(fam)$qf
    cdf <- fix_family_cdf(fam)$cdf
    mu <- c(.5, 1, 2)
    lp <- c(log(.5), log(.9), -1e-30)
    q <- qf(lp, mu, 1, .7, log_p = TRUE)
    expect_equal(q, qtweedie_mixture(lp, mu, 1.5, .7, log_p = TRUE))
    expect_equal(cdf(q, mu, 1, .7, log_p = TRUE, lower_tail = FALSE),
      log1mexp(lp), tolerance = 1e-8)
    expect_warning(qf(.9, 1, 1, 1, max_terms = 5), "fallback = TRUE")
    expect_warning(cdf(1, 1, 1, 1, max_terms = 5), "fallback = TRUE")
  }
})

test_that("Gamma limit requires no Tweedie backend and honours both tails", {
  local_mocked_bindings(
    qtweedie_mixture = function(...) stop("Unexpected mixture"),
    ptweedie_mixture = function(...) stop("Unexpected mixture"),
    require_tweedie_fallback = function(...) stop("Unexpected inversion")
  )
  fam <- mgcv::Tweedie(p = 2)
  qf <- fix_family_qf(fam)$qf
  cdf <- fix_family_cdf(fam)$cdf
  lp <- c(-Inf, log(.5), -1e-30, 0)
  mu <- c(1, 2, 3, 4)
  expect_equal(qf(lp, mu, 1, .7, log_p = TRUE),
    qgamma(lp, shape = 1/.7, scale = mu*.7, log.p = TRUE))
  for (lower in c(TRUE, FALSE)) {
    expect_equal(cdf(1, mu, 1, .7, log_p = TRUE, lower_tail = lower),
      pgamma(1, shape = 1/.7, scale = mu*.7, log.p = TRUE, lower.tail = lower))
  }
})

test_that("missing optional backend errors only when inversion is needed", {
  # Retain namespace checks for all other optional packages.
  available <- base::requireNamespace
  local_mocked_bindings(requireNamespace = function(package, quietly = FALSE, ...) {
    if (package == "tweedie") return(FALSE)
    available(package, quietly = quietly, ...)
  }, .package = "base")
  for (fam in list(mgcv::tw(theta = 1.5), mgcv::Tweedie(p = 1.5))) {
    qf <- fix_family_qf(fam)$qf
    cdf <- fix_family_cdf(fam)$cdf
    expect_silent(qf(.9, 1, 1, 1))
    expect_silent(cdf(1, 1, 1, 1))
    expect_silent(qf(.9, 1, 1, 1, fallback = TRUE))
    expect_silent(cdf(1, 1, 1, 1, fallback = TRUE))
    for (fun in list(qf, cdf)) {
      err <- expect_error(fun(.9, 1, 1, 1, max_terms = 1, fallback = TRUE),
        "Tweedie fallback requires", class = "rlang_error")
      expect_match(conditionMessage(err), 'install.packages("tweedie")', fixed = TRUE)
    }
  }
})

test_that("public diagnostics use mixtures without the optional backend", {
  local_mocked_bindings(require_tweedie_fallback = function(...) {
    stop("Unexpected optional backend")
  })
  withr::local_seed(410)
  d <- data.frame(x = seq(0, 1, length.out = 30))
  d$y <- mgcv::rTweedie(exp(d$x), p = 1.5, phi = 1)
  for (fam in list(mgcv::tw(), mgcv::Tweedie(p = 1.5))) {
    m <- mgcv::gam(y ~ x, family = fam, data = d, method = "REML")
    expect_silent(qq <- qq_plot(m, method = "uniform", n_uniform = 1))
    expect_equal(qq$labels$subtitle, "Method: uniform")
    expect_silent(worm_plot(m, method = "uniform", n_uniform = 1))
    expect_silent(appraise(m, method = "uniform", n_uniform = 1))
    expect_equal(qq_plot(m, n_simulate = 2)$labels$subtitle, "Method: simulate")
    expect_silent(pit <- quantile_residuals(m, type = "pit"))
    expect_silent(z <- quantile_residuals(m, type = "quantile"))
    expect_true(all(is.finite(pit) & pit >= 0 & pit <= 1))
    expect_equal(z, qnorm(pit), tolerance = 1e-8)
  }
})
