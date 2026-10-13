# Reference the pre-optimization quantiles, with their default percentage
# names, from the same seeded residual draws. Derive observation count from
# the simulation matrix so scalar and multivariate responses share the oracle.
named_simulation_quantiles <- function(model, n, type, level, detrend, seed) {
  model <- model_used_rows(model)
  fam <- fix_family_rd(stats::family(model))
  deviance <- if (is.null(fam$residuals)) fam$dev.resids else fam$residuals
  scale <- model$sig2
  if (is.null(scale)) scale <- summary(model)$dispersion
  sims <- withr::with_seed(seed, replicate(n,
    qq_simulate_data(fam$rd, fit = stats::fitted(model),
      weights = diagnostic_weights(model), sigma2 = scale,
      dev_resid_fun = deviance, var_fun = fam$variance,
      type = type, na_action = model$na.action, model = model)))
  theoretical <- stats::quantile(sims,
    probs = (seq_len(nrow(sims)) - .5) / nrow(sims), names = TRUE)
  alpha <- (1 - level) / 2
  intervals <- apply(sims, 1L, stats::quantile,
    probs = c(alpha, 1 - alpha), names = TRUE)
  residuals <- sort(as.vector(stats::residuals(model, type = type)))
  if (detrend) {
    residuals <- residuals - theoretical
    intervals <- sweep(intervals, 2L, theoretical)
  }
  list(theoretical = theoretical, residuals = residuals,
    lower = intervals[1L, ], upper = intervals[2L, ])
}

test_that("unnamed simulation quantiles retain all scalar numerical results", {
  withr::local_seed(227)
  d <- data.frame(x = seq(-1, 1, length.out = 36), wt = rep(c(.5, 1, 2), 12))
  families <- list(stats::gaussian(), stats::poisson(),
    stats::inverse.gaussian(link = "log"))
  responses <- list(1 + d$x + rnorm(36, sd = .5),
    rpois(36, exp(.3 + .5 * d$x)),
    rd_invgaussian(exp(.3 + .5 * d$x), d$wt, .4))
  for (i in seq_along(families)) {
    d$y <- responses[[i]]
    d$y[c(4, 19)] <- NA_real_
    m <- mgcv::gam(y ~ x, data = d, weights = wt, family = families[[i]],
      method = "REML", na.action = na.exclude)
    for (type in c("deviance", "response", "pearson")) {
      for (detrend in c(FALSE, TRUE)) {
        for (level in c(.8, .95)) {
          for (n in c(1L, 7L)) {
            expected <- named_simulation_quantiles(m, n, type, level,
              detrend, seed = 391)
            actual <- withr::with_seed(391,
              qq_simulate(m, n, type, level, detrend))
            for (column in names(expected)) {
              expect_identical(unname(actual[[column]]),
                unname(expected[[column]]))
            }
            expect_null(names(actual$theoretical))
            expect_equal(nrow(actual), 34L)
          }
        }
      }
    }
  }
})

test_that("unnamed simulation quantiles retain multivariate and worm results", {
  for (detrend in c(FALSE, TRUE)) {
    expected <- named_simulation_quantiles(m_mvn, n = 7L, type = "deviance",
      level = .9, detrend = detrend, seed = 821)
    actual <- withr::with_seed(821,
      qq_simulate(m_mvn, n = 7L, type = "deviance", detrend = detrend))
    for (column in names(expected)) {
      expect_identical(unname(actual[[column]]), unname(expected[[column]]))
    }
    expect_null(names(actual$theoretical))
    expect_equal(nrow(actual), length(as.vector(fitted(m_mvn))))
  }
})
