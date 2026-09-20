test_that("simultaneous intervals use consistent smooth-only uncertainty", {
  withr::local_seed(5005)
  d <- data.frame(x = runif(160), z = runif(160))
  d$y <- sin(6 * d$x) + d$z^2 + rnorm(nrow(d), sd = 0.4)
  model <- mgcv::gam(y ~ s(x, k = 8) + s(z, k = 6),
    data = d, method = "REML")
  nd <- data.frame(x = seq(0.1, 0.9, length.out = 15), z = 0.5)
  sm <- model$smooth[[1L]]
  cols <- sm$first.para:sm$last.para
  X <- predict(model, newdata = nd, type = "lpmatrix")[, cols, drop = FALSE]
  estimate <- unname(drop(X %*% coef(model)[cols]))
  level <- 0.9
  nsim <- 500L
  seed <- 5006L

  # Ensure this fixture can detect covariance options being ignored.
  expect_false(is.null(model$Vc))
  expect_gt(max(abs(model$Ve - model$Vp)), 1e-6)
  expect_gt(max(abs(model$Vc - model$Vp)), 1e-6)
  for (frequentist in c(FALSE, TRUE)) {
    for (unconditional in c(FALSE, TRUE)) {
      # Select independently of get_vcov(); frequentist takes precedence when
      # both options are TRUE.
      V <- if (frequentist) model$Ve else if (unconditional) model$Vc else model$Vp
      se <- unname(sqrt(rowSums((X %*% V[cols, cols]) * X)))
      smooth <- smooth_estimates(model, select = "s(x)", data = nd,
        frequentist = frequentist, unconditional = unconditional,
        overall_uncertainty = FALSE)

      # Use the same seeded coefficient draws and single-threaded backend,
      # but independently construct the smooth errors and their standardized
      # maxima. These are numerical equivalence checks, not coverage estimates.
      draws <- withr::with_seed(seed, mvnfast::rmvn(nsim,
        mu = rep(0, ncol(V)), sigma = V, ncores = 1))
      errors <- draws[, cols, drop = FALSE] %*% t(X)
      maxima <- apply(abs(sweep(errors, 2L, se, `/`)), 1L, max)
      critical <- unname(quantile(maxima, probs = level, type = 8))

      # Simultaneous intervals exclude overall-mean uncertainty regardless of
      # this argument, as documented; pointwise intervals support both modes.
      for (overall in c(FALSE, TRUE)) {
        actual <- withr::with_seed(seed, confint(model, parm = "s(x)",
          data = nd, type = "simultaneous", level = level, nsim = nsim,
          ncores = 1, frequentist = frequentist, unconditional = unconditional,
          overall_uncertainty = overall))
        expect_identical(actual$.se, smooth$.se)
        expect_equal(unname(actual$.estimate), estimate, tolerance = 1e-10)
        expect_equal(unname(actual$.se), se, tolerance = 1e-10)
        expect_equal(actual$.crit, rep(critical, nrow(nd)), tolerance = 1e-10)
        expect_equal(unname(actual$.lower_ci), estimate - critical * se,
          tolerance = 1e-10)
        expect_equal(unname(actual$.upper_ci), estimate + critical * se,
          tolerance = 1e-10)
      }
    }
  }
})
