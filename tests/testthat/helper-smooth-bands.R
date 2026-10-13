# Independent numerical oracle: use the native full model prediction matrix
# and simulate coefficient errors without calling gratia's interval/design code.
smooth_band_reference <- function(model, label, data, overall = FALSE,
                                  frequentist = FALSE, unconditional = FALSE,
                                  seed = 314L, n_sim = 200L, level = 0.9) {
  sm <- model$smooth[[match(label, vapply(model$smooth, `[[`, "", "label"))]]
  cols <- sm$first.para:sm$last.para
  native <- predict(model, newdata = data, type = "lpmatrix")
  V <- if (frequentist) model$Ve else if (unconditional && !is.null(model$Vc)) {
    model$Vc
  } else model$Vp
  X <- matrix(0, nrow(native), ncol(native))
  if (overall && !is.null(attr(sm, "nCons")) && attr(sm, "nCons") > 0) {
    means <- c(model$cmX, rep(0, ncol(native) - length(model$cmX)))
    if (!is.null(sm$meanL1)) means <- means / sm$meanL1
    X[,] <- rep(means, each = nrow(native))
    lpi <- attr(native, "lpi")
    if (!is.null(lpi)) {
      active <- unlist(lpi[vapply(lpi, function(idx) any(cols %in% idx), logical(1))])
      X[, setdiff(seq_len(ncol(X)), active)] <- 0
    }
  }
  X[, cols] <- native[, cols, drop = FALSE]
  estimate <- drop(native[, cols, drop = FALSE] %*% coef(model)[cols])
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  deviations <- withr::with_seed(seed, mvnfast::rmvn(n_sim,
    mu = rep(0, ncol(V)), sigma = V, ncores = 1))
  errors <- deviations %*% t(X)
  valid <- is.finite(se) & se > 0 & is.finite(estimate)
  maxima <- apply(abs(sweep(errors[, valid, drop = FALSE], 2L, se[valid], `/`)), 1L, max)
  critical <- unname(quantile(maxima, probs = level, type = 8))
  list(estimate = unname(estimate), se = unname(se), critical = critical,
    lower = unname(estimate - critical * se), upper = unname(estimate + critical * se))
}

expect_smooth_band_reference <- function(actual, reference) {
  expect_equal(unname(actual$.estimate), reference$estimate, tolerance = 1e-9)
  expect_equal(unname(actual$.se), reference$se, tolerance = 1e-9)
  expect_equal(actual$.crit, rep(reference$critical, nrow(actual)), tolerance = 1e-9)
  expect_equal(unname(actual$.lower_ci), reference$lower, tolerance = 1e-9)
  expect_equal(unname(actual$.upper_ci), reference$upper, tolerance = 1e-9)
}
