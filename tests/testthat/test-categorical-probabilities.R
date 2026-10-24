# Integrating the latent density provides a reference independent of CDF
# subtraction. Tail densities also give independent probability derivatives.
ordinal_probability_reference <- function(eta, thresholds) {
  cuts <- c(-Inf, thresholds, Inf)
  p <- complement <- slope <- matrix(NA_real_, length(eta), length(cuts) - 1L)
  for (j in seq_len(ncol(p))) {
    lo <- cuts[j] - eta
    hi <- cuts[j + 1L] - eta
    p[, j] <- vapply(seq_along(eta), function(i) {
      if (is.na(eta[i])) return(NA_real_)
      if (j == 1L) return(plogis(hi[i]))
      if (j == ncol(p)) return(plogis(lo[i], lower.tail = FALSE))
      integrate(dlogis, lo[i], hi[i], abs.tol = 0, rel.tol = 1e-11)$value
    }, numeric(1L))
    complement[, j] <- plogis(lo) + plogis(hi, lower.tail = FALSE)
    slope[, j] <- dlogis(lo) - dlogis(hi)
  }
  list(p = p, complement = complement, slope = slope)
}

test_that("ordinal fitted probabilities and uncertainty retain both fitted tails", {
  m <- ordinal_tail_model()
  for (x in list(c(-50, -1, 0, 1, 50), -50, 50, c(-50, NA, 50))) {
    nd <- data.frame(x = x, label = letters[seq_along(x)])
    fit <- fitted_values(m, data = nd, quantity = "expectation")
    eta <- as.numeric(predict(m, nd, type = "link"))
    se_eta <- as.numeric(predict(m, nd, type = "link", se.fit = TRUE)$se.fit)
    ref <- ordinal_probability_reference(eta, family(m)$getTheta(TRUE))
    # Relative comparisons ensure that small positive probabilities cannot pass
    # merely because they fall below an absolute comparison tolerance.
    ok <- !is.na(fit$.fitted)
    expect_equal(fit$.fitted[ok] / as.numeric(ref$p)[ok], rep(1, sum(ok)),
      tolerance = 1e-10)
    expected_se <- as.numeric(abs(ref$slope) * se_eta)
    expect_equal(fit$.se[ok] / expected_se[ok], rep(1, sum(ok)), tolerance = 1e-6)
    logit <- log(ref$p) - log(ref$complement)
    logit_se <- abs(ref$slope) * se_eta / (ref$p * ref$complement)
    critical <- qnorm(.975)
    expect_equal(fit$.lower_ci[ok] / as.numeric(plogis(logit - critical * logit_se))[ok],
      rep(1, sum(ok)), tolerance = 1e-6)
    expect_equal(fit$.upper_ci[ok] / as.numeric(plogis(logit + critical * logit_se))[ok],
      rep(1, sum(ok)), tolerance = 1e-6)
    expect_identical(fit$.row, rep(seq_along(x), 3L))
    expect_identical(fit$.category, rep(paste0("category", 1:3), each = length(x)))
    expect_identical(fit$label, rep(nd$label, 3L))
    expect_true(all(is.na(fit$.se[!ok])))
    expect_true(all(is.na(fit$.lower_ci[!ok])))
    expect_true(all(is.na(fit$.upper_ci[!ok])))
    moments <- fitted_moments(m, data = nd, moments = "mean", uncertainty = "none")
    moments <- moments[order(as.integer(moments$.category), moments$.row), ]
    expect_equal(fit$.fitted, moments$.fitted)
    draws <- fitted_samples(m, data = nd, quantity = "expectation",
      method = "user", draws = matrix(coef(m), 1L))
    expect_equal(fit$.fitted, draws$.fitted)
  }
})

test_that("grouped ordinal tails retain probabilities, intervals and identities", {
  m <- gfam_ordinal_model
  for (x in list(c(-100, 0, 100, NA), -100, 100)) {
    nd <- gfam_ordinal_data[rep(1L, length(x)), ]
    nd$x <- x
    fit <- fitted_values(m, data = nd, quantity = "expectation")
    eta <- as.numeric(gfam_predict_eta(m, nd, type = "link"))
    X <- gfam_predict_eta(m, nd, type = "lpmatrix")
    se_eta <- sqrt(rowSums((X %*% vcov(m)) * X))
    f <- gfam_components(family(m))[[1L]]$family
    ref <- ordinal_probability_reference(eta, f$getTheta(TRUE))
    # Grouped output keeps each original row together before its categories.
    expected <- as.numeric(t(ref$p))
    ok <- !is.na(expected)
    expect_equal(fit$.fitted[ok] / expected[ok], rep(1, sum(ok)), tolerance = 1e-10)
    expect_equal(fit$.se[ok] / as.numeric(t(abs(ref$slope) * se_eta))[ok],
      rep(1, sum(ok)), tolerance = 1e-6)
    logit <- log(ref$p) - log(ref$complement)
    logit_se <- abs(ref$slope) * se_eta / (ref$p * ref$complement)
    expect_equal(fit$.lower_ci[ok] /
      as.numeric(t(plogis(logit - qnorm(.975) * logit_se)))[ok],
      rep(1, sum(ok)), tolerance = 1e-6)
    expect_equal(fit$.upper_ci[ok] /
      as.numeric(t(plogis(logit + qnorm(.975) * logit_se)))[ok],
      rep(1, sum(ok)), tolerance = 1e-6)
    expect_identical(fit$.row, rep(seq_along(x), each = 4L))
    expect_identical(fit$.category, rep(paste0("category", 1:4), length(x)))
    expect_true(all(fit$.family == 1L))
    expect_true(all(is.na(fit$.lower_ci[!ok])))
    expect_true(all(is.na(fit$.upper_ci[!ok])))
  }
})

test_that("rounded ordinal boundaries have defined intervals and missing inputs stay missing", {
  m <- ordinal_tail_model()
  # Large finite offsets reach both probability boundaries without changing
  # the fitted covariance. End-category logits have exact slopes of -1 and 1.
  m$coefficients[] <- c(0, 1)
  nd <- data.frame(x = c(-1000, 1000, NA_real_))
  fit <- fitted_values(m, data = nd, quantity = "expectation")
  expect_true(any(fit$.fitted == 0, na.rm = TRUE))
  expect_true(any(fit$.fitted == 1, na.rm = TRUE))
  valid <- fit$.row != 3L
  expect_true(all(is.finite(fit$.se[valid])))
  expect_true(all(is.finite(fit$.lower_ci[valid])))
  expect_true(all(is.finite(fit$.upper_ci[valid])))
  expect_true(all(fit$.lower_ci[valid] <= fit$.fitted[valid]))
  expect_true(all(fit$.upper_ci[valid] >= fit$.fitted[valid]))
  for (j in c(1L, 3L)) {
    sel <- fit$.category == paste0("category", j) & valid
    cuts <- family(m)$getTheta(TRUE)
    logit <- if (j == 1L) cuts[1L] - nd$x[1:2] else nd$x[1:2] - cuts[2L]
    se <- as.numeric(predict(m, nd[1:2, , drop = FALSE], se.fit = TRUE)$se.fit)
    expect_equal(fit$.lower_ci[sel], plogis(logit - qnorm(.975) * se), tolerance = 1e-7)
    expect_equal(fit$.upper_ci[sel], plogis(logit + qnorm(.975) * se), tolerance = 1e-7)
  }
  expect_true(all(is.na(fit$.lower_ci[!valid])))
  expect_true(all(is.na(fit$.upper_ci[!valid])))
})

test_that("ordinal transforms preserve gradients and distinguish missing inputs", {
  eta <- c(-1000, -50, 0, 50, 1000, NA_real_, NaN)
  thresholds <- c(-1, 1)
  transform <- ordinal_probability_transform(eta, thresholds)
  h <- 1e-3
  logit_gradient <- (ordinal_probability_transform(eta + h, thresholds)$logit -
    ordinal_probability_transform(eta - h, thresholds)$logit) / (2 * h)
  expect_equal(logit_gradient[1:5, 1L], rep(-1, 5L), tolerance = 1e-7)
  expect_equal(logit_gradient[1:5, 3L], rep(1, 5L), tolerance = 1e-7)
  ref <- ordinal_probability_reference(eta[2:4], thresholds)
  gradient <- (transform$derivative * logit_gradient)[2:4, ]
  nonzero <- ref$slope != 0
  expect_equal(gradient[nonzero] / ref$slope[nonzero],
    rep(1, sum(nonzero)), tolerance = 1e-6)
  expect_equal(gradient[!nonzero], rep(0, sum(!nonzero)), tolerance = 1e-10)
  for (value in transform) {
    expect_true(all(is.na(value[6L, ])))
    expect_false(any(is.nan(value[6L, ])))
    expect_true(all(is.nan(value[7L, ])))
  }
  # Enough uncertainty to cross the logit origin gives nondegenerate limits
  # even though the probabilities and probability-scale SEs underflow.
  limits <- categorical_probability_limits(transform,
    matrix(1000, length(eta), 3L), qnorm(.975))
  expect_equal(limits$lower[1:5, ], matrix(0, 5L, 3L))
  expect_equal(limits$upper[1:5, ], matrix(1, 5L, 3L))
  expect_true(all(is.na(limits$lower[6L, ])))
  expect_true(all(is.nan(limits$lower[7L, ])))
})

test_that("multinomial uncertainty agrees with independent log odds gradients", {
  for (size in c(1L, 2L)) {
    for (location in c(0, 40, 1000)) {
      m <- m_multinom
      nd <- m$model[seq_len(size), , drop = FALSE]
      X <- predict(m, nd, type = "lpmatrix")
      indices <- attr(X, "lpi")
      m$coefficients[] <- 0
      m$coefficients[indices[[1L]][1L]] <- location
      m$coefficients[indices[[2L]][1L]] <- -location
      fit <- fitted_values(m, data = nd)
      # Utilities are 0, location, -location. The two other categories give
      # explicit log odds and conditional category weights for each gradient.
      z <- c(0, location, -location)
      logits <- c(-location - log1p(exp(-2 * location)),
        location - log1p(exp(-location)), -2 * location - log1p(exp(-location)))
      expected_se <- matrix(NA_real_, size, 3L)
      for (j in 1:3) {
        other <- setdiff(1:3, j)
        weights <- exp(z[other] - max(z[other]))
        weights <- weights / sum(weights)
        slope <- numeric(3L)
        slope[j] <- 1
        slope[other] <- -weights
        J <- matrix(0, size, length(coef(m)))
        for (k in 1:2) J[, indices[[k]]] <- X[, indices[[k]], drop = FALSE] * slope[k + 1L]
        expected_se[, j] <- sqrt(rowSums((J %*% vcov(m)) * J))
      }
      logit <- matrix(rep(logits, each = size), size, 3L)
      expect_equal(fit$.fitted, as.numeric(plogis(logit)), tolerance = 1e-12)
      se <- as.numeric(plogis(logit) * plogis(-logit) * expected_se)
      nonzero <- se > 0
      expect_equal(fit$.se[nonzero] / se[nonzero], rep(1, sum(nonzero)), tolerance = 1e-6)
      expect_equal(fit$.se[!nonzero], se[!nonzero])
      expect_equal(fit$.lower_ci, as.numeric(plogis(logit - qnorm(.975) * expected_se)),
        tolerance = 1e-7)
      expect_equal(fit$.upper_ci, as.numeric(plogis(logit + qnorm(.975) * expected_se)),
        tolerance = 1e-7)
      expect_identical(fit$.row, rep(seq_len(size), 3L))
      expect_identical(fit$.category, rep(paste0("category", 1:3), each = size))
    }
  }
})
