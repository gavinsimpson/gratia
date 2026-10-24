# Work with population moments, never moments of simulated observations.
moment_names <- function() {
  c("mean", "variance", "sd", "cv", "skewness", "kurtosis", "excess_kurtosis")
}

moment_theta <- function(family) {
  if (!is.function(family$getTheta)) return(numeric())
  if (inherits(family, "extended.family")) family$getTheta(TRUE) else family$getTheta()
}

moment_summary <- function(mean, variance, skewness, excess) {
  n <- max(length(mean), length(variance), length(skewness), length(excess))
  mean <- rep_len(mean, n)
  variance <- rep_len(variance, n)
  variance[variance < 0 | !is.finite(variance)] <- NA_real_
  sd <- sqrt(variance)
  cv <- sd / mean
  cv[!is.finite(mean) | mean <= 0 | !is.finite(sd)] <- NA_real_
  skewness <- rep_len(skewness, n)
  excess <- rep_len(excess, n)
  skewness[!is.finite(variance) | variance <= 0] <- NA_real_
  excess[!is.finite(variance) | variance <= 0] <- NA_real_
  cbind(mean = mean, variance = variance, sd = sd, cv = cv,
    skewness = skewness, kurtosis = excess + 3, excess_kurtosis = excess)
}

moment_from_raw <- function(raw) {
  m <- raw[, 1L]
  v <- raw[, 2L] - m^2
  third <- raw[, 3L] - 3 * m * raw[, 2L] + 2 * m^3
  fourth <- raw[, 4L] - 4 * m * raw[, 3L] + 6 * m^2 * raw[, 2L] - 3 * m^4
  moment_summary(m, v, third / v^1.5, fourth / v^2 - 3)
}

moment_bernoulli <- function(p) {
  v <- p * (1 - p)
  moment_summary(p, v, (1 - 2 * p) / sqrt(v), (1 - 6 * v) / v)
}

# The Gumbel raw moments give a convergent expansion about GEV shape zero.
# This avoids subtracting nearly equal Gamma functions in third/fourth moments.
moment_gev_series <- local({
  order <- 18L
  cumulant <- c(-digamma(1), vapply(2:order, function(k)
    (-1)^k * psigamma(1, deriv = k - 1L), numeric(1L)))
  raw <- c(1, numeric(order))
  stirling <- matrix(0, order + 1L, 5L)
  stirling[1L, 1L] <- 1
  for (k in seq_len(order)) {
    raw[k + 1L] <- sum(choose(k - 1L, 0:(k - 1L)) *
      cumulant[seq_len(k)] * rev(raw[seq_len(k)]))
    for (r in seq_len(min(k, 4L))) stirling[k + 1L, r + 1L] <-
      r * stirling[k, r + 1L] + stirling[k, r]
  }
  coefficient <- lapply(1:4, function(r) {
    k <- r:order
    factorial(r) * stirling[k + 1L, r + 1L] * raw[k + 1L] / factorial(k)
  })
  function(shape) {
    vapply(coefficient, function(a) {
      ans <- rep(0, length(shape))
      for (j in rev(seq_along(a))) ans <- ans * shape + a[j]
      ans
    }, numeric(length(shape)))
  }
})

moment_gev <- function(location, sigma, shape) {
  n <- length(shape)
  raw <- matrix(NA_real_, n, 4L)
  small <- which(abs(shape) < 0.02)
  if (length(small)) raw[small, ] <- moment_gev_series(shape[small])
  other <- setdiff(seq_len(n), small)
  for (r in 1:4) {
    i <- other[shape[other] < 1 / r]
    if (length(i)) raw[i, r] <- vapply(shape[i], function(x) {
      j <- 0:r
      sum(choose(r, j) * (-1)^(r - j) * gamma(1 - j * x)) / x^r
    }, numeric(1L))
  }
  ans <- moment_from_raw(raw)
  ans[, "mean"] <- location + sigma * gev_standard_mean(shape)
  ans[, "variance"] <- sigma^2 * gev_standard_variance(shape)
  ans[, "sd"] <- sqrt(ans[, "variance"])
  ans[, "cv"] <- ifelse(ans[, "mean"] > 0,
    ans[, "sd"] / ans[, "mean"], NA_real_)
  ans[shape >= 1, "mean"] <- NA_real_
  ans
}

moment_shash <- function(mu) {
  delta <- exp(mu[, 4L])
  a <- mu[, 3L] / delta
  P <- function(r) exp(0.25) / sqrt(8 * pi) *
    (besselK(0.25, (r / delta + 1) / 2) +
      besselK(0.25, (r / delta - 1) / 2))
  raw <- cbind(sinh(a) * P(1), (cosh(2 * a) * P(2) - 1) / 2,
    (sinh(3 * a) * P(3) - 3 * sinh(a) * P(1)) / 4,
    (cosh(4 * a) * P(4) - 4 * cosh(2 * a) * P(2) + 3) / 8)
  ans <- moment_from_raw(raw)
  sigma <- delta * exp(mu[, 2L])
  mean <- mu[, 1L] + sigma * raw[, 1L]
  result <- moment_summary(mean, sigma^2 * ans[, "variance"],
    ans[, "skewness"], ans[, "excess_kurtosis"])
  # Bessel expressions lose precision when powers of sinh become very small.
  # Gaussian quadrature evaluates centred moments directly in that regime.
  large <- which(delta > 10)
  if (length(large)) {
    quadrature <- moment_normal_quadrature()
    for (i in large) {
      z <- asinh(quadrature$x) / delta[i]
      coshm1 <- 2 * sinh(z / 2)^2
      average <- sum(quadrature$w * coshm1)
      centred <- sinh(a[i]) * (coshm1 - average) + cosh(a[i]) * sinh(z)
      v <- sum(quadrature$w * centred^2)
      result[i, ] <- moment_summary(mu[i, 1L] + sigma[i] * sinh(a[i]) * (1 + average),
        sigma[i]^2 * v, sum(quadrature$w * centred^3) / v^1.5,
        sum(quadrature$w * centred^4) / v^2 - 3)
    }
  }
  result
}

moment_normal_quadrature <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      n <- 320L
      J <- matrix(0, n, n)
      J[cbind(seq_len(n - 1L), 2:n)] <- sqrt(seq_len(n - 1L))
      eig <- eigen(J + t(J), symmetric = TRUE)
      value <<- list(x = eig$values, w = eig$vectors[1L, ]^2)
    }
    value
  }
})

# Positive counts follow a zero-truncated Poisson, with a separate probability
# of a positive response. Use its Poisson limit when truncation is negligible.
moment_hurdle <- function(lambda, presence) {
  a <- lambda / (-expm1(-lambda))
  a[lambda == 0] <- 1
  raw <- cbind(a, a * (1 + lambda), a * (1 + 3 * lambda + lambda^2),
    a * (1 + 7 * lambda + 6 * lambda^2 + lambda^3))
  z <- moment_from_raw(raw)
  v <- z[, "variance"]
  t <- z[, "skewness"] * v^1.5
  q <- z[, "kurtosis"] * v^2
  tiny <- lambda < 1e-5
  v[tiny] <- t[tiny] <- q[tiny] <- lambda[tiny] / 2
  large <- lambda > 30
  v[large] <- t[large] <- lambda[large]
  q[large] <- lambda[large] + 3 * lambda[large]^2
  p <- presence
  mean <- p * a
  variance <- p * v + p * (1 - p) * a^2
  third <- p * t + 3 * p * (1 - p) * a * v +
    p * (1 - p) * (1 - 2 * p) * a^3
  fourth <- p * q + 4 * p * (1 - p) * a * t +
    6 * p * (1 - p)^2 * a^2 * v +
    p * (1 - p) * ((1 - p)^3 + p^3) * a^4
  moment_summary(mean, variance, third / variance^1.5, fourth / variance^2 - 3)
}

moment_family_values <- function(family, eta, scale, beta, scores = NULL) {
  ft <- family_type(family)
  n <- NROW(eta)
  th <- moment_theta(family)
  category <- response <- NULL
  if (ft == "multinom" || startsWith(ft, "ordered_categorical")) {
    if (ft == "multinom") {
      z <- cbind(0, eta)
      z <- exp(z - apply(z, 1L, max))
      p <- z / rowSums(z)
      labels <- seq.int(0, ncol(p) - 1L)
    } else {
      p <- ordinal_probability_transform(eta[, 1L], th)$probability
      labels <- seq_len(ncol(p))
    }
    if (is.null(scores)) {
      ans <- moment_bernoulli(as.vector(p))
      category <- rep(as.character(labels), each = n)
    } else {
      if (!is.numeric(scores) || length(scores) != ncol(p) || any(!is.finite(scores)))
        stop("'scores' must supply one finite numeric score per category.", call. = FALSE)
      # Centre scores before computing powers to retain location accuracy.
      centre <- min(scores)
      raw <- vapply(1:4, function(k) drop(p %*% (scores - centre)^k), numeric(n))
      ans <- moment_from_raw(raw)
      ans <- moment_summary(ans[, "mean"] + centre, ans[, "variance"],
        ans[, "skewness"], ans[, "excess_kurtosis"])
    }
  } else if (ft == "multivariate_normal") {
    R <- posterior_likelihood_mvn_R(beta, NCOL(eta))
    variance <- rep(diag(solve(crossprod(R))), each = n)
    ans <- moment_summary(as.vector(eta), variance, 0, 0)
    response <- rep(paste0("response", seq_len(NCOL(eta))), each = n)
  } else {
    mu <- family_prediction_parameters(family, eta)
    m <- if (is.matrix(mu)) mu[, 1L] else as.vector(mu)
    invalid <- moment_invalid_mu(ft, m)
    m[invalid] <- NA_real_
    if (is.matrix(mu)) mu[invalid, 1L] <- NA_real_
    ans <- switch(ft,
      gaussian = moment_summary(m, scale, 0, 0),
      cnorm = moment_summary(m, th[1L]^2, 0, 0),
      clog = moment_summary(m, pi^2 * th[1L]^2 / 3, 0, 6 / 5),
      poisson = moment_summary(m, m, 1 / sqrt(m), 1 / m),
      cpois = moment_summary(m, m, 1 / sqrt(m), 1 / m),
      binomial = moment_bernoulli(m),
      gamma = moment_summary(m, scale * m^2, 2 * sqrt(scale), 6 * scale),
      inverse_gaussian = moment_summary(m, scale * m^3,
        3 * sqrt(scale * m), 15 * scale * m),
      negative_binomial = {
        k <- th[1L]
        v <- m + m^2 / k
        moment_summary(m, v, (1 + 2 * m / k) / sqrt(v), 6 / k + 1 / v)
      },
      beta_regression = {
        k <- th[1L]
        v <- m * (1 - m)
        moment_summary(m, v / (k + 1),
          2 * (1 - 2 * m) * sqrt(k + 1) / ((k + 2) * sqrt(v)),
          6 * ((2 * m - 1)^2 * (k + 1) - v * (k + 2)) /
            (v * (k + 2) * (k + 3)))
      },
      scaled_t = {
        df <- th[1L]
        moment_summary(if (df > 1) m else rep(NA_real_, n),
          if (df > 2) th[2L]^2 / (1 - 2 / df) else NA_real_,
          if (df > 3) 0 else NA_real_, if (df > 4) 6 / (df - 4) else NA_real_)
      },
      tweedie = {
        p <- if (length(th)) th[1L] else get_tw_params(family)[1L]
        cv <- sqrt(scale) * m^(p / 2 - 1)
        moment_summary(m, scale * m^p, p * cv, p * (2 * p - 1) * cv^2)
      },
      gaulss = moment_summary(mu[, 1L], 1 / mu[, 2L]^2, 0, 0),
      gammals = {
        phi <- exp(mu[, 2L])
        moment_summary(mu[, 1L], phi * mu[, 1L]^2, 2 * sqrt(phi), 6 * phi)
      },
      gumbls = moment_summary(mu[, 1L], exp(2 * mu[, 2L]) * pi^2 / 6,
        12 * sqrt(6) * 1.202056903159594 / pi^3, 12 / 5),
      gevlss = moment_gev(mu[, 1L], exp(mu[, 2L]), mu[, 3L]),
      shash = moment_shash(mu),
      twlss = {
        ab <- get_tw_params(family)
        p <- theta_2_power(mu[, 2L], ab[1L], ab[2L])
        phi <- exp(mu[, 3L])
        cv <- sqrt(phi) * mu[, 1L]^(p / 2 - 1)
        moment_summary(mu[, 1L], phi * mu[, 1L]^p, p * cv, p * (2 * p - 1) * cv^2)
      },
      ziplss = moment_hurdle(exp(eta[, 1L]), -expm1(-exp(eta[, 2L]))),
      bcg = {
        if (th[1L] != 0) stop("Original-response moments for bcg require a fixed Box-Cox parameter of zero; a Gaussian transformed response does not specify how to complete the inverse transformation outside its domain.", call. = FALSE)
        v <- expm1(th[2L]^2)
        moment_summary(exp(m + th[2L]^2 / 2), v * exp(2 * m + th[2L]^2),
          (v + 3) * sqrt(v), exp(4 * th[2L]^2) + 2 * exp(3 * th[2L]^2) +
            3 * exp(2 * th[2L]^2) - 6)
      },
      NULL)
    if (startsWith(ft, "zero_inflated_poisson")) ans <-
      moment_hurdle(exp(eta[, 1L]), -expm1(-exp(th[1L] + th[2L] * eta[, 1L])))
    if (is.null(ans)) stop("Response moments are not implemented for family <",
      family_name(family), ">.", call. = FALSE)
    # Inverse and identity links can leave a family's parameter domain in
    # either a Gaussian draw or a numerical perturbation. No moment is valid
    # for those parameter values, even if a formula happens to return a number.
    ans[invalid, ] <- NA_real_
  }
  list(values = ans, status = moment_family_status(family, eta, ans),
    category = category, response = response,
    row = rep(seq_len(n), length.out = nrow(ans)))
}

moment_invalid_mu <- function(family, mu) {
  invalid <- !is.finite(mu)
  if (family %in% c("poisson", "cpois", "gamma", "inverse_gaussian",
      "negative_binomial", "tweedie", "gammals", "twlss")) invalid <- invalid | mu <= 0
  if (family %in% c("binomial", "beta_regression")) invalid <- invalid | mu < 0 | mu > 1
  if (family == "beta_regression") invalid <- invalid | mu == 0 | mu == 1
  invalid
}

moment_family_status <- function(family, eta, values) {
  status <- matrix("ok", nrow(values), ncol(values), dimnames = dimnames(values))
  status[!is.finite(values)] <- "numerical_failure"
  status[!is.finite(values[, "cv"]) & is.finite(values[, "mean"]) &
    values[, "mean"] <= 0, "cv"] <- "nonpositive_mean"
  zero <- which(!is.na(values[, "variance"]) & values[, "variance"] == 0)
  status[zero, c("skewness", "kurtosis", "excess_kurtosis")] <- "zero_variance"
  ft <- family_type(family)
  if (ft %in% c("gevlss", "scaled_t")) {
    order <- c(mean = 1, variance = 2, sd = 2, cv = 2,
      skewness = 3, kurtosis = 4, excess_kurtosis = 4)
    shape <- if (ft == "gevlss") family_prediction_parameters(family, eta)[, 3L] else
      rep(moment_theta(family)[1L], nrow(values))
    for (j in seq_along(order)) {
      bad <- if (ft == "gevlss") shape >= 1 / order[j] else shape <= order[j]
      status[bad, j] <- "moment_does_not_exist"
    }
  }
  status
}

# One joint model supplies coefficients and global parameters. Its component
# list supplies the observation distribution for each row, never a separate fit.
moment_grouped_values <- function(context, family, eta, beta, scores) {
  components <- gfam_components(family)
  values <- matrix(NA_real_, nrow(eta), length(moment_names()),
    dimnames = list(NULL, moment_names()))
  status <- matrix("missing_predictors", nrow(eta), length(moment_names()),
    dimnames = dimnames(values))
  for (i in seq_along(components)) {
    rows <- which(context$grouped$index == i)
    if (!length(rows)) next
    component <- components[[i]]
    result <- moment_family_values(component$family, eta[rows, , drop = FALSE],
      component$scale, beta, scores)
    if (nrow(result$values) != length(rows)) stop(
      "Grouped categorical moments require explicit category scores; category-indicator output is not supported for grouped families.", call. = FALSE)
    values[rows, ] <- result$values
    status[rows, ] <- result$status
  }
  list(values = values, status = status, row = seq_len(nrow(eta)))
}
