# Internal simulation helpers. Nothing in this file changes the package API.
coverage_configurations <- function() {
  data.frame(engine = c("gam", "gam", "bam", "bam", "bam"),
    method = c("ML", "REML", "ML", "REML", "fREML"), stringsAsFactors = FALSE)
}

coverage_cases <- function() {
  data.frame(case = c("normal_known", "normal_estimated", "normal_small", "normal_weak",
    "normal_null", "normal_shared", "normal_fixed", "normal_tensor",
    "poisson", "poisson_sparse", "binomial", "binomial_rare", "gamma", "nb", "beta", "student"),
    family = c(rep("normal", 8), "poisson", "poisson", "binomial", "binomial", "gamma", "nb", "beta", "student"),
    n = c(200L, 200L, 60L, 100L, 100L, 200L, 200L, 300L,
      250L, 400L, 200L, 300L, 250L, 300L, 250L, 250L),
    structure = c(rep("single", 5), "shared", "fixed", "tensor", rep("single", 8)),
    amplitude = c(1, 1, 1, .15, 0, rep(1, 11)),
    known_scale = c(TRUE, rep(FALSE, 15)), stringsAsFactors = FALSE)
}

coverage_definition <- function(spec) {
  knots <- if (spec$structure == "tensor") seq(0, 1, length.out = 6L) else
    seq(0, 1, length.out = 8L)
  f <- stats::splinefun(knots, sin(2 * pi * knots), method = "natural")
  g <- stats::splinefun(knots, sin(2 * pi * knots), method = "natural")
  a <- stats::splinefun(knots, sin(pi * knots), method = "natural")
  b <- stats::splinefun(knots, cos(pi * knots), method = "natural")
  signal <- function(x, z, derivative = FALSE) {
    order <- as.integer(derivative)
    if (spec$structure == "tensor")
      return(1.2 * a(x, deriv = order) * a(z) + .4 * f(x, deriv = order) * b(z))
    first <- spec$amplitude * f(x, deriv = order)
    if (!derivative && spec$structure %in% c("shared", "fixed")) first <- first + g(z)
    first
  }
  first_smooth <- function(x, z, derivative = FALSE) {
    if (spec$structure == "tensor") signal(x, z, derivative) else
      spec$amplitude * f(x, deriv = as.integer(derivative))
  }
  intercept <- switch(spec$case, poisson_sparse = -2, binomial_rare = -3,
    binomial = -.2, beta = -.2, .4)
  linkinv <- switch(spec$family, normal = , student = identity,
    binomial = , beta = stats::plogis, exp)
  list(knots = knots, signal = signal, first = first_smooth,
    intercept = intercept, inverse = linkinv)
}

coverage_data <- function(spec, replicate) {
  index <- match(spec$case, coverage_cases()$case)
  definition <- coverage_definition(spec)
  d <- withr::with_seed(20261015L + index * 10000L, {
    data.frame(x = (seq_len(spec$n) - .5) / spec$n,
      z = sample((seq_len(spec$n) - .5) / spec$n))
  })
  d$w <- if (spec$family == "normal") .6 + 1.2 * d$x else rep(1, spec$n)
  d$o <- .1 + .2 * d$x
  eta <- definition$intercept + definition$signal(d$x, d$z) + d$o
  mu <- definition$inverse(eta)
  trials <- if (spec$case == "binomial_rare") rep(1L, spec$n) else
    6L + seq_len(spec$n) %% 10L
  d$y <- withr::with_seed(10261015L + index * 10000L + replicate, {
    switch(spec$family, normal = eta + rnorm(spec$n, sd = .5 / sqrt(d$w)),
      poisson = rpois(spec$n, mu),
      binomial = {success <- rbinom(spec$n, trials, mu); cbind(success, trials - success)},
      gamma = rgamma(spec$n, shape = 4, scale = mu / 4),
      nb = rnbinom(spec$n, size = 2, mu = mu),
      beta = rbeta(spec$n, mu * 12, (1 - mu) * 12),
      student = eta + .4 * rt(spec$n, df = 5))
  })
  evaluation <- data.frame(x = seq(0, 1, length.out = 11L), z = .5)
  evaluation$o <- .1 + .2 * evaluation$x
  eta_true <- definition$intercept + definition$signal(evaluation$x, evaluation$z) + evaluation$o
  truth <- cbind(link = eta_true,
    smooth = definition$first(evaluation$x, evaluation$z) - mean(definition$first(d$x, d$z)),
    derivative = definition$first(evaluation$x, evaluation$z, derivative = TRUE),
    response_mean = definition$inverse(eta_true))
  list(data = d, evaluation = evaluation, truth = truth, definition = definition)
}

coverage_fit <- function(spec, fixture, engine, method) {
  family <- switch(spec$family, normal = stats::gaussian(), poisson = stats::poisson(),
    binomial = stats::binomial(), gamma = stats::Gamma("log"), nb = mgcv::nb(),
    beta = mgcv::betar(), student = mgcv::scat())
  family <- get("sampling_smoothing_family_bindings", asNamespace("gratia"))(family)
  formula <- switch(spec$structure,
    single = y ~ s(x, bs = "cr", k = 8) + offset(o),
    shared = y ~ s(x, bs = "cr", k = 8, id = "shared") +
      s(z, bs = "cr", k = 8, id = "shared") + offset(o),
    fixed = y ~ s(x, bs = "cr", k = 8, sp = .1) + s(z, bs = "cr", k = 8) + offset(o),
    tensor = y ~ te(x, z, bs = c("cr", "cr"), k = c(6, 6)) + offset(o))
  fitter <- get(engine, asNamespace("mgcv"))
  fitter(formula, data = fixture$data, weights = w, family = family, method = method,
    knots = list(x = fixture$definition$knots, z = fixture$definition$knots),
    scale = if (spec$known_scale) .25 else 0,
    control = mgcv::gam.control(epsilon = 1e-9, maxit = 200L))
}

coverage_projection <- function(model, fixture) {
  evaluation <- fixture$evaluation
  X <- stats::predict(model, newdata = evaluation, type = "lpmatrix")
  j <- model$smooth[[1L]]$first.para:model$smooth[[1L]]$last.para
  smooth <- matrix(0, nrow(X), ncol(X)); smooth[, j] <- X[, j, drop = FALSE]
  plus <- minus <- evaluation
  plus$x <- plus$x + 1e-5; minus$x <- minus$x - 1e-5
  D <- (stats::predict(model, newdata = plus, type = "lpmatrix") -
    stats::predict(model, newdata = minus, type = "lpmatrix")) / 2e-5
  D[, -j] <- 0
  list(A = rbind(X, smooth, D), offset = c(evaluation$o, rep(0, 2L * nrow(X))),
    points = nrow(X))
}

coverage_normal <- function(mean, covariance, projection) {
  if (any(!is.finite(covariance)) || max(abs(covariance - t(covariance))) > 1e-8)
    stop("Non-finite or asymmetric coefficient covariance.")
  values <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  if (min(values) < -1e-10 * max(1, max(values))) stop("Coefficient covariance is not positive semidefinite.")
  location <- drop(projection$A %*% mean) + projection$offset
  variance <- rowSums((projection$A %*% covariance) * projection$A)
  if (any(variance < -1e-10)) stop("Negative projected variance.")
  cbind(location + qnorm(.025) * sqrt(pmax(variance, 0)), location,
    location + qnorm(.975) * sqrt(pmax(variance, 0)))
}

coverage_mixture_quantiles <- function(means, variances, probabilities = c(.025, .5, .975)) {
  stopifnot(is.matrix(means), identical(dim(means), dim(variances)),
    all(is.finite(means)), all(is.finite(variances)), all(variances >= 0))
  out <- matrix(NA_real_, ncol(means), length(probabilities))
  for (j in seq_len(ncol(means))) {
    mu <- means[, j]; sd <- sqrt(variances[, j])
    for (k in seq_along(probabilities)) {
      probability <- probabilities[k]
      lo <- min(mu + qnorm(probability) * sd)
      hi <- max(mu + qnorm(probability) * sd)
      if (hi - lo < 1e-12) out[j, k] <- (lo + hi) / 2 else {
        cdf <- function(x) mean(ifelse(sd == 0, as.numeric(x >= mu),
          pnorm(x, mean = mu, sd = pmax(sd, .Machine$double.xmin)))) - probability
        out[j, k] <- stats::uniroot(cdf, c(lo - 1e-10, hi + 1e-10), tol = 1e-8)$root
      }
    }
  }
  out
}

coverage_mixture <- function(setup, projection, components, seed) {
  stopifnot(components %% 2L == 0L)
  rho <- withr::with_seed(seed, {
    z <- matrix(rnorm(components / 2L * length(setup$log_sp)), nrow = components / 2L)
    sweep(rbind(z, -z) %*% chol(setup$log_sp_cov), 2L, setup$log_sp, `+`)
  })
  means <- variances <- matrix(NA_real_, nrow(rho), nrow(projection$A))
  conditional <- get("sampling_smoothing_conditional", asNamespace("gratia"))
  for (i in seq_len(nrow(rho))) {
    fit <- tryCatch(conditional(setup, rho[i, ]), error = function(e)
      stop(sprintf("Smoothing component %d failed: %s", i, conditionMessage(e)), call. = FALSE))
    means[i, ] <- drop(projection$A %*% fit$mean) + projection$offset
    variances[i, ] <- rowSums((projection$A %*% fit$covariance) * projection$A)
  }
  if (any(variances < -1e-10)) stop("Negative conditional projected variance.")
  coverage_mixture_quantiles(means, pmax(variances, 0))
}

coverage_intervals <- function(quantiles, fixture) {
  n <- nrow(fixture$evaluation)
  result <- array(NA_real_, c(n, 3L, 4L), dimnames = list(NULL,
    c("lower", "median", "upper"), colnames(fixture$truth)))
  for (j in 1:3) result[, , j] <- quantiles[(j - 1L) * n + seq_len(n), , drop = FALSE]
  result[, , 4L] <- fixture$definition$inverse(result[, , 1L])
  result
}

coverage_replicate <- function(spec, replicate, components = 256L) {
  fixture <- coverage_data(spec, replicate)
  configurations <- coverage_configurations()
  records <- vector("list", nrow(configurations))
  for (configuration in seq_len(nrow(configurations))) {
    cfg <- configurations[configuration, ]
    warnings <- list(fit = character(), setup = character(), mixture = character(), unconditional = character())
    stage <- "fit"
    capture <- function(expr, which) {
      stage <<- which
      withCallingHandlers(tryCatch(expr, error = function(e) structure(conditionMessage(e), class = "coverage_error")),
        warning = function(w) {warnings[[which]] <<- c(warnings[[which]], conditionMessage(w)); invokeRestart("muffleWarning")})
    }
    record <- list(case = spec$case, replicate = replicate, engine = cfg$engine,
      method = cfg$method, components = components, intervals = list(), status = list(),
      fit_warnings = "", setup_warnings = "", mixture_warnings = "", unconditional_warnings = "")
    fit <- capture(withr::with_seed(30261015L + match(spec$case, coverage_cases()$case) * 10000L +
      replicate * 10L + configuration, coverage_fit(spec, fixture, cfg$engine, cfg$method)), "fit")
    if (inherits(fit, "coverage_error")) record$status$fit <- as.character(fit) else {
      record$actual_method <- fit$method
      record$log_sp <- log(fit$sp)
      record$dispersion <- fit$sig2
      record$family_parameters <- if (is.function(fit$family$getTheta)) fit$family$getTheta() else NULL
      failed <- !isTRUE(fit$converged) ||
        any(grepl("step fail|step failure|not converge|iteration limit", c(fit$outer.info$conv,
          warnings$fit), ignore.case = TRUE))
      if (failed) record$status$fit <- paste("Native fit did not converge:", paste(c(fit$outer.info$conv, warnings$fit), collapse = "; ")) else {
        record$status$fit <- "OK"
        projection <- coverage_projection(fit, fixture)
        start <- proc.time()[["elapsed"]]
        q <- capture(coverage_normal(stats::coef(fit), stats::vcov(fit, unconditional = FALSE), projection), "setup")
        record$status$conditional <- if (inherits(q, "coverage_error")) as.character(q) else "OK"
        if (!inherits(q, "coverage_error")) record$intervals$conditional <- coverage_intervals(q, fixture)
        q <- capture({if (is.null(fit$Vc)) stop("No unconditional covariance retained; no fallback used.")
          coverage_normal(stats::coef(fit), stats::vcov(fit, unconditional = TRUE), projection)}, "unconditional")
        record$status$unconditional <- if (inherits(q, "coverage_error")) as.character(q) else "OK"
        if (!inherits(q, "coverage_error")) record$intervals$unconditional <- coverage_intervals(q, fixture)
        setup <- capture(get("sampling_smoothing_setup", asNamespace("gratia"))(fit), "setup")
        record$status$setup <- if (inherits(setup, "coverage_error")) as.character(setup) else "OK"
        if (!inherits(setup, "coverage_error")) {
          record$log_sp_sd <- sqrt(diag(setup$log_sp_cov))
          q <- capture(coverage_mixture(setup, projection, components,
            seed = 40261015L + match(spec$case, coverage_cases()$case) * 10000L + replicate), "mixture")
          record$status$mixture <- if (inherits(q, "coverage_error")) as.character(q) else "OK"
          if (!inherits(q, "coverage_error")) record$intervals$mixture <- coverage_intervals(q, fixture)
        } else record$status$mixture <- paste("Setup failed:", record$status$setup)
        record$interval_seconds <- proc.time()[["elapsed"]] - start
      }
    }
    for (which in names(warnings)) record[[paste0(which, "_warnings")]] <- paste(unique(warnings[[which]]), collapse = "; ")
    records[[configuration]] <- record
  }
  list(case = spec$case, replicate = replicate, truth = fixture$truth,
    evaluation = fixture$evaluation, records = records)
}
