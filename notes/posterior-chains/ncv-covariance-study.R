# Run at the repository root, adding --load-all for an uninstalled checkout.
# Optional numeric argument: repetitions per design (at least 100; default 200).
args <- commandArgs(trailingOnly = TRUE)
if ("--load-all" %in% args) pkgload::load_all(quiet = TRUE)
library("gratia")
library("mgcv")
ns <- asNamespace("gratia")
setup_ncv <- get("sampling_ncv_gaussian_setup", ns)
eval_ncv <- get("sampling_ncv_gaussian_eval", ns)
moments_ncv <- get("sampling_ncv_gaussian_moments", ns)
cov_ncv <- get("sampling_ncv_gaussian_covariance", ns)
n_arg <- args[grepl("^[0-9]+$", args)]
n_rep <- if (length(n_arg)) as.integer(n_arg[1]) else 200L
stopifnot(n_rep >= 100L)
results <- decompositions <- simulations <- scales <- list()
for (design in c("weighted", "unweighted")) {
  set.seed(918)
  d <- data.frame(x = runif(240), z = runif(240), w = runif(240, .5, 2))
  if (design == "unweighted") d$w <- 1
  mu <- sin(6 * d$x) + cos(7 * d$z)
  rows <- vector("list", n_rep)
  reference <- NULL
  for (i in seq_len(n_rep)) {
    d$y <- mu + rnorm(nrow(d), sd = .4 / sqrt(d$w))
    b <- gam(y ~ s(x, k = 6) + s(z, k = 6), data = d, weights = w,
      method = "NCV", nei = list(jackknife = FALSE))
    setup <- setup_ncv(b)
    if (is.null(reference)) reference <- setup
    oracle <- cov_ncv(setup, mu, .16)
    Hi <- solve(oracle$hessian)
    op <- eval_ncv(setup)
    plugin <- moments_ncv(op, sqrt(d$w) * setup$fitted, setup$phi)
    # An unpenalised basis fit is a second mean estimate. This exposes the
    # sensitivity to estimating the mean rather than treating truth as known.
    ols_mean <- drop(setup$Z %*% solve(crossprod(setup$Z),
      crossprod(setup$Z, sqrt(d$w) * d$y))) / sqrt(d$w)
    pilot <- moments_ncv(op, sqrt(d$w) * ols_mean, setup$phi)
    sandwich <- function(V) Hi %*% V %*% Hi
    vc <- list(native = setup$native$log_sp_covariance,
      native_exact_hessian = sandwich(crossprod(op$contributions)),
      gaussian_oracle = oracle$log_sp_covariance,
      gaussian_fitted_mean = sandwich(plugin$gradient_covariance),
      gaussian_ols_mean = sandwich(pilot$gradient_covariance))
    row <- data.frame(design = design, repetition = i,
      convergence = b$outer.info$conv, rho1 = log(b$sp[1]), rho2 = log(b$sp[2]),
      dispersion = b$sig2,
      native_construction_error = max(abs(solve(b$outer.info$hess) %*%
        crossprod(op$contributions) %*% solve(b$outer.info$hess) - b$Vg)))
    for (nm in names(vc)) {
      row[[paste0(nm, "_11")]] <- vc[[nm]][1, 1]
      row[[paste0(nm, "_12")]] <- vc[[nm]][1, 2]
      row[[paste0(nm, "_22")]] <- vc[[nm]][2, 2]
    }
    rows[[i]] <- row
  }
  rows <- do.call(rbind, rows)
  results[[design]] <- rows
  stopifnot(all(rows$convergence == "full convergence"),
    max(rows$native_construction_error) < 1e-7)
  # Evaluate all covariance terms at the SAME population optimum and Hessian.
  # Mixing expected and observed curvature would obscure the decomposition.
  muz <- sqrt(d$w) * mu
  expectation <- function(rho) moments_ncv(eval_ncv(reference, rho), muz, .16)
  opt <- optim(colMeans(rows[c("rho1", "rho2")]),
    function(rho) expectation(rho)$mean_criterion,
    function(rho) expectation(rho)$mean_gradient,
    method = "BFGS", control = list(reltol = 1e-12))
  stopifnot(opt$convergence == 0L)
  population <- cov_ncv(reference, mu, .16, curvature = "expected", log_sp = opt$par)
  Hi <- solve(population$hessian)
  m <- population$gradient_moments
  sandwich <- function(V) Hi %*% V %*% Hi
  terms <- list(expected_native_crossprod = m$expected_crossprod,
    squared_means = m$mean_outer, within_observation = m$within_observation_covariance,
    between_observation = m$between_observation_covariance,
    total_gradient = m$gradient_covariance,
    quadratic_noise = m$noise_covariance, linear_signal = m$signal_covariance)
  decompositions[[design]] <- do.call(rbind, lapply(names(terms), function(nm) {
    V <- sandwich(terms[[nm]])
    data.frame(design = design, term = nm, covariance11 = V[1, 1],
      covariance12 = V[1, 2], covariance22 = V[2, 2])
  }))
  # Independent responses, fixed smoothing parameters: no smoothing refits.
  # Compare exact Gaussian moments with covariance of the TOTAL gradient,
  # and separately compare E[crossprod(individual gradients)].
  op <- eval_ncv(reference, opt$par)
  check <- withr::with_seed(401, {
    gradients <- matrix(NA_real_, 20000L, 2L)
    crossprod_mean <- matrix(0, 2L, 2L)
    for (start in seq.int(1L, 20000L, by = 1000L)) {
      z <- matrix(rnorm(nrow(d) * 1000L, sd = .4), nrow(d)) + muz
      v <- op$B %*% z
      pieces <- lapply(op$B1, function(Bj) 2 * v * (Bj %*% z))
      gradients[start:(start + 999L), ] <- vapply(pieces, colSums, numeric(1000L))
      for (j in 1:2) for (k in 1:2) crossprod_mean[j, k] <- crossprod_mean[j, k] +
        sum(pieces[[j]] * pieces[[k]]) / 20000
    }
    V <- cov(gradients)
    D <- diag(1 / sqrt(diag(m$gradient_covariance)))
    error <- max(abs(D %*% (V - m$gradient_covariance) %*% D))
    D <- diag(1 / sqrt(diag(m$expected_crossprod)))
    crossprod_error <- max(abs(D %*% (crossprod_mean - m$expected_crossprod) %*% D))
    stopifnot(error < .04, crossprod_error < .04)
    list(V = sandwich(V), crossprod = sandwich(crossprod_mean),
      error = error, crossprod_error = crossprod_error)
  })
  simulations[[design]] <- data.frame(design = design, draws = 20000L,
    total_gradient_covariance11 = check$V[1, 1],
    total_gradient_covariance22 = check$V[2, 2],
    mean_native_crossprod11 = check$crossprod[1, 1],
    mean_native_crossprod22 = check$crossprod[2, 2],
    standardised_covariance_error = check$error,
    standardised_crossprod_error = check$crossprod_error)
  # Normalising the criterion rescales BOTH Hessian and gradient covariance.
  n <- nrow(d)
  scaled <- solve(population$hessian / n) %*% (m$gradient_covariance / n^2) %*%
    solve(population$hessian / n)
  scaled_setup <- reference
  scaled_setup$response <- 3 * reference$response
  scaled_setup$fitted <- 3 * reference$fitted
  scaled_setup$phi <- 9 * reference$phi
  scaled_setup$native$derivative <- 3 * reference$native$derivative
  units <- cov_ncv(scaled_setup, 3 * mu, 9 * .16,
    curvature = "expected", log_sp = opt$par)
  scales[[design]] <- data.frame(design = design,
    summed_vs_averaged_covariance_error =
      max(abs(scaled - population$log_sp_covariance)),
    response_units_covariance_error =
      max(abs(units$log_sp_covariance - population$log_sp_covariance)),
    log10_covariance_conversion_error =
      max(abs(cov(as.matrix(rows[c("rho1", "rho2")]) / log(10)) * log(10)^2 -
        cov(as.matrix(rows[c("rho1", "rho2")])))))
  stopifnot(max(unlist(scales[[design]][-1])) < 1e-7)
  cat(design, ": ", n_rep, " refits and 20000 fixed-parameter responses passed\n", sep = "")
}
results <- do.call(rbind, results)
write.csv(results, "notes/posterior-chains/ncv-covariance-repetitions.csv", row.names = FALSE)
summary <- do.call(rbind, lapply(split(results, results$design), function(x) {
  methods <- c("native", "native_exact_hessian", "gaussian_oracle",
    "gaussian_fitted_mean", "gaussian_ols_mean")
  empirical <- c(var(x$rho1), var(x$rho2))
  do.call(rbind, lapply(methods, function(method) {
    estimated <- c(mean(x[[paste0(method, "_11")]]), mean(x[[paste0(method, "_22")]]))
    data.frame(design = x$design[1], method = method, parameter = 1:2,
      repetitions = nrow(x), empirical_variance = empirical,
      mean_estimated_variance = estimated, variance_ratio = estimated / empirical)
  }))
}))
write.csv(summary, "notes/posterior-chains/ncv-covariance-summary.csv", row.names = FALSE)
write.csv(do.call(rbind, decompositions),
  "notes/posterior-chains/ncv-covariance-decomposition.csv", row.names = FALSE)
write.csv(do.call(rbind, simulations),
  "notes/posterior-chains/ncv-covariance-simulation.csv", row.names = FALSE)
write.csv(do.call(rbind, scales), "notes/posterior-chains/ncv-covariance-scaling.csv", row.names = FALSE)
old <- read.csv("notes/posterior-chains/smoothing-ncv-repetitions.csv")
x <- results[results$design == "weighted", ]
comparison_rows <- seq_len(min(n_rep, nrow(old)))
stopifnot(isTRUE(all.equal(as.matrix(x[comparison_rows, c("rho1", "rho2")]),
  as.matrix(old[comparison_rows, c("rho1", "rho2")]), check.attributes = FALSE)))
writeLines(trimws(c(capture.output(summary), "", capture.output(do.call(rbind, decompositions)),
  "", capture.output(do.call(rbind, scales)), "", capture.output(sessionInfo())),
  which = "right"), "notes/posterior-chains/ncv-covariance-validation.txt")
