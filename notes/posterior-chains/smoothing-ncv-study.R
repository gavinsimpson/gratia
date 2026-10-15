# Exploratory repeated-sampling check of Vg. This is not posterior validation.
# Run from the repository root; optional argument is the number of repetitions.
library("mgcv")
args <- commandArgs(trailingOnly = TRUE)
n_rep <- if (length(args)) as.integer(args[1]) else 200L
stopifnot(n_rep >= 100L)
set.seed(918)
d <- data.frame(x = runif(240), z = runif(240), w = runif(240, .5, 2))
mean_y <- sin(6 * d$x) + cos(7 * d$z)
# Recompute the Gaussian criterion Hessian independently. This is used only
# inside a sandwich, never as a posterior covariance.
checked_sandwich <- function(b) {
  X <- predict(b, type = "lpmatrix")
  w <- b$prior.weights
  XtWX <- crossprod(X, w * X)
  XtWy <- crossprod(X, w * b$y)
  penalties <- lapply(b$smooth, function(sm) {
    S <- matrix(0, ncol(X), ncol(X))
    j <- sm$first.para:sm$last.para
    S[j, j] <- sm$S[[1L]]
    S
  })
  contributions <- function(rho) {
    P <- XtWX
    for (k in seq_along(rho)) P <- P + exp(rho[k]) * penalties[[k]]
    inverse <- solve(P)
    beta <- inverse %*% XtWy
    XI <- X %*% inverse
    h <- w * rowSums(XI * X)
    r <- drop(b$y - X %*% beta)
    vapply(seq_along(rho), function(k) {
      S <- exp(rho[k]) * penalties[[k]]
      J <- -inverse %*% S %*% beta
      dh <- -w * rowSums((XI %*% S) * XI)
      dr <- -drop(X %*% J) / (1 - h) + r * dh / (1 - h)^2
      2 * w * r / (1 - h) * dr
    }, numeric(nrow(X)))
  }
  rho <- log(b$sp)
  gradients <- contributions(rho)
  eps <- 1e-4
  H <- vapply(seq_along(rho), function(k) {
    step <- rep(0, length(rho)); step[k] <- eps
    colSums(contributions(rho + step) - contributions(rho - step)) / (2 * eps)
  }, numeric(length(rho)))
  B <- solve((H + t(H)) / 2)
  V <- B %*% crossprod(gradients) %*% B
  native_B <- solve(b$outer.info$hess)
  list(V = V, curvature_error = norm(H - b$outer.info$hess, "F") / norm(H, "F"),
    native_sandwich_error = max(abs(native_B %*% crossprod(gradients) %*%
      native_B - b$Vg)))
}
results <- lapply(seq_len(n_rep), function(i) {
  d$y <- mean_y + rnorm(nrow(d), sd = .4 / sqrt(d$w))
  warnings <- character()
  b <- withCallingHandlers(tryCatch(gam(y ~ s(x, k = 6) + s(z, k = 6),
    data = d, weights = w, method = "NCV", nei = list(jackknife = FALSE)),
    error = identity), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (inherits(b, "error")) return(data.frame(repetition = i,
    error = conditionMessage(b), warnings = paste(warnings, collapse = "; ")))
  independent <- checked_sandwich(b)
  data.frame(repetition = i, error = "", warnings = paste(warnings, collapse = "; "),
    convergence = b$outer.info$conv,
    rho1 = log(b$sp[1]), rho2 = log(b$sp[2]),
    Vg11 = b$Vg[1, 1], Vg12 = b$Vg[1, 2], Vg22 = b$Vg[2, 2],
    checked11 = independent$V[1, 1], checked22 = independent$V[2, 2],
    relative_curvature_error = independent$curvature_error,
    native_sandwich_error = independent$native_sandwich_error)
})
fields <- unique(unlist(lapply(results, names)))
results <- do.call(rbind, lapply(results, function(x) {
  x[setdiff(fields, names(x))] <- NA
  x[fields]
}))
write.csv(results, "notes/posterior-chains/smoothing-ncv-repetitions.csv", row.names = FALSE)
good <- results$error == ""
stopifnot(all(good), all(is.finite(as.matrix(results[c("rho1", "rho2", "Vg11", "Vg22")]))))
summary <- data.frame(parameter = c("log sp 1", "log sp 2"), repetitions = n_rep,
  empirical_variance = c(var(results$rho1), var(results$rho2)),
  mean_estimated_variance = c(mean(results$Vg11), mean(results$Vg22)),
  median_estimated_variance = c(median(results$Vg11), median(results$Vg22)),
  mean_checked_hessian_sandwich_variance = c(mean(results$checked11), mean(results$checked22)),
  minimum_log_sp = c(min(results$rho1), min(results$rho2)),
  maximum_log_sp = c(max(results$rho1), max(results$rho2)))
write.csv(summary, "notes/posterior-chains/smoothing-ncv-repeated-summary.csv", row.names = FALSE)
writeLines(trimws(c(capture.output(summary), "",
  capture.output(table(results$convergence)), "", capture.output(sessionInfo())),
  which = "right"), "notes/posterior-chains/smoothing-ncv-repeated-validation.txt")
