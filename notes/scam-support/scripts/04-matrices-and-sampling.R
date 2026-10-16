args <- commandArgs(TRUE)
repo <- if (length(args)) args[1L] else getwd()
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
library("scam")
pkgload::load_all(repo, quiet = TRUE)
capture_result <- function(expr) {
  val <- tryCatch(expr, error = identity)
  list(value = val, error = if (inherits(val, "error")) conditionMessage(val) else "")
}
maxdiff <- function(a, b) {
  if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA_real_)
  max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE)
}
inv <- readRDS(file.path(out, "inventory.rds"))
api <- readRDS(file.path(out, "api-details.rds"))
d <- inv$data
rows <- samples <- list()
for (bs in names(inv$models)) {
  cat("Matrices", bs, "\n"); flush.console()
  m <- inv$models[[bs]]
  sm <- m$smooth[[1L]]
  P <- inv$plots[[bs]]
  if (is.null(P)) next
  nd <- if (sm$dim == 2L) expand.grid(x = P$x, z = P$y) else data.frame(x = P$x)
  if (sm$by != "NA") nd[[sm$by]] <- 1
  idx <- sm$first.para:sm$last.para
  X <- predict(m, nd, type = "lpmatrix")
  G <- X[, idx, drop = FALSE]
  fit <- drop(G %*% coef(m)[idx])
  se <- sqrt(pmax(0, rowSums((G %*% m$Vp.t[idx, idx, drop = FALSE]) * G)))
  pt <- capture_result(predict(m, nd, type = "terms", se.fit = TRUE))
  rows[[bs]] <- data.frame(basis = bs, class = class(sm)[1L],
    matrix_columns = ncol(X), coefficients = length(coef(m)),
    fit_vs_plot = maxdiff(fit, P$fit),
    centred_fit_vs_plot = maxdiff(fit - mean(fit), P$fit),
    fit_minus_plot_mean = mean(fit - as.numeric(P$fit)),
    fit_minus_plot_range = diff(range(fit - as.numeric(P$fit))),
    direct_se_vs_plot = maxdiff(se, P$se),
    direct_se_vs_predict = if (!nzchar(pt$error)) maxdiff(se, pt$value$se.fit) else NA_real_,
    predict_se_error = pt$error)
  nd <- d[1:12, ]
  X <- predict(m, nd, type = "lpmatrix")
  backends <- lapply(c("mvnfast", "mgcv"), function(backend) {
    capture_result(gaussian_draws(m, n_draws = 5, seed = 7, mvn_method = backend))
  })
  for (j in seq_along(backends)) {
    samples[[length(samples) + 1L]] <- data.frame(basis = bs,
      operation = paste0("gaussian_draws_", c("mvnfast", "mgcv")[j]),
      error = backends[[j]]$error, max_difference = NA_real_)
  }
  j <- if (!nzchar(backends[[2L]]$error)) 2L else 1L
  backend <- c("mvnfast", "mgcv")[j]
  B <- backends[[j]]
  if (nzchar(B$error)) next
  B <- B$value
  for (op in c("smooth_samples", "fitted_samples", "derivative_samples")) {
    val <- capture_result(switch(op,
      smooth_samples = smooth_samples(m, data = nd, n_draws = 5, seed = 7, mvn_method = backend),
      fitted_samples = fitted_samples(m, data = nd, n_draws = 5, seed = 7, mvn_method = backend, scale = "linear_predictor"),
      derivative_samples = derivative_samples(m, data = nd, focal = "x", n_draws = 5, seed = 7,
        mvn_method = backend, scale = "linear_predictor", type = "central", eps = 1e-5)))
    delta <- NA_real_
    if (!nzchar(val$error)) {
      v <- val$value
      expected <- switch(op,
        smooth_samples = X[, idx, drop = FALSE] %*% t(B[, idx, drop = FALSE]),
        fitted_samples = X[, seq_len(ncol(B)), drop = FALSE] %*% t(B),
        derivative_samples = {
          hi <- lo <- nd; hi$x <- hi$x + 1e-5; lo$x <- lo$x - 1e-5
          (predict(m, hi, type = "lpmatrix")[, seq_len(ncol(B)), drop = FALSE] -
            predict(m, lo, type = "lpmatrix")[, seq_len(ncol(B)), drop = FALSE]) %*% t(B) / 2e-5
        })
      vv <- v[[switch(op, smooth_samples = ".value", fitted_samples = ".fitted",
        derivative_samples = ".derivative")]]
      delta <- maxdiff(vv, expected[cbind(v$.row, v$.draw)])
    }
    samples[[length(samples) + 1L]] <- data.frame(basis = bs, operation = op,
      error = val$error, max_difference = delta)
  }
}
write.csv(do.call(rbind, rows), file.path(out, "matrix-comparison.csv"), row.names = FALSE)
write.csv(do.call(rbind, samples), file.path(out, "gaussian-sampling-comparison.csv"), row.names = FALSE)

set.seed(47)
weak <- data.frame(x = seq(0, 1, length.out = 60))
weak$y <- exp(weak$x) + rnorm(nrow(weak), sd = 0.7)
s <- mgcv::s
mw <- scam::scam(y ~ s(x, bs = "mpi", k = 10), data = weak, sp = 0.01)
B <- gaussian_draws(mw, n_draws = 2000, seed = 47, mvn_method = "mgcv")
F <- predict(mw, weak, type = "lpmatrix") %*% t(B)
violation <- apply(F, 2L, function(f) any(diff(f) < -1e-8))
shape <- list(proportion_draws_violating_increase = mean(violation),
  most_negative_increment = min(apply(F, 2L, diff)),
  coefficient_negative = mean(apply(B[, mw$p.ident, drop = FALSE], 1, function(b) any(b < 0))))
saveRDS(list(model = mw, shape = shape), file.path(out, "shape-sampling.rds"))
print(shape)
