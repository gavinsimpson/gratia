args <- commandArgs(TRUE)
repo <- if (length(args)) args[1L] else getwd()
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
library("scam")
pkgload::load_all(repo, quiet = TRUE)
capture_result <- function(expr) {
  w <- character()
  val <- tryCatch(withCallingHandlers(expr, warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")
  }), error = identity)
  list(value = val, error = if (inherits(val, "error")) conditionMessage(val) else "",
    warning = paste(w, collapse = " | "))
}
inv <- readRDS(file.path(out, "inventory.rds"))
api <- readRDS(file.path(out, "api-details.rds"))
detail <- readRDS(file.path(out, "detail-results.rds"))
d <- inv$data
s <- mgcv::s
ti <- mgcv::ti
grDevices::pdf(file.path(out, "supplementary-plots.pdf"))
variants <- list()
for (bs in c("tesmi1", "tesmi2", "tesmd1", "tesmd2", "tescx", "tescv", "tismi", "tismd")) {
  cat("Cyclic margin", bs, "\n")
  f <- as.formula(sprintf("y ~ s(x, z, bs = '%s', k = c(5, 5), xt = 'cc')", bs))
  fm <- capture_result(scam::scam(f, data = d))
  r <- list(case = paste0(bs, "_cc"), fit_error = fm$error)
  if (!nzchar(fm$error)) {
    m <- fm$value
    r$predict_error <- capture_result(predict(m, d[1:5, ], se.fit = TRUE, type = "terms"))$error
    r$plot_error <- capture_result(scam::plot.scam(m, n2 = 20))$error
    r$gratia_error <- capture_result(smooth_estimates(m, data = d[1:5, ]))$error
  }
  variants[[length(variants) + 1L]] <- r
}
for (bs in c("tismi", "tismd")) {
  cat("Interaction with main effects", bs, "\n")
  f <- as.formula(sprintf("y ~ ti(x, k = 5) + ti(z, k = 5) + s(x, z, bs = '%s', k = c(5, 5))", bs))
  fm <- capture_result(scam::scam(f, data = d))
  r <- list(case = paste0(bs, "_main_effects"), fit_error = fm$error)
  if (!nzchar(fm$error)) {
    m <- fm$value
    r$predict_error <- capture_result(predict(m, d[1:5, ], se.fit = TRUE, type = "terms"))$error
    r$plot_error <- capture_result(scam::plot.scam(m, n2 = 20))$error
    r$gratia_error <- capture_result(smooth_estimates(m, data = d[1:5, ]))$error
  }
  variants[[length(variants) + 1L]] <- r
}
set.seed(20)
X <- matrix(runif(600), 120, 5)
L <- matrix(runif(600), 120, 5)
dl <- list(X = X, L = L, y = rowSums(L * exp(X)) + rnorm(120, sd = 0.1))
fm <- capture_result(scam::scam(y ~ s(X, by = L, bs = "mpiBy", k = 6), data = dl))
r <- list(case = "linear_functional_mpiBy", fit_error = fm$error)
if (!nzchar(fm$error)) {
  m <- fm$value
  nd <- data.frame(y = dl$y[1:5]); nd$X <- X[1:5, ]; nd$L <- L[1:5, ]
  r$predict_error <- capture_result(predict(m, nd, se.fit = TRUE, type = "terms"))$error
  r$plot_error <- capture_result(scam::plot.scam(m))$error
  r$gratia_error <- capture_result(smooth_estimates(m, data = nd))$error
}
variants[[length(variants) + 1L]] <- r
cols <- unique(unlist(lapply(variants, names)))
vs <- do.call(rbind, lapply(variants, function(x) {
  x[setdiff(cols, names(x))] <- NA; as.data.frame(x[cols])
}))
write.csv(vs, file.path(out, "smooth-variants.csv"), row.names = FALSE)

# A numerical derivative of the actual plot verifies which covariance it uses.
jacobians <- list()
for (bs in c("mpiBy", "ipo", "dpo", "tedmi", "tesmi1", "temicx", "tismi")) {
  cat("Plot covariance", bs, "\n")
  m <- inv$models[[bs]]
  sm <- m$smooth[[1L]]
  idx <- sm$first.para:sm$last.para
  base <- m
  bt <- m$coefficients.t
  bt[idx] <- 1
  base$coefficients.t <- bt
  base$coefficients <- bt
  base$coefficients[base$p.ident] <- log(bt[base$p.ident])
  P <- scam::plot.scam(base, n = 100, n2 = 12, se = FALSE, rug = FALSE, too.far = 0)[[1L]]
  J <- matrix(0, length(P$fit), length(idx))
  for (j in seq_along(idx)) {
    perturbed <- base
    perturbed$coefficients[idx[j]] <- if (base$p.ident[idx[j]]) log(2) else 2
    Q <- scam::plot.scam(perturbed, n = 100, n2 = 12, se = FALSE, rug = FALSE, too.far = 0)[[1L]]
    J[, j] <- as.numeric(Q$fit - P$fit)
  }
  native <- scam::plot.scam(m, n = 100, n2 = 12, se = 1, rug = FALSE, too.far = 0)[[1L]]
  se <- sqrt(pmax(0, rowSums((J %*% m$Vp.t[idx, idx, drop = FALSE]) * J)))
  jacobians[[bs]] <- data.frame(basis = bs,
    max_native_se_vs_plot_gradient = max(abs(se - native$se), na.rm = TRUE))
}
write.csv(do.call(rbind, jacobians), file.path(out, "plot-covariance-gradient.csv"), row.names = FALSE)
grDevices::dev.off()

# Render the actual native and gratia methods on a single comparison sheet.
ragg::agg_png(file.path(out, "plot-comparison.png"), width = 1600, height = 1600, res = 160)
bs_list <- c("mpi", "mifo", "tedmi")
for (i in seq_along(bs_list)) {
  bs <- bs_list[i]
  par(fig = c(0.01, 0.49, 1 - i / 3 + 0.01, 1 - (i - 1) / 3 - 0.035),
    new = i > 1L, mar = c(4, 4, 2, 1))
  scam::plot.scam(inv$models[[bs]], n = 100, n2 = 20, se = qnorm(0.975),
    shade = TRUE, rug = FALSE, too.far = 0, main = bs)
}
grid::grid.text("scam::plot.scam()", x = 0.25, y = 0.985,
  gp = grid::gpar(fontsize = 16, fontface = "bold"))
grid::grid.text("gratia::draw()", x = 0.75, y = 0.985,
  gp = grid::gpar(fontsize = 16, fontface = "bold"))
for (i in seq_along(bs_list)) {
  bs <- bs_list[i]
  pp <- capture_result(draw(inv$models[[bs]], parametric = FALSE,
    n = 100, n_2d = 20, rug = FALSE, dist = 0, wrap = FALSE))
  vp <- grid::viewport(x = 0.75, y = 1 - (i - 0.5) / 3 - 0.015,
    width = 0.48, height = 0.29)
  if (!nzchar(pp$error) && length(pp$value)) {
    print(pp$value[[1L]] + ggplot2::labs(title = bs), newpage = FALSE, vp = vp)
  } else {
    grid::pushViewport(vp)
    grid::grid.rect(gp = grid::gpar(fill = "#f7f7f7", col = "#dddddd"))
    grid::grid.text(bs, x = 0.08, y = 0.88, just = "left",
      gp = grid::gpar(fontsize = 14, fontface = "bold"))
    grid::grid.text(if (nzchar(pp$error)) pp$error else "No plot returned",
      gp = grid::gpar(fontsize = 13, col = "#9b2929"))
    grid::popViewport()
  }
}
grDevices::dev.off()

# Confirm working family adapters and inherited helper semantics.
family_checks <- lapply(names(detail$extra), function(nm) {
  m <- detail$extra[[nm]]
  nd <- d[1:10, ]
  list(case = nm,
    fitted = capture_result(fitted_values(m, data = nd))$error,
    fitted_samples = capture_result(fitted_samples(m, data = nd, n_draws = 5, seed = 7))$error,
    posterior_samples = capture_result(posterior_samples(m, data = nd, n_draws = 5, seed = 7))$error,
    appraise = capture_result(appraise(m, n_simulate = 5, seed = 7))$error)
})
helper_checks <- list(
  fixed_effects_includes_smooth = length(fixed_effects(api$models$multi)) > api$models$multi$nsdf,
  model_terms_returns_coefficient_names = identical(model_terms(api$models$multi), names(coef(api$models$multi))),
  no_intercept_model_constant = model_constant(detail$extra$without_intercept),
  no_intercept_names = names(coef(detail$extra$without_intercept)),
  generate_draws_error = capture_result(generate_draws(api$models$single, n_draws = 5))$error,
  waic_error = capture_result(loo::waic(api$models$single, n_draws = 5))$error)
saveRDS(list(families = family_checks, helpers = helper_checks), file.path(out, "supplementary.rds"))
print(vs)
print(do.call(rbind, jacobians))
print(helper_checks)
