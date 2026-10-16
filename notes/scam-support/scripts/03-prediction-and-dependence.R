args <- commandArgs(TRUE)
repo <- if (length(args)) args[1L] else getwd()
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
library("scam")
pkgload::load_all(repo, quiet = TRUE)
capture_result <- function(expr) {
  w <- msg <- character()
  val <- tryCatch(withCallingHandlers(expr, warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")
  }, message = function(e) {
    msg <<- c(msg, conditionMessage(e)); invokeRestart("muffleMessage")
  }), error = identity)
  list(value = val, error = if (inherits(val, "error")) conditionMessage(val) else "",
    warnings = paste(unique(w), collapse = " | "), messages = paste(unique(msg), collapse = " | "))
}
maxdiff <- function(a, b) {
  if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA_real_)
  v <- abs(as.numeric(a) - as.numeric(b))
  if (!any(is.finite(v))) return(NA_real_)
  max(v, na.rm = TRUE)
}
inv <- readRDS(file.path(out, "inventory.rds"))
api <- readRDS(file.path(out, "api-details.rds"))
d <- inv$data
s <- mgcv::s
set.seed(2026)
extra <- list(
  numeric_by = scam::scam(y ~ s(x, bs = "mpi", by = w, k = 8), data = d),
  without_intercept = scam::scam(y ~ 0 + s(x, bs = "mpi", k = 8), data = d),
  not_exp = scam::scam(y ~ s(x, bs = "mpi", k = 8), data = d, not.exp = TRUE,
    control = scam::scam.control(b.notexp = 2, threshold.notexp = 15)),
  mixed = scam::scam(y ~ s(x, bs = "mpi", k = 8) + s(z, bs = "cr", k = 6), data = d),
  transformed = scam::scam(y ~ s(log(x), bs = "mpi", k = 8), data = d),
  poisson = scam::scam(rpois(nrow(d), exp(x + z)) ~ s(x, bs = "mpi", k = 8),
    data = d, family = poisson()),
  binomial = scam::scam(rbinom(nrow(d), 1, plogis(x)) ~ s(x, bs = "mpi", k = 8),
    data = d, family = binomial()),
  gamma = scam::scam(rgamma(nrow(d), shape = 6, scale = exp(x) / 6) ~ s(x, bs = "mpi", k = 8),
    data = d, family = Gamma(link = "log")))
models <- c(api$models, extra)
rows <- list()
grDevices::pdf(file.path(out, "detail-plots.pdf"))
for (mn in names(models)) {
  cat("Details", mn, "\n"); flush.console()
  m <- models[[mn]]
  p <- capture_result(scam::plot.scam(m, n = 100, n2 = 20, se = 1, rug = FALSE, too.far = 0))
  for (i in seq_along(m$smooth)) {
    sm <- m$smooth[[i]]
    for (n in c(1L, 2L, 100L)) {
      nd <- d[seq_len(n), ]
      se <- capture_result(smooth_estimates(m, data = nd, select = sm$label, overall_uncertainty = FALSE))
      pr <- capture_result(predict(m, nd, type = "terms", se.fit = TRUE))
      r <- data.frame(model = mn, term = sm$label, n = n,
        error = se$error, predict_error = pr$error,
        fit_difference = NA_real_, se_difference = NA_real_)
      if (!nzchar(se$error) && !nzchar(pr$error) && nrow(se$value)) {
        sel <- if (!is.null(sm$by.level)) which(nd[[sm$by]] == sm$by.level) else seq_len(n)
        r$fit_difference <- maxdiff(se$value$.estimate, pr$value$fit[sel, sm$label])
        r$se_difference <- maxdiff(se$value$.se, pr$value$se.fit[sel, sm$label])
      }
      rows[[length(rows) + 1L]] <- r
    }
  }
}
write.csv(do.call(rbind, rows), file.path(out, "prediction-edges.csv"), row.names = FALSE)

# Compare the exact same coefficient draws with explicit matrix multiplication.
sampling_rows <- list()
for (bs in c("mpi", "mpd", "cx", "cv", "micx", "micv", "mdcx", "mdcv",
  "miso", "mifo", "po", "ipo", "dpo", "lmpi", "lipl", "mpiBy", "tedmi", "tesmi1", "tismi")) {
  m <- inv$models[[bs]]
  nd <- d[1:12, ]
  X <- predict(m, nd, type = "lpmatrix")
  beta <- matrix(rep(coef(m), each = 2L), nrow = 2L)
  colnames(beta) <- names(coef(m))
  sm <- m$smooth[[1L]]
  idx <- sm$first.para:sm$last.para
  expected <- drop(X[, seq_along(coef(m)), drop = FALSE] %*% coef(m))
  term <- drop(X[, idx, drop = FALSE] %*% coef(m)[idx])
  for (op in c("smooth_samples", "fitted_samples", "derivative_samples")) {
    val <- capture_result(switch(op,
      smooth_samples = smooth_samples(m, data = nd, method = "user", draws = beta),
      fitted_samples = fitted_samples(m, data = nd, method = "user", draws = beta, scale = "linear_predictor"),
      derivative_samples = derivative_samples(m, data = nd, focal = "x", method = "user", draws = beta,
        scale = "linear_predictor", type = "central", eps = 1e-5)))
    diff <- NA_real_
    if (!nzchar(val$error)) {
      v <- val$value
      if (op == "smooth_samples") diff <- maxdiff(v$.value[v$.draw == 1], term)
      if (op == "fitted_samples") diff <- maxdiff(v$.fitted[v$.draw == 1], expected)
      if (op == "derivative_samples") {
        hi <- lo <- nd; hi$x <- hi$x + 1e-5; lo$x <- lo$x - 1e-5
        ref <- (predict(m, hi, type = "link") - predict(m, lo, type = "link")) / 2e-5
        diff <- maxdiff(v$.derivative[v$.draw == 1], ref)
      }
    }
    sampling_rows[[length(sampling_rows) + 1L]] <- data.frame(basis = bs, operation = op,
      error = val$error, max_difference = diff)
  }
}
write.csv(do.call(rbind, sampling_rows), file.path(out, "sampling-checks.csv"), row.names = FALSE)

# Missing public GAM methods and wrappers, including inherited generic methods.
m <- api$models$multi
beta <- matrix(rep(coef(m), each = 2L), nrow = 2L)
operations <- list(
  confint = function() confint(m),
  confint_smooth = function() confint(m, parm = "s(x)"),
  add_partial_residuals = function() add_partial_residuals(smooth_estimates(m), m),
  draw_residuals = function() draw(m, residuals = TRUE, parametric = FALSE),
  draw_without_parametric = function() draw(m, parametric = FALSE),
  draw_parametric = function() draw(m),
  evaluate_parametric_term = function() evaluate_parametric_term(m, term = "z"),
  fderiv = function() fderiv(m),
  generate_draws = function() generate_draws(m, n_draws = 5),
  inv_link = function() inv_link(m),
  is_factor_term = function() is_factor_term(m, term = "z"),
  user_draws = function() user_draws(m, draws = beta),
  mh_draws = function() mh_draws(m, n_draws = 5, burnin = 0),
  smooth_terms_model = function() smooth_terms(m),
  smooth_dim_model = function() smooth_dim(m),
  smooth_label_model = function() smooth_label(m),
  basis_size_model = function() basis_size(m),
  get_smooths_by_id = function() get_smooths_by_id(m, 1L),
  term_names_smooth = function() term_names(m$smooth[[1L]]),
  which_smooths = function() which_smooths(m, terms = "s(x)"),
  bayesplot_pp_check = function() bayesplot::pp_check(m, n_draws = 5, seed = 7),
  waic = function() loo::waic(m, n_draws = 5),
  theta = function() theta(m),
  nb_theta = function() nb_theta(m),
  model_edf_unconditional = function() model_edf(m, type = "unconditional"),
  variance_comp_direct = function() mgcv::gam.vcomp(m),
  variance_comp_reuse = function() getFromNamespace("variance_comp.gam", "gratia")(m))
extras <- lapply(names(operations), function(nm) {
  x <- capture_result(operations[[nm]]())
  data.frame(operation = nm, error = x$error, warnings = x$warnings, messages = x$messages,
    class = paste(class(x$value), collapse = ","))
})
write.csv(do.call(rbind, extras), file.path(out, "additional-api.csv"), row.names = FALSE)

# Check covariance and transformations in the fitted and unconstrained coordinates.
mc <- extra$not_exp
ii <- mc$p.ident
transformed <- mc$coefficients
transformed[ii] <- scam:::notExp(transformed[ii], mc$control$b.notexp, mc$control$threshold.notexp)
transform_checks <- c(coef_vs_stored = maxdiff(coef(mc), mc$coefficients.t),
  covariance_vs_stored = maxdiff(vcov(mc), mc$Vp.t),
  scam_softplus_vs_stored = maxdiff(transformed, coef(mc)),
  legacy_exp_fun_vs_stored = maxdiff(gratia:::exp_fun(mc)(mc$coefficients[ii]), coef(mc)[ii]))

# Demonstrate shape violations from Gaussian draws of already transformed coefficients.
md <- inv$models$tedmd
default_draws <- capture_result(gaussian_draws(md, n_draws = 1000, seed = 25))
B <- gaussian_draws(md, n_draws = 1000, seed = 25, mvn_method = "mgcv")
shape_checks <- list(default_sampler_error = default_draws$error,
  proportion_negative_coefficients = mean(B[, md$p.ident] < 0),
  proportion_draws_with_negative = mean(rowSums(B[, md$p.ident, drop = FALSE] < 0) > 0))
X <- predict(md, expand.grid(x = seq(0.1, 0.9, length.out = 20), z = 0.5), type = "lpmatrix")
F <- X %*% t(B)
shape_checks$proportion_non_decreasing_violation <- mean(apply(F, 2, function(x) any(diff(x) > 1e-10)))

# AR1 simulation must retain correlation and restart at the fitted series boundaries.
set.seed(75)
da <- d[order(d$x), ]
da$start <- seq_len(nrow(da)) %in% c(1L, 121L)
da$y <- 2 + exp(da$x) + as.numeric(arima.sim(model = list(ar = 0.7), n = nrow(da), sd = 0.15))
ar <- scam::scam(y ~ s(x, bs = "mpi", k = 8), data = da,
  AR1.rho = 0.7, AR.start = da$start)
dist <- gratia:::response_distribution(ar)
set.seed(2)
rd <- replicate(1000, dist$rd())
sim <- as.matrix(simulate(ar, nsim = 1000, seed = 2))
ps <- posterior_samples(ar, n_draws = 1000, seed = 2)
probes <- list(model_names = names(ar), stored_start = ar$AR.start,
  model_frame_start = ar$model[["(AR.start)"]], distribution_independent = dist$independent,
  distribution_joint_available = dist$joint_available,
  distribution_lag1 = cor(as.numeric(rd[-nrow(rd), ] - fitted(ar)[-nrow(rd)]),
    as.numeric(rd[-1, ] - fitted(ar)[-1])),
  simulate_lag1 = cor(as.numeric(sim[-nrow(sim), ] - fitted(ar)[-nrow(sim)]),
    as.numeric(sim[-1, ] - fitted(ar)[-1])),
  simulate_at_boundary = cor(sim[120, ], sim[121, ]))
grDevices::dev.off()
saveRDS(list(extra = extra, transformation = transform_checks, shape = shape_checks,
  ar = ar, ar_checks = probes), file.path(out, "detail-results.rds"))
cat("Transformation checks\n"); print(transform_checks)
cat("Shape checks\n"); print(shape_checks)
cat("AR1 checks\n"); print(probes[setdiff(names(probes), "model_names")])
