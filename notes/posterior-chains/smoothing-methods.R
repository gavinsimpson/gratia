# Run at the repository root with an installed checkout, or add --load-all.
args <- commandArgs(trailingOnly = TRUE)
if ("--load-all" %in% args) pkgload::load_all(quiet = TRUE)
library("gratia")
library("mgcv")
ns <- asNamespace("gratia")
`%||%` <- get("%||%", ns)
audit <- get("sampling_smoothing_info", ns)
setup <- get("sampling_smoothing_setup", ns)
ncv_delta <- get("sampling_smoothing_ncv_delta", ns)
set.seed(501)
d <- data.frame(x = runif(180), z = runif(180), w = runif(180, .5, 2))
d$o <- .2 + .3 * d$x
d$y <- sin(6 * d$x) + cos(3 * d$z) + d$o + rnorm(180, sd = .3)
d$count <- rnbinom(nrow(d), mu = exp(1 + sin(6 * d$x)), size = 3)
d$positive <- exp(d$y)
f <- y ~ s(x, k = 5) + s(z, k = 5) + offset(o)
f0 <- y ~ s(x, k = 5) + s(z, k = 5)
linked <- y ~ s(x, k = 5, id = "shared") + s(z, k = 5, id = "shared")
fixed <- y ~ s(x, k = 5, sp = .1) + s(z, k = 5)
cases <- list()
for (engine in c("gam", "bam")) for (method in c("ML", "REML", "fREML")) {
  cases[[paste(engine, method, sep = "_")]] <- list(engine = engine, method = method)
}
cases <- c(cases, list(
  gam_ML_known_scale = list(engine = "gam", method = "ML", scale = .09),
  bam_ML_known_scale = list(engine = "bam", method = "ML", scale = .09),
  bam_fREML_all_fixed = list(engine = "bam", method = "fREML",
    formula = y ~ s(x, k = 5, sp = .1) + s(z, k = 5, sp = .2)),
  bam_ML_discrete_requested = list(engine = "bam", method = "ML", discrete = TRUE),
  gam_ML_efs = list(engine = "gam", method = "ML", optimizer = "efs"),
  gam_NCV = list(engine = "gam", method = "NCV", formula = f0),
  gam_NCV_one_sp = list(engine = "gam", method = "NCV", formula = y ~ s(x, k = 5)),
  gam_QNCV = list(engine = "gam", method = "QNCV", formula = f0),
  gam_NCV_offset = list(engine = "gam", method = "NCV"),
  gam_NCV_no_jackknife = list(engine = "gam", method = "NCV", formula = f0,
    nei = list(jackknife = FALSE)),
  gam_NCV_linked = list(engine = "gam", method = "NCV", formula = linked),
  gam_NCV_fixed = list(engine = "gam", method = "NCV", formula = fixed),
  bam_NCV = list(engine = "bam", method = "NCV", formula = f0, discrete = TRUE),
  bam_NCV_discrete_reset = list(engine = "bam", method = "NCV", formula = f0),
  gam_ML_nb = list(engine = "gam", method = "ML", family = nb(),
    formula = count ~ s(x, k = 5) + s(z, k = 5)),
  gam_ML_tw = list(engine = "gam", method = "ML", family = tw(),
    formula = positive ~ s(x, k = 5)),
  gam_ML_scat = list(engine = "gam", method = "ML", family = scat()),
  gam_ML_gaulss = list(engine = "gam", method = "ML", family = gaulss(),
    formula = list(y ~ s(x, k = 5), ~s(z, k = 5)))))
fits <- list()
rows <- lapply(names(cases), function(nm) {
  spec <- cases[[nm]]
  engine <- spec$engine
  spec$engine <- NULL
  spec$formula <- spec$formula %||% f
  spec$data <- d
  spec$weights <- d$w
  warnings <- character()
  fit <- withCallingHandlers(tryCatch(do.call(get(engine), spec), error = identity),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  out <- data.frame(case = nm, requested_method = spec$method,
    fit_error = if (inherits(fit, "error")) conditionMessage(fit) else "",
    warnings = paste(warnings, collapse = "; "))
  if (inherits(fit, "error")) return(out)
  fits[[nm]] <<- fit
  info <- audit(fit)
  dimension <- function(x) if (is.null(x)) "absent" else
    if (is.null(dim(x))) paste0("scalar (length ", length(x), ")") else
      paste(dim(x), collapse = "x")
  indices <- function(x) if (is.null(x)) "unknown" else
    paste0("family=", paste(x$family, collapse = ","),
      "; log(sp)=", paste(x$log_sp, collapse = ","),
      "; log(scale)=", paste(x$log_scale, collapse = ","))
  probe <- tryCatch(if (fit$method %in% c("NCV", "QNCV"))
    ncv_delta(fit) else setup(fit), error = identity)
  cbind(out, data.frame(actual_method = info$fitting$actual_method,
    stored_score_label = paste(info$fitting$stored_score_label, collapse = ","),
    discrete = info$fitting$discrete,
    stored_optimizer = paste(info$fitting$stored_optimizer, collapse = ","),
    estimated_family_parameters = info$family_parameters$estimated,
    scale_estimated = isTRUE(fit$scale.estimated), free_sp = length(fit$sp),
    full_sp = length(fit$full.sp %||% fit$sp),
    V_sp = dimension(info$V.sp), sp_vcov = dimension(info$sp.vcov),
    db_drho = dimension(info$db.drho), Vc = dimension(info$Vc), Vg = dimension(info$Vg),
    V_sp_order = indices(info$V_sp_layout), sp_vcov_order = indices(info$sp_vcov_layout),
    research_calculation = if (inherits(probe, "error")) conditionMessage(probe) else
      if (fit$method %in% c("NCV", "QNCV")) "local delta contribution only" else
        "Gaussian estimation-uncertainty mixture"))
})
fields <- unique(unlist(lapply(rows, names)))
rows <- do.call(rbind, lapply(rows, function(x) {
  x[setdiff(fields, names(x))] <- NA
  x[fields]
}))
write.csv(rows, "notes/posterior-chains/smoothing-methods.csv", row.names = FALSE)
# Independently differentiate exact Gaussian leave-one-out losses. No gratia
# conditional evaluator or mgcv derivative is used to construct these scores.
ncv_check <- function(fit) {
  X <- predict(fit, type = "lpmatrix")
  w <- fit$prior.weights
  y <- fit$y - (fit$offset %||% 0)
  penalties <- lapply(fit$smooth, function(sm) {
    S <- matrix(0, ncol(X), ncol(X))
    j <- sm$first.para:sm$last.para
    S[j, j] <- sm$S[[1L]]
    S
  })
  losses <- function(rho) {
    P <- crossprod(X, w * X)
    for (k in seq_along(penalties)) P <- P + exp(rho[k]) * penalties[[k]]
    inverse <- solve(P)
    beta <- inverse %*% crossprod(X, w * y)
    h <- w * rowSums((X %*% inverse) * X)
    w * drop((y - X %*% beta) / (1 - h))^2
  }
  rho <- log(fit$sp)
  eps <- 1e-5
  gradients <- vapply(seq_along(rho), function(k) {
    step <- rep(0, length(rho)); step[k] <- eps
    (losses(rho + step) - losses(rho - step)) / (2 * eps)
  }, numeric(nrow(X)))
  B <- solve(fit$outer.info$hess)
  sandwich <- B %*% crossprod(gradients) %*% B
  data.frame(method = fit$method,
    criterion_error = abs(sum(losses(rho)) - as.numeric(fit$gcv.ubre)),
    gradient_sum_error = max(abs(colSums(gradients) - fit$outer.info$grad)),
    sandwich_error = max(abs(sandwich - fit$Vg)),
    Vg_inverse_hessian_difference = max(abs(fit$Vg - B)),
    delta_error = max(abs(ncv_delta(fit)$propagated_covariance - fit$Vc)))
}
checks <- do.call(rbind, lapply(fits[c("gam_NCV", "gam_QNCV")], ncv_check))
write.csv(checks, "notes/posterior-chains/smoothing-ncv-checks.csv", row.names = FALSE)
# Demonstrate the offset convention on the fitted NCV object explicitly.
fit <- fits$gam_NCV_offset
X <- predict(fit, type = "lpmatrix")
P <- crossprod(X, fit$prior.weights * X)
for (k in seq_along(fit$smooth)) {
  sm <- fit$smooth[[k]]
  j <- sm$first.para:sm$last.para
  P[j, j] <- P[j, j] + fit$sp[k] * sm$S[[1L]]
}
h <- fit$prior.weights * rowSums((X %*% solve(P)) * X)
mean_cv <- fit$y - (fit$y - fit$linear.predictors) / (1 - h)
native_cv <- attr(fit$gcv.ubre, "eta.cv")
offset_check <- data.frame(
  predictor_error_with_offset = max(abs(mean_cv - native_cv)),
  predictor_error_after_removing_offset = max(abs(mean_cv - fit$offset - native_cv)),
  correct_leave_one_out_loss = sum(fit$prior.weights * (fit$y - mean_cv)^2),
  native_reported_loss = as.numeric(fit$gcv.ubre))
write.csv(offset_check, "notes/posterior-chains/smoothing-ncv-offset.csv", row.names = FALSE)
stopifnot(max(checks$sandwich_error) < 1e-5, max(checks$criterion_error) < 1e-7,
  max(checks$gradient_sum_error) < 1e-6, max(checks$delta_error) < 1e-10)
writeLines(trimws(c(paste("R", getRversion(), "; mgcv", packageVersion("mgcv")),
  capture.output(print(rows, row.names = FALSE)), "", capture.output(checks), "",
  capture.output(offset_check), "", capture.output(sessionInfo())), which = "right"),
  "notes/posterior-chains/smoothing-methods-validation.txt")
