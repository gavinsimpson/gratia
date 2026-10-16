# Run at the repository root with an installed checkout, or add --load-all.
args <- commandArgs(trailingOnly = TRUE)
if ("--load-all" %in% args) pkgload::load_all(quiet = TRUE)
library("gratia")
library("mgcv")
ns <- asNamespace("gratia")
`%||%` <- get("%||%", ns)
audit <- get("sampling_smoothing_info", ns)
setup <- get("sampling_smoothing_setup", ns)
set.seed(501)
d <- data.frame(x = runif(180), z = runif(180), w = runif(180, .5, 2))
d$o <- .2 + .3 * d$x
d$y <- sin(6 * d$x) + cos(3 * d$z) + d$o + rnorm(180, sd = .3)
d$count <- rnbinom(nrow(d), mu = exp(1 + sin(6 * d$x)), size = 3)
d$positive <- exp(d$y)
f <- y ~ s(x, k = 5) + s(z, k = 5) + offset(o)
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
  gam_ML_nb = list(engine = "gam", method = "ML", family = nb(),
    formula = count ~ s(x, k = 5) + s(z, k = 5)),
  gam_ML_tw = list(engine = "gam", method = "ML", family = tw(),
    formula = positive ~ s(x, k = 5)),
  gam_ML_scat = list(engine = "gam", method = "ML", family = scat()),
  gam_ML_gaulss = list(engine = "gam", method = "ML", family = gaulss(),
    formula = list(y ~ s(x, k = 5), ~s(z, k = 5)))))
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
  info <- audit(fit)
  dimension <- function(x) if (is.null(x)) "absent" else
    if (is.null(dim(x))) paste0("scalar (length ", length(x), ")") else
      paste(dim(x), collapse = "x")
  indices <- function(x) if (is.null(x)) "unknown" else
    paste0("family=", paste(x$family, collapse = ","),
      "; log(sp)=", paste(x$log_sp, collapse = ","),
      "; log(scale)=", paste(x$log_scale, collapse = ","))
  probe <- tryCatch(setup(fit), error = identity)
  cbind(out, data.frame(actual_method = info$fitting$actual_method,
    stored_score_label = paste(info$fitting$stored_score_label, collapse = ","),
    discrete = info$fitting$discrete,
    stored_optimizer = paste(info$fitting$stored_optimizer, collapse = ","),
    estimated_family_parameters = info$family_parameters$estimated,
    scale_estimated = isTRUE(fit$scale.estimated), free_sp = length(fit$sp),
    full_sp = length(fit$full.sp %||% fit$sp),
    V_sp = dimension(info$V.sp), sp_vcov = dimension(info$sp.vcov),
    db_drho = dimension(info$db.drho), Vc = dimension(info$Vc),
    V_sp_order = indices(info$V_sp_layout), sp_vcov_order = indices(info$sp_vcov_layout),
    research_calculation = if (inherits(probe, "error")) conditionMessage(probe) else
        if (identical(probe$conditional_backend, "family"))
          "Native conditional normal estimation-uncertainty mixture" else
          "Gaussian estimation-uncertainty mixture"))
})
fields <- unique(unlist(lapply(rows, names)))
rows <- do.call(rbind, lapply(rows, function(x) {
  x[setdiff(fields, names(x))] <- NA
  x[fields]
}))
write.csv(rows, "notes/posterior-chains/smoothing-methods.csv", row.names = FALSE)
writeLines(trimws(c(paste("R", getRversion(), "; mgcv", packageVersion("mgcv")),
  capture.output(print(rows, row.names = FALSE)), "", capture.output(sessionInfo())),
  which = "right"), "notes/posterior-chains/smoothing-methods-validation.txt")
