args <- commandArgs(TRUE)
repo <- if (length(args)) args[1L] else getwd()
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
dir.create(out, showWarnings = FALSE)
library("scam")
pkgload::load_all(repo, quiet = TRUE)
set.seed(1047)
d <- data.frame(x = runif(240, 0.05, 0.95), z = runif(240, 0.05, 0.95),
  w = runif(240, 0.5, 1.5), fac = factor(rep(c("a", "b"), 120)))
d$y <- 2 + exp(d$x) + 0.4 * d$z + rnorm(nrow(d), sd = 0.15)
s <- mgcv::s
ns <- asNamespace("scam")
constructors <- grep("^smooth.construct.*smooth.spec$", ls(ns), value = TRUE)
bases <- sub(".smooth.spec$", "", sub("^smooth.construct.", "", constructors))
capture_result <- function(expr) {
  w <- character()
  val <- tryCatch(withCallingHandlers(expr, warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")
  }), error = identity)
  list(value = val, error = if (inherits(val, "error")) conditionMessage(val) else "",
    warnings = paste(unique(w), collapse = " | "))
}
maxdiff <- function(a, b) {
  if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA_real_)
  v <- abs(as.numeric(a) - as.numeric(b))
  if (!any(is.finite(v))) return(NA_real_)
  max(v, na.rm = TRUE)
}
models <- list()
plots <- list()
rows <- list()
grDevices::pdf(file.path(out, "native-all-smooths.pdf"), width = 7, height = 5)
for (bs in c(bases, "cr", "tp", "ps")) {
  cat("Auditing", bs, "\n"); flush.console()
  bivar <- startsWith(bs, "te") || startsWith(bs, "ti")
  by <- endsWith(bs, "By")
  dd <- d
  f <- if (bivar) {
    as.formula(sprintf("y ~ s(x, z, bs = '%s', k = c(5, 5))", bs))
  } else if (by) {
    as.formula(sprintf("y ~ s(x, by = w, bs = '%s', k = 8)", bs))
  } else if (bs %in% c("lmpi", "lipl")) {
    as.formula(sprintf("y ~ s(x, bs = '%s', k = 12, xt = list(xc = 0.5))", bs))
  } else as.formula(sprintf("y ~ s(x, bs = '%s', k = 8)", bs))
  if (bs %in% c("mpd", "mpdBy", "mdcx", "mdcxBy", "dpo"))
    dd$y <- 2 + exp(-dd$x) + rnorm(nrow(dd), sd = 0.1)
  if (bs %in% c("mdcv", "mdcvBy"))
    dd$y <- 4 - dd$x^2 + rnorm(nrow(dd), sd = 0.1)
  if (bs %in% c("cv", "cvBy", "micv", "micvBy"))
    dd$y <- 2 + sqrt(dd$x) + rnorm(nrow(dd), sd = 0.1)
  fm <- capture_result(scam::scam(f, data = dd))
  r <- list(basis = bs, dimension = if (bivar) 2L else 1L,
    fit_error = fm$error, fit_warning = fm$warnings)
  if (nzchar(fm$error)) { rows[[bs]] <- r; next }
  m <- fm$value
  models[[bs]] <- m
  r$smooth_class <- class(m$smooth[[1L]])[1L]
  r$converged <- isTRUE(m$conv)
  r$selection <- m$method
  pp <- capture_result(scam::plot.scam(m, n = 100, n2 = 20, se = 1,
    rug = FALSE, too.far = 0, main = bs))
  r$plot_error <- pp$error
  r$plot_warning <- pp$warnings
  if (!nzchar(pp$error)) {
    P <- pp$value[[1L]]
    plots[[bs]] <- P
    nd <- if (bivar) expand.grid(x = P$x, z = P$y) else data.frame(x = P$x)
    if (by) nd$w <- 1
  } else {
    nd <- if (bivar) expand.grid(x = seq(min(dd$x), max(dd$x), length.out = 20),
      z = seq(min(dd$z), max(dd$z), length.out = 20)) else
      data.frame(x = seq(min(dd$x), max(dd$x), length.out = 100))
    if (by) nd$w <- 1
    P <- NULL
  }
  pr <- capture_result(predict(m, nd, type = "terms", se.fit = TRUE))
  r$predict_error <- pr$error
  se <- capture_result(smooth_estimates(m, data = nd, overall_uncertainty = FALSE))
  r$estimates_error <- se$error
  dr <- capture_result(draw(m, data = nd, overall_uncertainty = FALSE,
    rug = FALSE, parametric = FALSE, wrap = FALSE))
  r$draw_error <- dr$error
  r$draw_panels <- if (nzchar(dr$error)) NA_integer_ else length(dr$value)
  if (!nzchar(se$error)) {
    r$estimate_vs_predict <- if (!nzchar(pr$error)) maxdiff(se$value$.estimate, pr$value$fit) else NA_real_
    r$se_vs_predict <- if (!nzchar(pr$error)) maxdiff(se$value$.se, pr$value$se.fit) else NA_real_
    r$estimate_vs_plot <- maxdiff(se$value$.estimate, P$fit)
    r$se_vs_plot <- maxdiff(se$value$.se, P$se)
    delta <- as.numeric(se$value$.estimate) - as.numeric(P$fit)
    r$plot_difference_range <- if (length(delta)) diff(range(delta, na.rm = TRUE)) else NA_real_
  }
  if (!nzchar(pr$error) && !is.null(P)) {
    r$predict_vs_plot <- maxdiff(pr$value$fit, P$fit)
    delta <- as.numeric(pr$value$fit) - as.numeric(P$fit)
    r$predict_plot_difference_range <- diff(range(delta, na.rm = TRUE))
  }
  ba <- capture_result(basis(m, data = nd))
  r$basis_error <- ba$error
  de <- capture_result(if (bivar) partial_derivatives(m, data = nd, focal = "x") else
    derivatives(m, data = nd, type = "central"))
  r$derivatives_error <- de$error
  ov <- capture_result(overview(m))
  r$overview_error <- ov$error
  rows[[bs]] <- r
  saveRDS(list(models = models, plots = plots, rows = rows, data = d), file.path(out, "inventory.rds"))
}
grDevices::dev.off()
cols <- unique(unlist(lapply(rows, names)))
tab <- do.call(rbind, lapply(rows, function(r) {
  missing <- setdiff(cols, names(r)); r[missing] <- NA; as.data.frame(r[cols])
}))
rownames(tab) <- NULL
write.csv(tab, file.path(out, "smooth-inventory.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
print(tab[, c("basis", "fit_error", "estimates_error", "estimate_vs_plot", "draw_error")], row.names = FALSE)
