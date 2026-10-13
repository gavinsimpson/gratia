# Exact randomized PIT/quantile residuals for a ZINB GAMM.
# Run from the package root:
# Rscript notes/scalable-diagnostics/quantile-pit-count-model.R /path/to/output
# This is a plotting demonstration, not new public glmmTMB adapter support.
pkgload::load_all()
library("mgcv")
library("ggplot2")
library("patchwork")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply an output directory as the sole argument.")
output_dir <- args[[1L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(103)
d_tmb <- data.frame(x = runif(10000), group = factor(rep(1:40, each = 250)))
b <- rnorm(40, sd = .4)
mu <- exp(.6 + sin(6 * d_tmb$x) + b[d_tmb$group])
d_tmb$y <- rnbinom(10000, mu = mu, size = 2)
d_tmb$y[runif(10000) < plogis(-2 + 1.5 * d_tmb$x)] <- 0
m_tmb <- glmmTMB::glmmTMB(y ~ s(x, k = 10) + (1 | group),
  ziformula = ~x, data = d_tmb, family = glmmTMB::nbinom2())
stopifnot(isTRUE(m_tmb$sdr$pdHess), m_tmb$fit$convergence == 0)

# Conditional count-component mean, NB2 size, and zero-inflation probability.
# Response predictions alone would incorrectly fold zero inflation into mu.
mu_fit <- exp(predict(m_tmb, type = "link"))
size_fit <- predict(m_tmb, type = "disp")
pi_fit <- predict(m_tmb, type = "zprob")
y <- model.response(model.frame(m_tmb))
upper <- pi_fit + (1 - pi_fit) * pnbinom(y, mu = mu_fit, size = size_fit)
lower <- ifelse(y == 0, 0,
  pi_fit + (1 - pi_fit) * pnbinom(y - 1, mu = mu_fit, size = size_fit))
mass <- (1 - pi_fit) * dnbinom(y, mu = mu_fit, size = size_fit) +
  pi_fit * (y == 0)
stopifnot(all(lower >= 0), all(upper <= 1), all(upper > lower),
  isTRUE(all.equal(as.numeric(upper - lower), as.numeric(mass), tolerance = 1e-12)))
before <- .Random.seed
pit <- withr::with_seed(42, lower + runif(length(y)) * (upper - lower))
stopifnot(identical(.Random.seed, before), all(pit > 0 & pit < 1))
quantile <- qnorm(pit)
stopifnot(all(is.finite(quantile)))

# Reuse the aligned model data and existing model-independent density renderer.
prepared <- gratia:::diagnostic_data(m_tmb)
panels <- list()
for (type in c("quantile", "pit")) {
  d <- prepared
  d$type <- type
  d$data$residuals <- if (type == "quantile") quantile else pit
  row <- lapply(c("histogram", "kde"), function(method) {
    p <- scalable_residuals_linpred_plot(d, density_method = method,
      density_bins = c(50, 50))
    stopifnot(sum(p$data$count) == length(y), nrow(p$data) <= 2500)
    integrals <- tapply(p$data$value * (p$data$ymax - p$data$ymin),
      p$data$xmin, sum)
    stopifnot(max(abs(integrals - 1)) < 1e-10)
    p
  })
  maximum <- max(vapply(row, function(p) max(p$data$value), numeric(1)))
  for (i in seq_along(row)) {
    panels[[length(panels) + 1L]] <- suppressMessages(row[[i]] +
      scale_fill_viridis_c(transform = "sqrt", limits = c(0, maximum),
        name = "Conditional density")) +
      coord_cartesian(ylim = if (type == "quantile") c(-5, 5) else c(-.15, 1.15)) +
      theme_minimal(base_size = 12) +
      labs(title = paste(if (type == "quantile") "Quantile residuals" else "Randomized PIT",
        if (i == 1) "- unsmoothed bins" else "- KernSmooth KDE"))
  }
}
p <- wrap_plots(panels, ncol = 2) + plot_annotation(
  title = "Zero-inflated negative binomial GAMM: randomized quantile and PIT residuals",
  subtitle = "Same 10,000-observation model; shared CDF randomization; 50 x 50 grids; common colour limits within each row",
  caption = "KDE uses Gaussian kernels without boundary correction: PIT density tapers near 0 and 1 and extends beyond them.")
grDevices::png(file.path(output_dir, "13-zinb-quantile-pit-conditional.png"),
  width = 15, height = 10.5, units = "in", res = 150, bg = "white", type = "cairo")
tryCatch(print(p), finally = grDevices::dev.off())

# A bounded normal QQ and histogram provide a marginal calibration check.
n <- length(quantile)
index <- seq_len(n)
z <- qnorm(ppoints(n))
reference <- list(data = data.frame(theoretical = z, residuals = sort(quantile),
  lower = qnorm(qbeta(.05, index, n + 1L - index)),
  upper = qnorm(qbeta(.95, index, n + 1L - index))),
  center = z, method = "normal", intercept = 0, slope = 1)
binned <- gratia:::bin_diagnostic_qq(reference, qq_bins = 1000L)
qq <- gratia:::render_diagnostic_qq(binned, reference, type = "quantile",
  title = "Randomized quantile residuals", subtitle = paste(nrow(binned), "QQ points drawn")) +
  theme_minimal(base_size = 12)
d <- prepared
d$type <- "quantile"
d$data$residuals <- quantile
hist <- scalable_residuals_hist_plot(d, n_bins = 40) +
  theme_minimal(base_size = 12)
grDevices::png(file.path(output_dir, "14-zinb-quantile-qq-histogram.png"),
  width = 13, height = 5.5, units = "in", res = 150, bg = "white", type = "cairo")
tryCatch(print(qq + hist + plot_annotation(title = "Marginal checks for the same ZINB model")),
  finally = grDevices::dev.off())
cat("Convergence:", m_tmb$fit$convergence, "; positive-definite Hessian:", m_tmb$sdr$pdHess, "\n")
cat("PIT mean:", mean(pit), "; variance:", var(pit), "\n")
cat("Quantile mean:", mean(quantile), "; SD:", sd(quantile), "\n")
cat("QQ rows:", nrow(binned), "of", n, "observations\n")
