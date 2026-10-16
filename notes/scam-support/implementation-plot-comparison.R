# Reproduce the current implementation comparison without the audit model cache.
# Run outside the sandbox from the repository root:
# Rscript notes/scam-support/implementation-plot-comparison.R
args <- commandArgs(TRUE)
repo <- normalizePath(if (length(args)) args[1L] else getwd(), mustWork = TRUE)
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
library("scam")
pkgload::load_all(repo, quiet = TRUE)

set.seed(824)
d <- expand.grid(x = seq(0.05, 0.95, length.out = 16),
  z = seq(0.05, 0.95, length.out = 16))
d$y <- 2 + exp(d$x) + 0.5 * exp(d$z) + 0.3 * d$x * d$z +
  rnorm(nrow(d), sd = 0.08)
s <- mgcv::s
models <- list(
  mifo = scam::scam(y ~ s(x, bs = "mifo", k = 8), data = d, sp = 0.1),
  tedmi = scam::scam(y ~ s(x, z, bs = "tedmi", k = c(5, 5)),
    data = d, sp = c(0.1, 0.1)))
stopifnot(all(vapply(models, function(m) isTRUE(m$conv), logical(1))))

# Native methods choose the grids. Supply those exact coordinates to draw().
# Match curve interval multipliers, axis limits, surface colour limits and
# exclusion distance; retain each method's own plotting code.
critical <- qnorm(0.975)
curve_n <- 100L
surface_n <- 40L
reference <- list()
device_file <- tempfile(fileext = ".pdf")
grDevices::pdf(device_file)
reference$mifo <- scam::plot.scam(models$mifo, n = curve_n,
  se = critical, shade = TRUE, rug = FALSE, too.far = 0)[[1L]]
reference$tedmi <- scam::plot.scam(models$tedmi, n2 = surface_n,
  se = FALSE, scheme = 2, rug = FALSE, too.far = 0)[[1L]]
grDevices::dev.off()
unlink(device_file)

curve_data <- data.frame(x = reference$mifo$x)
surface_data <- expand.grid(x = reference$tedmi$x, z = reference$tedmi$y)
curve_x <- range(curve_data$x)
curve_y <- range(reference$mifo$fit - reference$mifo$se,
  reference$mifo$fit + reference$mifo$se)
surface_x <- range(surface_data$x)
surface_y <- range(surface_data$z)
surface_z <- range(reference$tedmi$fit)

plots <- list(
  mifo = draw(models$mifo, data = curve_data, centre = "plot",
    overall_uncertainty = FALSE, ci_level = 0.95, rug = FALSE, dist = 0,
    parametric = FALSE, wrap = FALSE)[[1L]],
  tedmi = draw(models$tedmi, data = surface_data, centre = "plot",
    overall_uncertainty = FALSE, rug = FALSE, dist = 0,
    continuous_fill = ggplot2::scale_fill_gradientn(
      colours = grDevices::heat.colors(50), limits = surface_z,
      oob = scales::squish),
    parametric = FALSE, wrap = FALSE)[[1L]])
plots$mifo <- plots$mifo + ggplot2::labs(title = "mifo", y = "Partial effect") +
  ggplot2::coord_cartesian(xlim = curve_x, ylim = curve_y, expand = FALSE)
plots$tedmi <- plots$tedmi + ggplot2::labs(title = "tedmi", x = "x", y = "z") +
  ggplot2::coord_cartesian(xlim = surface_x, ylim = surface_y, expand = FALSE)

comparison <- data.frame(
  basis = c("mifo", "tedmi"),
  points_per_axis = c(curve_n, surface_n),
  exclusion_distance = 0,
  max_function_difference = c(
    max(abs(plots$mifo$data$.estimate - as.numeric(reference$mifo$fit))),
    max(abs(plots$tedmi$data$.estimate - as.numeric(reference$tedmi$fit)))),
  max_curve_se_difference = c(
    max(abs(plots$mifo$data$.se - reference$mifo$se / critical)), NA_real_))
stopifnot(all(comparison$max_function_difference < 1e-8),
  comparison$max_curve_se_difference[1L] < 1e-8)
write.csv(comparison, file.path(out, "implementation-plot-comparison.csv"), row.names = FALSE)

image_file <- file.path(out, "implementation-plot-comparison.png")
ragg::agg_png(image_file, width = 1800, height = 1500, res = 180)
for (i in seq_along(models)) {
  par(fig = c(0.015, 0.49, 1 - i / 2 + 0.025, 1 - (i - 1) / 2 - 0.055),
    new = i > 1L, mar = c(4, 4, 2.5, 1), mgp = c(2.5, 0.8, 0),
    cex = 1.05, xaxs = "i", yaxs = "i")
  if (i == 1L) {
    scam::plot.scam(models$mifo, n = curve_n, se = critical, shade = TRUE,
      rug = FALSE, too.far = 0, xlim = curve_x, ylim = curve_y,
      main = "mifo", xlab = "x", ylab = "Partial effect")
  } else {
    scam::plot.scam(models$tedmi, n2 = surface_n, se = FALSE, scheme = 2,
      rug = FALSE, too.far = 0, xlim = surface_x, ylim = surface_y,
      main = "tedmi", xlab = "x", ylab = "z")
  }
}
grid::grid.text("scam::plot.scam()", x = 0.25, y = 0.977,
  gp = grid::gpar(fontsize = 17, fontface = "bold"))
grid::grid.text("gratia::draw(centre = 'plot')", x = 0.75, y = 0.977,
  gp = grid::gpar(fontsize = 17, fontface = "bold"))
for (i in seq_along(plots)) {
  viewport <- grid::viewport(x = 0.75, y = 1 - (i - 0.5) / 2 - 0.02,
    width = 0.49, height = 0.435)
  print(plots[[i]], newpage = FALSE, vp = viewport)
}
grDevices::dev.off()
print(comparison, row.names = FALSE)
cat("Saved", normalizePath(image_file), "\n")
