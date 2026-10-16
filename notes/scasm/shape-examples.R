# Run from the repository root: Rscript notes/scasm/shape-examples.R
# The displayed intervals describe centred smooth components on model coordinates.
pkgload::load_all(quiet = TRUE)
library("ggplot2")

output <- file.path("notes", "scasm", "shape-examples")
dir.create(output, showWarnings = FALSE)

compare_intervals <- function(model, ..., label) {
  args <- list(object = model, ..., interval = "simultaneous",
    n_sim = 5000, seed = 1809)
  initial <- do.call(smooth_estimates, c(args, list(interval_method = "gaussian")))
  shaped <- do.call(smooth_estimates, c(args, list(interval_method = "shape")))
  data.frame(initial, initial_lower = initial$.lower_ci,
    initial_upper = initial$.upper_ci, shape_lower = shaped$.lower_ci,
    shape_upper = shaped$.upper_ci, shape_estimate = shaped$.estimate,
    example = label)
}

set.seed(1801)
d <- data.frame(x = runif(160), z = runif(160))
d$y <- 0.4 * d$x + rnorm(nrow(d), sd = 0.3)
increasing <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 8), data = d)
a <- compare_intervals(increasing, n = 101, label = "Increasing curve")

set.seed(1806)
d <- data.frame(x = runif(160))
d$y <- 0.3 * d$x + 0.3 * d$x^2 + rnorm(nrow(d), sd = 0.3)
convex <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "c+", k = 8), data = d)
b <- compare_intervals(convex, n = 101, label = "Convex curve")
curves <- rbind(a, b)

plot_intervals <- function(data) {
  ggplot(data, aes(x = x)) +
    geom_ribbon(aes(ymin = shape_lower, ymax = shape_upper),
      fill = "#B9DDD7", alpha = 0.65) +
    geom_line(aes(y = initial_lower, colour = "Initial Gaussian",
      linetype = "Initial Gaussian"), linewidth = 0.7) +
    geom_line(aes(y = initial_upper, colour = "Initial Gaussian",
      linetype = "Initial Gaussian"), linewidth = 0.7) +
    geom_line(aes(y = shape_lower, colour = "Shape-enforced",
      linetype = "Shape-enforced"), linewidth = 0.75) +
    geom_line(aes(y = shape_upper, colour = "Shape-enforced",
      linetype = "Shape-enforced"), linewidth = 0.75) +
    geom_line(aes(y = shape_estimate), colour = "#243447", linewidth = 0.85) +
    facet_wrap(~example, scales = "free_y", nrow = 1) +
    scale_colour_manual(NULL, values = c("Initial Gaussian" = "#BF6B1F",
      "Shape-enforced" = "#087E8B")) +
    scale_linetype_manual(NULL, values = c("Initial Gaussian" = "dashed",
      "Shape-enforced" = "solid")) +
    labs(x = "x (model coordinate)", y = "Smooth component",
      subtitle = "Black: fitted component. Dashed: initial limits. Teal: shape-enforced limits.",
      caption = paste("Initial 95% Gaussian simultaneous band on each complete grid.",
        "Shape enforcement preserves whole-grid containment; nominal coverage depends on the initial band.")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 9, hjust = 0),
      strip.text = element_text(face = "bold"))
}

p <- plot_intervals(curves) + labs(title = "Shape-enforced intervals for two scasm curves")
ggsave(file.path(output, "curves.png"), p, width = 11, height = 5, dpi = 160)
utils::write.csv(curves, file.path(output, "curves.csv"), row.names = FALSE)

set.seed(1807)
d <- data.frame(x = runif(300), z = runif(300))
d$y <- 0.4 * d$x + 0.6 * (d$z - 0.5)^2 + rnorm(nrow(d), sd = 0.3)
surface <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5),
  xt = list("m+", NULL), np = FALSE), data = d)
grid <- expand.grid(x = seq(min(d$x), max(d$x), length.out = 101),
  z = seq(min(d$z), max(d$z), length.out = 21))
all_slices <- compare_intervals(surface, data = grid, dist = 0,
  label = "Surface increasing with x")
z_levels <- sort(unique(all_slices$z))[c(3, 11, 19)]
slices <- all_slices[all_slices$z %in% z_levels, ]
slices$example <- sprintf("Surface slice: z = %.2f", slices$z)
p <- plot_intervals(slices) + labs(title = "Intervals for a tensor surface, increasing with x",
  caption = paste("Initial 95% Gaussian simultaneous band on the full 101 x 21 grid; three slices shown.",
    "The surface is unrestricted in z. All panels share the full-grid calibration."))
ggsave(file.path(output, "surface-slices.png"), p, width = 12, height = 5, dpi = 160)
utils::write.csv(all_slices, file.path(output, "surface-grid.csv"), row.names = FALSE)

summarise_change <- function(data) {
  c(max_endpoint_change = max(abs(c(data$shape_lower - data$initial_lower,
      data$shape_upper - data$initial_upper))),
    initial_max_width = max(data$initial_upper - data$initial_lower),
    shape_max_width = max(data$shape_upper - data$shape_lower))
}
print(rbind(increasing = summarise_change(a), convex = summarise_change(b),
  surface = summarise_change(all_slices)))
