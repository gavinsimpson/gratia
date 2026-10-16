# Run from the repository root:
# Rscript notes/scasm/shape-examples-3d-comparison.R
# Set SHAPE_3D_ARTIFACT_DIR to save the HTML and RDS outside the figure folder.
# Uses the original saved grid; no refitting or recalibration is performed.
library("ggplot2")
library("ggcube")

output <- file.path("notes", "scasm", "shape-examples")
artifact_dir <- Sys.getenv("SHAPE_3D_ARTIFACT_DIR", output)
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
grid <- utils::read.csv(file.path(output, "surface-grid.csv"))
stopifnot(nrow(grid) == 101L * 21L, !anyDuplicated(grid[c("x", "z")]))

methods <- c("Original Gaussian", "Shape-enforced")
labels <- c("Lower limit", "Fitted smooth", "Upper limit")
surfaces <- do.call(rbind, lapply(methods, function(method) {
  columns <- if (method == "Original Gaussian") {
    c("initial_lower", ".estimate", "initial_upper")
  } else c("shape_lower", "shape_estimate", "shape_upper")
  do.call(rbind, lapply(seq_along(labels), function(i) {
    data.frame(x = grid$x, z = grid$z, height = grid[[columns[i]]],
      surface = labels[i], method = method)
  }))
}))
surfaces$surface <- factor(surfaces$surface, levels = labels)
surfaces$method <- factor(surfaces$method, levels = methods)
max_change <- max(abs(c(grid$shape_lower - grid$initial_lower,
  grid$shape_upper - grid$initial_upper)))

# Fixed facets and explicit common limits give both panels the same scales.
# A single coordinate system also gives them exactly the same viewing angle.
limits <- list(x = range(surfaces$x), y = range(surfaces$z),
  z = range(surfaces$height))
p <- ggplot(surfaces, ggcube::aes(x = x, y = z, z = height,
    group = surface, fill = surface, alpha = surface)) +
  geom_surface_3d(colour = NA) +
  facet_wrap(~method, nrow = 1, scales = "fixed") +
  coord_3d(yaw = -35, roll = -60, persp = FALSE, ratio = c(1, 1, 0.85),
    light = light(backface_scale = 1, contrast = 0.4)) +
  scale_x_continuous(limits = limits$x) +
  scale_y_continuous(limits = limits$y) +
  scale_z_continuous(name = "Smooth component", limits = limits$z) +
  scale_fill_manual(NULL, values = c("Lower limit" = "#C77B31",
    "Fitted smooth" = "#243447", "Upper limit" = "#087E8B")) +
  scale_alpha_manual(values = c("Lower limit" = 0.5,
    "Fitted smooth" = 0.8, "Upper limit" = 0.4), guide = "none") +
  labs(x = "x", y = "z", title = "Original and shape-enforced interval surfaces",
    subtitle = "Same tensor fit, 95% initial Gaussian full-grid band, axis limits and viewing angle",
    caption = paste("All 101 x 21 evaluation points are retained. Increasing in x; unrestricted in z.",
      sprintf("Largest endpoint change: %.5f in smooth-component units; the two bands are very similar in this example.",
        max_change), sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 20)),
    strip.text = element_text(face = "bold", size = 13, margin = margin(b = 20)),
    panel.spacing.x = grid::unit(2.5, "lines"),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(12, 24, 12, 24))

built <- ggplot_build(p)
stopifnot(length(built$layout$panel_params) == 2L,
  identical(built$layout$panel_params[[1L]], built$layout$panel_params[[2L]]))
ggsave(file.path(output, "surface-3d-comparison.png"), p,
  width = 15, height = 8.5, dpi = 160)
saveRDS(p, file.path(artifact_dir, "surface-3d-comparison-plot.rds"))
print(limits)
cat("Largest endpoint change:", format(max_change, digits = 10), "\n")

# Faceting keeps both panels synchronized in each of the 24 rotation views.
orbit_3d(p, yaw = c(325, -35), n = 24, start = -35,
  width = 1600, height = 900, res = 110, cores = 1, progress = FALSE,
  file = file.path(artifact_dir, "surface-3d-comparison.html"))
