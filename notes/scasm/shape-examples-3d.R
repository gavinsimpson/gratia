# Run from the repository root: Rscript notes/scasm/shape-examples-3d.R
# Uses the saved tensor example without refitting or recalibrating its band.
library("ggplot2")
library("ggcube")

output <- file.path("notes", "scasm", "shape-examples")
grid <- utils::read.csv(file.path(output, "surface-grid.csv"))
stopifnot(nrow(grid) == 101L * 21L, !anyDuplicated(grid[c("x", "z")]),
  all(grid$shape_lower <= grid$shape_estimate),
  all(grid$shape_estimate <= grid$shape_upper))

surfaces <- do.call(rbind, lapply(c("Lower limit", "Fitted smooth", "Upper limit"),
  function(name) {
    column <- switch(name, "Lower limit" = "shape_lower",
      "Fitted smooth" = "shape_estimate", "Upper limit" = "shape_upper")
    data.frame(x = grid$x, z = grid$z, height = grid[[column]], surface = name)
  }))
surfaces$surface <- factor(surfaces$surface,
  levels = c("Lower limit", "Fitted smooth", "Upper limit"))

# One grouped surface layer lets the renderer order all three surfaces together.
p <- ggplot(surfaces, ggcube::aes(x = x, y = z, z = height,
    group = surface, fill = surface, alpha = surface)) +
  geom_surface_3d(colour = NA) +
  coord_3d(yaw = -35, roll = -60, persp = FALSE, ratio = c(1, 1, 0.85),
    light = light(backface_scale = 1, contrast = 0.4)) +
  scale_fill_manual(NULL, values = c("Lower limit" = "#C77B31",
    "Fitted smooth" = "#243447", "Upper limit" = "#087E8B")) +
  scale_alpha_manual(values = c("Lower limit" = 0.5,
    "Fitted smooth" = 0.8, "Upper limit" = 0.4), guide = "none") +
  scale_z_continuous(name = "Smooth component") +
  labs(x = "x", y = "z", title = "Tensor smooth and its interval surfaces",
    subtitle = "Shape-enforced limits from the same initial 95% Gaussian full-grid band",
    caption = paste("Increasing in x; unrestricted in z. All 101 x 21 evaluation points are retained.",
      "The vertical gap between the lower and upper surfaces is the interval width.", sep = "\n")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(12, 24, 12, 24))

ggsave(file.path(output, "surface-3d.png"), p, width = 10, height = 8.5, dpi = 160)
saveRDS(p, file.path(output, "surface-3d-plot.rds"))

# Drag horizontally, or use the arrow keys, to inspect the same three surfaces
# from 24 directions. Keep the full evaluation grid in every frame.
orbit_3d(p, yaw = c(325, -35), n = 24, start = -35,
  width = 1000, height = 850, res = 100, cores = 1, progress = FALSE,
  file = file.path(output, "surface-3d.html"))
