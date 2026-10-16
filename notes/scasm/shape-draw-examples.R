# Run from the repository root: Rscript notes/scasm/shape-draw-examples.R
# Load this feature branch, then show unmodified public draw() output.
pkgload::load_all(quiet = TRUE)
library("mgcv")

# Recreate the increasing and convex fits from shape-examples.R exactly.
set.seed(1801)
d <- data.frame(x = runif(160), z = runif(160))
d$y <- 0.4 * d$x + rnorm(nrow(d), sd = 0.3)
increasing <- scasm(y ~ s(x, bs = "sc", xt = "m+", k = 8), data = d)

set.seed(1806)
d <- data.frame(x = runif(160))
d$y <- 0.3 * d$x + 0.3 * d$x^2 + rnorm(nrow(d), sd = 0.3)
convex <- scasm(y ~ s(x, bs = "sc", xt = "c+", k = 8), data = d)

output <- file.path("notes", "scasm", "shape-examples")
for (name in c("increasing", "convex")) {
  model <- get(name)
  p <- draw(model, interval = "simultaneous", interval_method = "shape",
    n_sim = 5000, seed = 1809)
  ggplot2::ggsave(file.path(output, paste0("native-draw-", name, ".png")), p,
    width = 10.5, height = 5, dpi = 160)

  # Check that the native ribbon actually uses the stored shape-enforced bounds.
  estimates <- smooth_estimates(model, interval = "simultaneous",
    interval_method = "shape", n_sim = 5000, seed = 1809)
  built <- ggplot2::ggplot_build(p[[1]])
  ribbon <- which(vapply(p[[1]]$layers, function(layer)
    inherits(layer$geom, "GeomRibbon"), logical(1)))
  stopifnot(length(ribbon) == 1L,
    isTRUE(all.equal(built$data[[ribbon]]$ymin, estimates$.lower_ci)),
    isTRUE(all.equal(built$data[[ribbon]]$ymax, estimates$.upper_ci)))
  cat(name, ": draw() ribbon matches computed shape-enforced bounds\n")
}
