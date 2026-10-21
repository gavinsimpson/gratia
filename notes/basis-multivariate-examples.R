# Run from the repository root to use the implementation in this checkout.
library("mgcv")
pkgload::load_all(".", quiet = TRUE)

set.seed(258)
dat <- data.frame(
  x = runif(600), z = runif(600), w = runif(600),
  v = runif(600), u = runif(600), q = runif(600)
)
effect_3d <- 3 * sin(pi * dat$x) * cos(pi * dat$z) * (0.5 + dat$w)
effect_4d <- effect_3d * (0.5 + dat$v)
dat$y3 <- effect_3d + rnorm(nrow(dat), sd = 0.2)
dat$y4 <- effect_4d + rnorm(nrow(dat), sd = 0.2)
dat$y <- effect_3d + 2 * sin(2 * pi * dat$u) +
  3 * (dat$q - 0.5)^2 + rnorm(nrow(dat), sd = 0.2)

m3 <- gam(y3 ~ s(x, z, w, k = 20), data = dat, method = "REML")
m4 <- gam(y4 ~ s(x, z, w, v, k = 30), data = dat, method = "REML")
mixed <- gam(y ~ s(u, k = 8) + s(q, k = 8) + s(x, z, w, k = 20),
  data = dat, method = "REML")

# Extract all basis functions, then retain two per surface for these displays.
# Omitting the row subsets below draws every basis function.
bf3 <- basis(m3, variables = c("x", "z", "w"), n_2d = 40, n_3d = 3)
p3 <- draw(bf3[bf3$.bf %in% c("2", "3"), ], ncol = 1,
  legend = TRUE, guides = "collect")

bf4 <- basis(m4, variables = c("x", "z", "w", "v"),
  n_2d = 40, n_4d = 2)
p4 <- draw(bf4[bf4$.bf %in% c("2", "3"), ], ncol = 2,
  legend = TRUE, guides = "collect")

# Different smooths have different covariates, so use their default orders.
bf_mixed <- basis(mixed, n = 150, n_2d = 40, n_3d = 2)
keep <- bf_mixed$.smooth %in% c("s(u)", "s(q)") |
  bf_mixed$.bf %in% c("2", "3")
p_mixed <- draw(bf_mixed[keep, ], ncol = 2, legend = TRUE)

# Select just the 3D smooth from the same fitted model when needed.
bf_mixed_3d <- basis(mixed, select = "s(x,z,w)",
  variables = c("x", "z", "w"), n_2d = 40, n_3d = 2)
stopifnot(identical(unique(bf_mixed$.smooth),
  c("s(u)", "s(q)", "s(x,z,w)")))
stopifnot(identical(unique(bf_mixed_3d$.smooth), "s(x,z,w)"))
stopifnot(length(p3) == 2L, length(p4) == 2L, length(p_mixed) == 4L)

output_dir <- "notes/basis-multivariate-examples"
dir.create(output_dir, showWarnings = FALSE)
ggplot2::ggsave(file.path(output_dir, "basis-3d.png"), p3,
  device = ragg::agg_png, width = 14, height = 10, dpi = 120)
ggplot2::ggsave(file.path(output_dir, "basis-4d.png"), p4,
  device = ragg::agg_png, width = 14, height = 8.5, dpi = 120)
ggplot2::ggsave(file.path(output_dir, "basis-mixed.png"), p_mixed,
  device = ragg::agg_png, width = 14, height = 10, dpi = 120)
