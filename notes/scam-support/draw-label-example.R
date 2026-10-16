library("mgcv")
library("scam")
pkgload::load_all(quiet = TRUE)

set.seed(42)
d <- data.frame(x = runif(160))
d$y <- exp(d$x) + rnorm(nrow(d), sd = 0.15)
m <- scam(y ~ s(x, bs = "mpi", k = 8), data = d)
p <- draw(m)

ragg::agg_png("notes/scam-support/results/draw-label-example.png",
  width = 1000, height = 700, res = 144)
print(p)
grDevices::dev.off()
