library("mgcv")
library("scam")
pkgload::load_all(quiet = TRUE)

set.seed(502)
d <- data.frame(age = runif(180), dose = runif(180))
d$y <- 2 + exp(d$age) + 0.3 * d$dose + rnorm(nrow(d), sd = 0.08)
endpoint <- scam(y ~ s(age, bs = "mifo", k = 7), data = d,
  not.exp = TRUE)
local_data <- d
local_data$y <- 2 + exp(pmin(d$age, 0.55)) + rnorm(nrow(d), sd = 0.08)
plateau <- scam(y ~ s(age, bs = "lipl", k = 12, xt = list(xc = 0.55)),
  data = local_data, not.exp = TRUE)
surface <- scam(y ~ s(age, dose, bs = "temicx", k = c(5, 5)),
  data = d, not.exp = TRUE)
interaction <- scam(y ~ s(age, bs = "mpi", k = 5) +
  s(dose, bs = "ps", k = 5) +
  s(age, dose, bs = "tismi", k = c(5, 5)), data = d, not.exp = TRUE)
plots <- list(draw(endpoint), draw(plateau), draw(surface),
  draw(interaction, select = 3))
ragg::agg_png("notes/scam-support/results/caption-examples.png",
  width = 1800, height = 1250, res = 144)
print(patchwork::wrap_plots(plots, ncol = 2))
grDevices::dev.off()
