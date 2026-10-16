# Run from the package root after pkgload::load_all().
library("mgcv")
library("gratia")
set.seed(183)
n <- 600L
dat <- data.frame(x = runif(n, -2, 2), z = runif(n), off = rnorm(n, sd = .15))
locations <- cbind(0, .3 + sin(dat$x) + dat$off, -.4 + dat$z - dat$x / 2)
utilities <- locations - log(-log(matrix(runif(n * 3L), n)))
dat$y <- max.col(utilities) - 1L
fit <- gam(list(y ~ s(x, k = 6) + offset(off), ~ z + x),
  family = multinom(K = 2), data = dat, method = "REML")
ordinary <- appraise(fit, type = "surrogate", seed = 183, scalable = FALSE)
scalable <- appraise(fit, type = "surrogate", seed = 183, scalable = TRUE,
  qq_bins = 100L, density_bins = 30L)
worm <- appraise(fit, type = "surrogate", seed = 183, scalable = TRUE,
  use_worm = TRUE, qq_bins = 100L, density_bins = 30L)
for (name in c("ordinary", "scalable", "worm"))
  ggplot2::ggsave(file.path("notes/multinom-surrogate", paste0(name, ".png")),
    get(name), width = 15, height = 9, dpi = 120)
# The public result is a wide utility matrix. Chunking is internal and must
# leave the complete draws unchanged, including the caller's RNG state.
mu <- gratia:::multinom_utility_locations(fit)
y <- fit$y
chunk_one <- withr::with_seed(183, gratia:::multinom_surrogate_draw(mu, y, 1L))
chunk_large <- withr::with_seed(183, gratia:::multinom_surrogate_draw(mu, y, 10000L))
stopifnot(identical(chunk_one, chunk_large))
# A larger example records working-memory and displayed-summary sizes.
large_mu <- mu[rep(seq_len(n), 200L), , drop = FALSE]
large_y <- rep(y, 200L)
timing <- system.time(large <- withr::with_seed(183,
  gratia:::multinom_surrogate_draw(large_mu, large_y)))
cat("Observations:", nrow(large_mu), "\nComponents:", ncol(large_mu),
  "\nResidual matrix bytes:", as.numeric(object.size(large)),
  "\nElapsed seconds:", timing[["elapsed"]], "\n")
