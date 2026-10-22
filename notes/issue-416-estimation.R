# Compare estimation of constant power and dispersion in tw() and twlss().
# Run from the repository root: Rscript notes/issue-416-estimation.R
# Source: https://github.com/cran/mgcv/blob/1.9-4/R/gam.fit4.r
# gam.fit4() constructs the outer criterion at lines 734-758.
# gam.fit5() constructs the coefficient objective and gradient at lines 1003-1004.
library("mgcv")

set.seed(416)
dat <- data.frame(x = runif(10000))
dat$z <- sin(2 * pi * dat$x)
dat$mu <- exp(2 + dat$z)
dat$y <- rTweedie(dat$mu, p = 1.1, phi = 5)

# With REML, tw() estimates p and phi as outer parameters. twlss() estimates
# their intercepts as coefficients of the penalized likelihood, with smoothing
# parameters selected by REML. These are different estimation procedures.
tw_reml <- gam(y ~ s(x), data = dat, family = tw(), method = "REML")
lss_reml <- gam(list(y ~ s(x), ~ 1, ~ 1), data = dat,
  family = twlss(), method = "REML")

# Check the ordinary likelihood derivatives in the two nuisance directions,
# holding the fitted mean fixed. No smooth penalty involves these directions.
tw_derivs <- ldTweedie(dat$y, mu = fitted(tw_reml),
  theta = tw_reml$family$getTheta(), rho = log(tw_reml$sig2),
  a = 1.01, b = 1.99)
eta <- predict(lss_reml, type = "link")
lss_derivs <- ldTweedie(dat$y, mu = exp(eta[, 1L]),
  theta = eta[, 2L], rho = eta[, 3L], a = 1.01, b = 1.99)
scores <- data.frame(
  model = c("tw() REML", "twlss() REML"),
  d_log_lik_d_theta = c(sum(tw_derivs[, 4L]), sum(lss_derivs[, 4L])),
  d_log_lik_d_rho = c(sum(tw_derivs[, 2L]), sum(lss_derivs[, 2L]))
)
cat("Ordinary likelihood derivatives at the smooth-model estimates:\n")
print(scores, row.names = FALSE, digits = 10)
cat("\nOptimizers:\n")
print(list(tw = tw_reml$optimizer, twlss = lss_reml$optimizer))

# Remove smooths to compare ordinary maximum likelihood directly, without
# smoothing selection. The true log mean is linear in the known covariate z.
tw_linear_reml <- gam(y ~ z, data = dat, family = tw(), method = "REML")
tw_linear_ml <- gam(y ~ z, data = dat, family = tw(), method = "ML")
lss_linear <- gam(list(y ~ z, ~ 1, ~ 1), data = dat,
  family = twlss(), method = "REML")
eta_linear <- predict(lss_linear, type = "link")
estimates <- data.frame(
  model = c("tw() REML, no smooth", "tw() ML, no smooth", "twlss(), no smooth"),
  p = c(tw_linear_reml$family$getTheta(TRUE),
    tw_linear_ml$family$getTheta(TRUE),
    1.01 + .98 * plogis(eta_linear[1L, 2L])),
  phi = c(tw_linear_reml$sig2, tw_linear_ml$sig2, exp(eta_linear[1L, 3L]))
)
cat("\nEstimates without smooths:\n")
print(estimates, row.names = FALSE, digits = 10)
cat("\nLargest mean-coefficient difference between the ML fits:\n")
print(max(abs(coef(tw_linear_ml) - coef(lss_linear)[1:2])), digits = 10)

out_dir <- "notes/issue-416-results"
dir.create(out_dir, showWarnings = FALSE)
write.csv(scores, file.path(out_dir, "likelihood-scores.csv"), row.names = FALSE)
write.csv(estimates, file.path(out_dir, "estimation-comparison.csv"),
  row.names = FALSE)
