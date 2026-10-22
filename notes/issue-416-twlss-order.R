# Reproduce from the repository root with:
# Rscript notes/issue-416-twlss-order.R
#
# Issue: https://github.com/gavinsimpson/gratia/issues/416
# The likelihood in mgcv::twlss() constructs mu from predictor 1, theta from
# predictor 2, and rho from predictor 3, then calls ldTweedie(). That function
# sets p = a + (b - a) * plogis(theta) and phi = exp(rho).
# Hence the formula order is mean, power, dispersion.
#
# This follows from the density calculation itself: for y > 0 its part
# involving mu is (y * mu^(1 - p) / (1 - p) - mu^(2 - p) / (2 - p)) / phi.
# Thus p controls the exponents and phi divides this expression. For y = 0,
# log P(Y = 0) = -mu^(2 - p) / (phi * (2 - p)); checked numerically below.
# In rTweedie(), the Poisson rate is mu^(2 - p) / ((2 - p) * phi), and
# Gamma jumps have shape (2 - p) / (p - 1) and scale phi * (p - 1) * mu^(p - 1).
# Their compound Poisson mean is mu and variance is phi * mu^p.
#
# Source for mgcv 1.9-4:
# https://github.com/cran/mgcv/blob/1.9-4/R/gamlss.r#L2566-L2579
# https://github.com/cran/mgcv/blob/1.9-4/R/gam.fit3.r#L2838-L2854
# https://github.com/cran/mgcv/blob/1.9-4/R/gam.fit3.r#L2974-L2980
# https://github.com/cran/mgcv/blob/1.9-4/R/gam.fit3.r#L3112-L3131

library("mgcv")

set.seed(416)
n <- 10000L
p <- 1.1
phi <- 5
a <- 1.01
b <- 1.99
dat <- data.frame(x = runif(n))
dat$mu <- exp(2 + sin(2 * pi * dat$x))
dat$y <- rTweedie(dat$mu, p = p, phi = phi)

# Only mu depends on x. Conditional variance is phi * mu^p.
# Both fits estimate p and phi; neither is supplied the generating values.
# Their REML estimation procedures differ: recovering the same generating
# values does not imply that the finite-sample estimators are equivalent.
# See notes/issue-416-estimation.R for a comparison.
m_tw <- gam(y ~ s(x), data = dat, family = tw(a = a, b = b),
  method = "REML")
m_twlss <- gam(list(y ~ s(x), ~ 1, ~ 1), data = dat,
  family = twlss(a = a, b = b), method = "REML")

eta <- predict(m_twlss, type = "link")
p_hat <- a + (b - a) * plogis(eta[, 2L])
phi_hat <- exp(eta[, 3L])
mu_hat <- exp(eta[, 1L])

estimates <- data.frame(
  model = c("Generating values", "tw()", "twlss()"),
  p = c(p, m_tw$family$getTheta(trans = TRUE), p_hat[1L]),
  phi = c(phi, m_tw$sig2, phi_hat[1L])
)
cat("R:", R.version.string, "\n")
cat("mgcv:", as.character(packageVersion("mgcv")), "\n\n")
print(estimates, row.names = FALSE, digits = 8)
cat("\nSecond and third intercepts, before transformation:\n")
print(coef(m_twlss)[c("(Intercept).1", "(Intercept).2")], digits = 8)
cat("\nRanges across observations:\n")
print(rbind(mu = range(mu_hat), p = range(p_hat), phi = range(phi_hat)),
  digits = 8)

# Verify the interpretation against the actual family likelihood.
X <- predict(m_twlss, type = "lpmatrix")
family_log_lik <- m_twlss$family$ll(y = dat$y, X = X,
  coef = coef(m_twlss), wt = m_twlss$prior.weights,
  family = m_twlss$family)$l
direct_log_lik <- sum(ldTweedie(dat$y, mu = mu_hat,
  p = p_hat, phi = phi_hat)[, 1L])

# An independent mathematical check: for a compound Poisson Tweedie,
# P(Y = 0) = exp(-mu^(2 - p) / (phi * (2 - p))).
# This tests what p and phi do in the likelihood, beyond variable names.
zero_log_prob <- ldTweedie(rep(0, n), mu = mu_hat, p = NA, phi = NA,
  theta = eta[, 2L], rho = eta[, 3L], a = a, b = b)[, 1L]
zero_log_prob_formula <- -mu_hat^(2 - p_hat) / (phi_hat * (2 - p_hat))
checks <- data.frame(
  check = c("Family likelihood minus direct likelihood",
    "Largest error in log probability of zero",
    "Range of fitted p", "Range of fitted phi"),
  difference = c(family_log_lik - direct_log_lik,
    max(abs(zero_log_prob - zero_log_prob_formula)),
    diff(range(p_hat)), diff(range(phi_hat)))
)
cat("\nChecks of the parameter interpretation:\n")
print(checks, row.names = FALSE, digits = 8)
cat("\nConvergence:\n")
print(list(tw = m_tw$outer.info$conv, twlss = m_twlss$outer.info$conv))

stopifnot(
  isTRUE(all.equal(family_log_lik, direct_log_lik, tolerance = 1e-10)),
  max(abs(zero_log_prob - zero_log_prob_formula)) < 1e-10,
  diff(range(p_hat)) < 1e-12,
  diff(range(phi_hat)) < 1e-12
)

out_dir <- "notes/issue-416-results"
dir.create(out_dir, showWarnings = FALSE)
write.csv(estimates, file.path(out_dir, "estimates.csv"), row.names = FALSE)
write.csv(checks, file.path(out_dir, "checks.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session-info.txt"))

# Plot the recovered mean and the two constant parameters in their own units.
grid <- data.frame(x = seq(0, 1, length.out = 200L))
grid$mu <- exp(2 + sin(2 * pi * grid$x))
eta_grid <- predict(m_twlss, newdata = grid, type = "link")
tw_mu <- predict(m_tw, newdata = grid, type = "response")
tw_p <- rep(estimates$p[2L], nrow(grid))
tw_phi <- rep(estimates$phi[2L], nrow(grid))
twlss_mu <- exp(eta_grid[, 1L])
twlss_p <- a + (b - a) * plogis(eta_grid[, 2L])
twlss_phi <- exp(eta_grid[, 3L])

png(file.path(out_dir, "parameter-recovery.png"),
  width = 1500, height = 550, res = 150)
par(mfrow = c(1, 3), mar = c(4, 4, 1, 1), las = 1)
for (parameter in c("mu", "p", "phi")) {
  truth <- switch(parameter, mu = grid$mu, p = rep(p, nrow(grid)),
    phi = rep(phi, nrow(grid)))
  fit_tw <- switch(parameter, mu = tw_mu, p = tw_p, phi = tw_phi)
  fit_twlss <- switch(parameter, mu = twlss_mu, p = twlss_p, phi = twlss_phi)
  label <- switch(parameter, mu = expression(mu), p = expression(p),
    phi = expression(phi))
  plot(grid$x, truth, type = "l", xlab = "x", ylab = label,
    ylim = range(truth, fit_tw, fit_twlss), lwd = 2)
  lines(grid$x, fit_tw, col = "#0072B2", lty = 2, lwd = 2)
  lines(grid$x, fit_twlss, col = "#D55E00", lty = 3, lwd = 2)
  if (parameter == "mu") {
    legend("topright", c("Truth", "tw()", "twlss()"),
      col = c("black", "#0072B2", "#D55E00"), lty = 1:3,
      lwd = 2, bty = "n", cex = 0.85)
  }
}
invisible(dev.off())
