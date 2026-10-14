# Independent calculations for the worked documentation examples.
# Run from the repository root, outside the sandbox as required by AGENTS.md.
pkgload::load_all(".", quiet = TRUE)
library("mgcv")
library("gratia")

errors <- numeric()
record <- function(name, actual, expected, tolerance = 1e-9) {
  error <- max(abs(as.numeric(as.matrix(actual)) - as.numeric(as.matrix(expected))), na.rm = TRUE)
  stopifnot(is.finite(error), error < tolerance)
  errors[name] <<- error
}

set.seed(241)
survey <- data.frame(woodland = runif(600),
  source = factor(rep(c("Detection", "Count"), each = 300),
    levels = c("Detection", "Count")))
survey$family_id <- as.integer(survey$source)
eta <- ifelse(survey$source == "Detection", -1.3, 0.2) +
  1.8 * sin(pi * survey$woodland) - 0.8 * survey$woodland
survey$observed <- ifelse(survey$source == "Detection",
  rbinom(600, 1, plogis(eta)), rpois(600, exp(eta)))
joint <- gam(cbind(observed, family_id) ~ source + s(woodland, k = 10),
  data = survey, family = gfam(list(binomial(), poisson())), method = "REML")
grid <- expand.grid(woodland = seq(0, 1, length.out = 80),
  source = levels(survey$source))
grid$source <- factor(grid$source, levels = levels(survey$source))
index <- as.integer(grid$source)
X <- predict(joint, newdata = grid, type = "lpmatrix")
eta <- as.vector(X %*% coef(joint))
expected_mean <- ifelse(index == 1, plogis(eta), exp(eta))
fixed <- fitted_samples(joint, data = grid, family_index = index,
  method = "user", draws = matrix(coef(joint), nrow = 1))
record("gfam response means from direct inverse links", fixed$.fitted, expected_mean)
record("gfam native response means", predict(joint,
  newdata = transform(grid, observed = 0, family_id = index), type = "response"), expected_mean)
set.seed(243)
manual <- replicate(20, {
  out <- numeric(length(index))
  out[index == 1] <- rbinom(sum(index == 1), 1, plogis(eta[index == 1]))
  out[index == 2] <- rpois(sum(index == 2), exp(eta[index == 2]))
  out
})
record("gfam component response generators", simulate(joint, data = grid,
  family_index = index, nsim = 20, seed = 243), manual)

set.seed(301)
insects <- data.frame(temperature = runif(12000, 5, 30), wind = runif(12000, 0, 10))
insects$count <- rpois(nrow(insects),
  exp(2.5 - ((insects$temperature - 19) / 8)^2 - 0.1 * insects$wind))
count_fit <- bam(count ~ s(temperature, bs = "cr", k = 12) + wind,
  data = insects, family = poisson(), discrete = TRUE)
mu <- fitted(count_fit)
set.seed(302)
manual_pit <- runif(length(mu), ppois(insects$count - 1, mu), ppois(insects$count, mu))
record("Poisson PIT from native CDF", quantile_residuals(count_fit, "pit", seed = 302), manual_pit)
record("Poisson quantile scores from native CDF", quantile_residuals(count_fit,
  "quantile", seed = 302), qnorm(manual_pit), tolerance = 1e-7)
stopifnot(sum(scalable_residuals_linpred_plot(count_fit, type = "quantile",
  seed = 302)$data$count) == nrow(insects))

library("nlme")
data("Orthodont", package = "nlme")
growth <- transform(Orthodont, age_c = age - 11)
growth_fit <- lme(distance ~ Sex * age_c, random = ~ age_c | Subject,
  data = growth, method = "REML")
record("lme conditional Gaussian CDF", quantile_residuals(growth_fit,
  "quantile", conditioning = "conditional"),
  qnorm(pnorm(growth$distance, fitted(growth_fit), growth_fit$sigma)), tolerance = 1e-7)

set.seed(308)
monitor <- expand.grid(time = seq_len(120), station = factor(seq_len(4)))
monitor$start <- monitor$time == 1
monitor$measurement <- 2 + sin(monitor$time / 25) +
  unlist(lapply(seq_len(4), function(i)
    as.numeric(arima.sim(list(ar = 0.6), n = 120, sd = 0.5))))
serial_fit <- bam(measurement ~ s(time, bs = "cr", k = 12), data = monitor,
  rho = 0.6, AR.start = start, discrete = TRUE)
response <- as.matrix(simulate(serial_fit, nsim = 2000, seed = 502))
noise <- response - fitted(serial_fit)
correlation <- cor(t(noise[1:3, ]))
stopifnot(max(abs(correlation - outer(1:3, 1:3,
  function(i, j) 0.6^abs(i - j)))) < 0.04)
stopifnot(abs(cor(noise[120, ], noise[121, ])) < 0.05)
set.seed(310)
monitor$count <- rpois(nrow(monitor), exp(1 + monitor$measurement / 2))
working_fit <- bam(count ~ s(time, bs = "cr", k = 12), data = monitor,
  family = poisson(), discrete = TRUE, rho = 0.6, AR.start = start)
working_qq <- qq_plot(working_fit, type = "quantile", seed = 311)
stopifnot(!any(c("lower", "upper") %in% names(working_qq$data)))
for (f in list(function() simulate(working_fit),
  function() qq_plot(working_fit, method = "simulate"),
  function() posterior_predictive_plot(working_fit))) {
  stopifnot(inherits(tryCatch(f(), error = identity), "error"))
}

set.seed(405)
chemical <- data.frame(flow = runif(400, 0, 10),
  precision = rep(c(1, 2, 4, 8), length.out = 400))
chemical$concentration <- rgamma(400, shape = chemical$precision / 0.4,
  scale = exp(1 + 0.5 * sin(chemical$flow / 3)) * 0.4 / chemical$precision)
gamma_fit <- gam(concentration ~ s(flow, k = 10), data = chemical,
  weights = precision, family = Gamma(link = "log"), method = "REML")
phi <- summary(gamma_fit)$scale
record("weighted Gamma PIT from native CDF", quantile_residuals(gamma_fit,
  "pit", seed = 406), pgamma(chemical$concentration,
    shape = chemical$precision / phi, scale = fitted(gamma_fit) * phi / chemical$precision))

set.seed(411)
assay <- data.frame(dose = runif(300, 0, 4))
latent <- rnorm(300, 0.3 + 0.5 * assay$dose, 0.7)
assay$reading <- pmax(latent, 0.6)
assay$bound <- ifelse(latent < 0.6, -Inf, latent)
censored_fit <- gam(cbind(reading, bound) ~ s(dose, k = 8), data = assay,
  family = cnorm(), method = "REML")
sigma <- censored_fit$family$getTheta(TRUE)
set.seed(412)
manual <- runif(300, pnorm(pmin(assay$reading, assay$bound), fitted(censored_fit), sigma),
  pnorm(pmax(assay$reading, assay$bound), fitted(censored_fit), sigma))
record("censored normal PIT from native bounds", quantile_residuals(censored_fit,
  "pit", seed = 412), manual)

set.seed(413)
visits <- data.frame(cover = runif(240), trials = rep(c(5, 10, 20), 80))
visits$success <- rbinom(240, visits$trials, plogis(-1 + 2 * visits$cover))
visits$failure <- visits$trials - visits$success
visit_fit <- gam(cbind(success, failure) ~ s(cover, k = 8), data = visits,
  family = binomial(), method = "REML")
set.seed(414)
manual <- runif(240, pbinom(visits$success - 1, visits$trials, fitted(visit_fit)),
  pbinom(visits$success, visits$trials, fitted(visit_fit)))
record("binomial success-mass PIT", quantile_residuals(visit_fit,
  "pit", seed = 414), manual)

print(data.frame(comparison = names(errors), max_absolute_error = unname(errors)), row.names = FALSE)
cat("Gaussian AR1 covariance and series boundaries: passed\n")
cat("BAM working AR1: references omit bands; joint simulations/PPC reject: passed\n")
