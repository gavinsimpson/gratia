# Run from the repository root with Rscript. This investigates the fitting
# packages directly, without loading gratia or changing their namespaces.
# Warnings are recorded here; the package regression tests require silent fits.
record_warnings <- function(code) {
  messages <- character()
  value <- withCallingHandlers(code, warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart('muffleWarning')
  })
  list(value = value, warnings = messages)
}
cat(R.version.string, '\n')
for (package in c('GJRM', 'gamlss.dist'))
  cat(package, as.character(utils::packageVersion(package)), '\n')

mu <- c(2, 1, 1)
sigma <- c(1, 0.00011, 0.00009)
pdf <- record_warnings(gamlss.dist::dNBII(0, mu, sigma))
cdf <- record_warnings(gamlss.dist::pNBII(0, mu, sigma))
cat('\nMixed dispersion at the Poisson boundary:\n')
print(data.frame(mu, sigma, pdf = pdf$value,
  exact_pdf = stats::dnbinom(0, mu = mu, size = mu / sigma),
  cdf = cdf$value, exact_cdf = stats::pnbinom(0, mu = mu, size = mu / sigma)))
print(list(density_warnings = pdf$warnings, cdf_warnings = cdf$warnings))

# GJRM also evaluates the corrupted density before gratia is involved.
# Compare against its own lower bound on the fitted dispersion.
evaluator <- get('distrHsDiscr', asNamespace('GJRM'))
result <- record_warnings(evaluator(rep(0, 3), log(mu), sigma, log(sigma),
  1, 0, 'NBII', naive = TRUE, min.dn = 1e-40, min.pr = 1e-16,
  max.pr = 0.9999999))
cat('\nGJRM likelihood evaluator with mixed dispersions:\n')
print(data.frame(reported_mass = as.vector(result$value$pdf2),
  exact_mass = stats::dnbinom(0, mu = mu, size = mu / pmax(sigma, 1e-4))))
cat('Warnings from the likelihood evaluator:', length(result$warnings), '\n')

# The old test generated responses between fits. Check whether fitting a
# second distributional predictor removes the seed controlling later responses.
normal <- withr::with_seed(27, {
  d <- data.frame(x = stats::runif(200, -1, 1))
  d$y <- GJRM::sim.resp('N', nrow(d), eta = d$x / 4, sigma2 = 0.5, nu = 1)
  before <- .Random.seed
  model <- GJRM::gamlss(list(y ~ x, ~x), data = d, family = 'N')
  list(seed_exists = exists('.Random.seed', .GlobalEnv, inherits = FALSE),
    seed_unchanged = identical(before, get0('.Random.seed', .GlobalEnv)))
})
cat('\nCaller RNG after the first fit in the old test:\n')
print(normal)

# A deterministic near-Poisson fit reproduces warnings during optimisation.
# Base-R quantiles supply the same NBII generator without calling its CDF.
boundary <- withr::with_seed(27, {
  d <- data.frame(x = stats::runif(200, -1, 1))
  mean <- exp(d$x / 4)
  d$y <- withr::with_seed(27,
    stats::qnbinom(stats::runif(nrow(d)), mu = mean, size = mean / 0.5))
  fit <- record_warnings(GJRM::gamlss(list(y ~ x, ~x), data = d, family = 'NBII'))
  mean <- as.vector(exp(fit$value$eta1))
  sigma <- as.vector(fit$value$sigma2)
  exact_pdf <- stats::dnbinom(d$y, mu = mean, size = mean / sigma)
  list(sigma_range = range(sigma), warning_count = length(fit$warnings),
    warning_messages = unique(fit$warnings),
    maximum_mass_error = max(abs(as.vector(fit$value$fit$pdf2) - exact_pdf)))
})
cat('\nIndependent GJRM NBII fit at the boundary:\n')
print(boundary)

# Generate all responses before any fit, as in the corrected regression test.
families <- c('N', 'LN', 'GA', 'GAi', 'P', 'NBI', 'NBII')
fixture <- function() withr::with_seed(27, {
  x <- stats::runif(200, -1, 1)
  responses <- setNames(lapply(families, function(fam)
    GJRM::sim.resp(fam, length(x),
      eta = if (fam == 'GAi') 2 + x / 4 else x / 4,
      sigma2 = 0.5, nu = 1)), families)
  list(x = x, responses = responses)
})
a <- fixture()
b <- fixture()
stopifnot(identical(a, b))
cat('\nFixed fixtures, with no suppressed warnings:\n')
for (fam in families) {
  d <- data.frame(x = a$x, y = a$responses[[fam]])
  model <- withr::with_preserve_seed(GJRM::gamlss(
    if (fam == 'P') list(y ~ x) else list(y ~ x, ~x), data = d,
    family = if (fam == 'GAi') 'GA' else fam))
  cat(fam, 'sigma range:', if (is.null(model$sigma2)) 'not applicable' else
    paste(signif(range(model$sigma2), 8), collapse = ' to '), '\n')
  if (fam == 'NBII') {
    mean <- as.vector(exp(model$eta1))
    error <- max(abs(as.vector(model$fit$pdf2) - stats::dnbinom(d$y,
      mu = mean, size = mean / as.vector(model$sigma2))))
    cat('Maximum analytic likelihood mass error:', error, '\n')
    stopifnot(error < 1e-6)
  }
}
