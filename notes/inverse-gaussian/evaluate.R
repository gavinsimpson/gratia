# Run from the repository root with Rscript notes/inverse-gaussian/evaluate.R.
# statmod is optional and used only for comparison, never by gratia's helpers.
pkgload::load_all(quiet = TRUE)
options(digits = 17)
cat(R.version.string, "\n")
cat("mgcv", as.character(utils::packageVersion("mgcv")), "\n")

original <- new.env(parent = globalenv())
eval(parse(text = system2("git", c("show", "main:R/fix-family.R"),
  stdout = TRUE)), envir = original)
cat("\nOriginal CDF boundaries:\n")
print(suppressWarnings(original$cdf_invgaussian(c(-1, 0, 1, 2, Inf, NA),
  1.5, 1, .7)))
cat("Original CDF at mean, dispersion 1e-20:\n")
print(original$cdf_invgaussian(1.5, 1.5, 1, 1e-20))
cat("Original and native weighted CDF:\n")
print(data.frame(wt = c(.25, 1, 4),
  original = original$cdf_invgaussian(1, 1.5, c(.25, 1, 4), .7),
  native = cdf_invgaussian(1, 1.5, c(.25, 1, 4), .7)))
cat("Original family CDF and QF unavailable:\n")
print(c(cdf = is.null(original$fix_family_cdf(stats::inverse.gaussian())$cdf),
  qf = is.null(original$fix_family_qf(stats::inverse.gaussian())$qf)))

cat("\nNative extreme log probabilities:\n")
for (tail in c(TRUE, FALSE)) {
  logp <- c(-1e-100, -100, -700, -1000, -10000)
  q <- qf_invgaussian(logp, 1.5, 1, .7, log_p = TRUE, lower_tail = tail)
  back <- cdf_invgaussian(q, 1.5, 1, .7, log_p = TRUE, lower_tail = tail)
  print(data.frame(lower_tail = tail, logp, q, back))
}

g <- expand.grid(mu = c(.01, 1.5, 100), phi = c(1e-12, 1e-6, .7, 100,
  1e12, 1e20), ratio = c(.001, .01, .1, .5, .99), wt = c(.25, 4))
actual <- logspace_add(
  cdf_invgaussian(g$mu * g$ratio, g$mu, g$wt, g$phi, log_p = TRUE),
  cdf_invgaussian(g$mu / g$ratio, g$mu, g$wt, g$phi,
    lower_tail = FALSE, log_p = TRUE))
expected <- stats::pchisq((g$ratio - 1)^2 /
  (g$phi / g$wt * g$mu * g$ratio), 1, lower.tail = FALSE, log.p = TRUE)
error <- max(abs(actual - expected) / pmax(1, abs(expected)))
cat("\nChi-square identity maximum scaled log error:", error, "\n")
stopifnot(error < 2e-13)

set.seed(804)
n <- 2000
mu <- exp(runif(n, -300, 300))
phi <- exp(runif(n, -300, 300))
q <- mu * exp(runif(n, -20, 20))
lower <- cdf_invgaussian(q, mu, 1, phi, log_p = TRUE)
upper <- cdf_invgaussian(q, mu, 1, phi, lower_tail = FALSE, log_p = TRUE)
stopifnot(all(!is.na(lower) & lower <= 0 & !is.na(upper) & upper <= 0))
cat("Random CDF maximum complement error:",
  max(abs(exp(lower) + exp(upper) - 1)), "\n")
logp <- -exp(runif(n, -5, log(10000)))
for (tail in c(TRUE, FALSE)) {
  q <- qf_invgaussian(logp, mu, 1, phi, log_p = TRUE, lower_tail = tail)
  stopifnot(all(!is.na(q) & q > 0))
  cat("Random quantile stress cases passed, lower_tail =", tail, ":", n,
    "; output overflows:", sum(q == Inf), "\n")
}

cat("\nNative random generator moment and probability audit:\n")
set.seed(639)
n <- 40000L
p <- c(.01, .1, .5, .9, .99)
g <- expand.grid(mu = c(.2, 2, 5), wt = c(.25, 1, 4))
results <- lapply(seq_len(nrow(g)), function(i) {
  mu <- g$mu[i]; wt <- g$wt[i]; phi <- .3 / wt
  y <- rd_invgaussian(rep(mu, n), wt, .3)
  variance <- phi * mu^3
  q <- qf_invgaussian(p, mu, wt, .3)
  empirical <- vapply(q, function(x) mean(y <= x), numeric(1))
  data.frame(mu, wt, mean = mean(y), variance = var(y),
    mean_error_se = (mean(y) - mu) / sqrt(variance / n),
    variance_error_se = (var(y) - variance) /
      (variance * sqrt((2 + 15 * phi * mu) / n)),
    max_probability_error_se = max(abs(empirical - p) / sqrt(p * (1 - p) / n)))
})
results <- do.call(rbind, results)
print(results)
stopifnot(all(abs(results$mean_error_se) < 6),
  all(abs(results$variance_error_se) < 6),
  all(results$max_probability_error_se < 6))

if (requireNamespace("statmod", quietly = TRUE)) {
  cat("\nOptional comparison with statmod",
    as.character(utils::packageVersion("statmod")), "\n")
  set.seed(438)
  n <- 3000
  mu <- exp(runif(n, -6, 6)); phi <- exp(runif(n, -8, 8))
  q <- mu * exp(runif(n, -5, 5))
  for (tail in c(TRUE, FALSE)) {
    actual <- cdf_invgaussian(q, mu, 1, phi, log_p = TRUE, lower_tail = tail)
    reference <- statmod::pinvgauss(q, mean = mu, dispersion = phi,
      log.p = TRUE, lower.tail = tail)
    cat("CDF maximum scaled log discrepancy, lower_tail =", tail, ":",
      max(abs(actual - reference) / pmax(1, abs(reference))), "\n")
  }
  p <- runif(n)
  elapsed <- system.time(q <- qf_invgaussian(p, mu, 1, phi))
  reference <- statmod::qinvgauss(p, mean = mu, dispersion = phi)
  cat("Quantile maximum relative discrepancy:", max(abs(q - reference) / reference),
    "\nNative elapsed seconds:", elapsed[["elapsed"]], "\n")
  q <- statmod::qinvgauss(-1000, mean = 1.5, dispersion = .7, log.p = TRUE)
  cat("Reference backend round trip for logp = -1000:",
    statmod::pinvgauss(q, mean = 1.5, dispersion = .7, log.p = TRUE), "\n")
}
