library("gratia")
library("mgcv")
ns <- asNamespace("gratia")
set.seed(281)
d <- data.frame(x = runif(180), z = runif(180))
d$y <- sin(2 * pi * d$x) + 0.6 * sin(2 * pi * d$z) + rnorm(180, sd = 0.4)
fits <- list(
  gam = gam(y ~ s(x, k = 6) + s(z, k = 5), data = d, method = "REML"),
  bam = bam(y ~ s(x, k = 6) + s(z, k = 5), data = d),
  bam_discrete = bam(y ~ s(x, k = 6) + s(z, k = 5), data = d, discrete = TRUE))
output <- list()
for (name in names(fits)) {
  m <- fits[[name]]
  info <- ns$sampling_smoothing_info(m)
  mixture <- tryCatch(ns$sampling_smoothing_mixture(m, n = 100, seed = 281),
    error = identity)
  fixed <- post_draws(m, n_draws = 10000, seed = 281)
  corrected <- post_draws(m, n_draws = 10000, seed = 281, unconditional = TRUE)
  output[[name]] <- list(
    method = info$method, sp = info$sp, V.sp.dim = dim(info$V.sp),
    spind = info$spind, sp.vcov.dim = dim(info$sp.vcov),
    db.drho.dim = dim(info$db.drho), Vc.dim = dim(info$Vc),
    mean_sd_fixed = mean(apply(fixed, 2, sd)),
    mean_sd_corrected = mean(apply(corrected, 2, sd)),
    mixture_status = if (inherits(mixture, "error")) conditionMessage(mixture) else "supported",
    mixture_failures = if (inherits(mixture, "error")) NA_integer_ else sum(!is.na(mixture$failures)),
    mean_sd_mixture = if (inherits(mixture, "error")) NA_real_ else
      mean(apply(mixture$bs, 2, sd, na.rm = TRUE)))
}
# Reconstruct a discrete prediction representation once from the fitted bases.
# This is a research adapter to mgcv internals, not a public sampling backend.
m <- fits$bam_discrete
body <- deparse(mgcv:::predict.bamd)
writeLines(body, file.path("/tmp", "gratia-predict-bamd.txt"))
cat(capture.output(str(output)), sep = "\n")
source("notes/posterior-chains/compressed.R")
set.seed(282)
large <- data.frame(x = runif(4000), z = runif(4000))
large$y <- rpois(4000, exp(sin(2 * pi * large$x) + large$z / 2))
results <- list()
for (form in list(y ~ s(x, k = 10) + s(z, k = 8),
                  y ~ te(x, z, k = c(5, 5)) + s(x, k = 6))) {
  m <- bam(form, data = large, family = poisson(), discrete = TRUE)
  target <- ns$sampling_target(m, compiled = TRUE)
  design <- compressed_design(m)
  beta <- coef(m) + 0.02
  compressed <- compressed_evaluate(design, target, beta)
  original <- ns$sampling_target_eval(target, beta, gradient = TRUE)
  Xd <- do.call(mgcv::Xbd, c(design, list(beta = diag(length(beta)))))
  # These checks isolate the representation/gradient calculation from the
  # approximation introduced by discretising covariate values.
  compressed_target <- target
  compressed_target$X <- Xd
  exact_compressed <- ns$sampling_target_eval(compressed_target, beta, gradient = TRUE)
  stopifnot(isTRUE(all.equal(compressed$value, exact_compressed$value, tolerance = 1e-9)),
    isTRUE(all.equal(compressed$gradient, exact_compressed$gradient, tolerance = 1e-9)))
  results[[length(results) + 1L]] <- data.frame(coefficients = length(beta),
    max_eta_difference = max(abs(compressed$eta - drop(target$X %*% beta))),
    log_density_difference = compressed$value - original$value,
    max_gradient_difference = max(abs(compressed$gradient - original$gradient)),
    dense_bytes = as.numeric(object.size(target$X)),
    compressed_bytes = as.numeric(object.size(design)))
}
write.csv(do.call(rbind, results), "notes/posterior-chains/compressed.csv", row.names = FALSE)
