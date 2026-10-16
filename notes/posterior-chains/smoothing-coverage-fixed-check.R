# Independent Gaussian check of the partly fixed fREML BAM dispersion convention.
# Run outside the sandbox from the repository root.
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
source("notes/posterior-chains/smoothing-coverage-functions.R")
spec <- subset(coverage_cases(), case == "normal_fixed")
fixture <- coverage_data(spec, 1L)
fit <- coverage_fit(spec, fixture, "bam", "fREML")
X <- predict(fit, newdata = fixture$data, type = "lpmatrix")
precision <- crossprod(X, X * fixture$data$w)
linear <- drop(crossprod(X, fixture$data$w * (fixture$data$y - fixture$data$o)))
multipliers <- fit$full.sp
stopifnot(length(multipliers) == 2L)
for (k in seq_along(fit$smooth)) {
  smooth <- fit$smooth[[k]]; j <- smooth$first.para:smooth$last.para
  precision[j, j] <- precision[j, j] + multipliers[k] * smooth$S[[1L]]
}
mean <- drop(solve(precision, linear)); covariance <- fit$sig2 * solve(precision)
reference <- mgcv::gam(fit$formula, data = fixture$data, weights = w,
  method = "REML", scale = fit$sig2, sp = unname(multipliers),
  knots = list(x = fixture$definition$knots, z = fixture$definition$knots))
evaluation <- fixture$evaluation
XB <- predict(fit, newdata = evaluation, type = "lpmatrix")
XG <- predict(reference, newdata = evaluation, type = "lpmatrix")
result <- data.frame(
  quantity = c("relative_coefficient_disagreement", "relative_covariance_disagreement",
    "maximum_native_penalised_gradient", "maximum_direct_vs_gam_prediction_error",
    "maximum_direct_vs_gam_prediction_covariance_error", "maximum_bam_vs_gam_prediction_error"),
  value = c(max(abs(mean - coef(fit))) / max(1, max(abs(coef(fit)))),
    max(abs(covariance - vcov(fit))) / max(abs(vcov(fit))),
    max(abs(precision %*% coef(fit) - linear)),
    max(abs(XB %*% mean - XG %*% coef(reference))),
    max(abs(XB %*% covariance %*% t(XB) - XG %*% vcov(reference) %*% t(XG))),
    max(abs(predict(fit, newdata = evaluation) - predict(reference, newdata = evaluation)))))
result <- rbind(result, data.frame(quantity = c("reported_dispersion", "native_covariance_implied_dispersion", "native_over_direct_covariance_scale"),
  value = c(fit$sig2, fit$sig2 * mean(diag(vcov(fit)) / diag(covariance)), mean(diag(vcov(fit)) / diag(covariance)))))
penalty <- precision - crossprod(X, X * fixture$data$w)
rank_penalty <- sum(eigen(penalty, symmetric = TRUE, only.values = TRUE)$values > 1e-8 * max(abs(penalty)))
residual <- fixture$data$y - fixture$data$o - drop(X %*% mean)
rss <- sum(fixture$data$w * residual^2)
profile_dispersion <- (rss + drop(crossprod(mean, penalty %*% mean))) / (nrow(X) - (ncol(X) - rank_penalty))
estimated_reference <- mgcv::gam(fit$formula, data = fixture$data, weights = w,
  method = "REML", sp = unname(multipliers),
  knots = list(x = fixture$definition$knots, z = fixture$definition$knots))
known_spec <- spec; known_spec$known_scale <- TRUE
known_fit <- coverage_fit(known_spec, fixture, "bam", "fREML")
known_setup <- get("sampling_smoothing_setup", asNamespace("gratia"))(known_fit)
result <- rbind(result, data.frame(quantity = c("profile_REML_dispersion", "Pearson_dispersion",
  "fixed_smoothing_gam_estimated_dispersion", "known_dispersion_setup_covariance_error"),
  value = c(profile_dispersion, rss / (nrow(X) - sum(fit$edf)), estimated_reference$sig2, known_setup$audit$covariance_error)))
stopifnot(abs(profile_dispersion - fit$sig2) < 1e-7,
  abs(rss / (nrow(X) - sum(fit$edf)) - fit$sig2 * mean(diag(vcov(fit)) / diag(covariance))) < 1e-7,
  abs(estimated_reference$sig2 - rss / (nrow(X) - sum(fit$edf))) < 1e-7,
  known_setup$audit$covariance_error < 1e-7)
estimated_setup <- get("sampling_smoothing_setup", asNamespace("gratia"))(fit)
result <- rbind(result, data.frame(quantity = c("corrected_setup_dispersion",
  "corrected_setup_covariance_error"),
  value = c(estimated_setup$phi, estimated_setup$audit$covariance_error)))
stopifnot(abs(estimated_setup$phi - rss / (nrow(X) - sum(fit$edf))) < 1e-7,
  identical(estimated_setup$audit$reported_dispersion, fit$sig2),
  estimated_setup$audit$covariance_error < 1e-7)
print(result, row.names = FALSE)
ratio <- diag(vcov(fit)) / diag(covariance)
cat("Native Vp/direct covariance diagonal ratio range:", range(ratio), "\n")
cat("sig2, scale, reml.scale:", fit$sig2, fit$scale, fit$reml.scale, "\n")
cat("Maximum departure from common covariance scaling:", max(abs(vcov(fit) - mean(ratio) * covariance)), "\n")
cat("Fitted full smoothing multipliers:\n"); print(multipliers)
stopifnot(result$value[4L] < 1e-7, result$value[5L] < 1e-7)
write.csv(result, "notes/posterior-chains/smoothing-coverage-fixed-check.csv", row.names = FALSE)
