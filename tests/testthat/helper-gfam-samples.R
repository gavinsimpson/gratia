gfam_test_draws <- function(model) {
  beta <- as.numeric(coef(model))
  rbind(beta, beta + seq(-0.15, 0.2, length.out = length(beta)), beta - 0.1)
}

# An independent oracle for the mixed binomial/Gaussian/Poisson fixtures.
# Never use grouped inverse-link closures or gratia's family-row helpers here.
gfam_test_eta <- function(model, data, draws) {
  X <- mgcv::predict.gam(model, newdata = data, type = "lpmatrix",
    newdata.guaranteed = TRUE)
  eta <- X %*% t(draws)
  offset <- attr(X, "model.offset")
  if (is.list(offset)) offset <- offset[[1L]]
  if (!is.null(offset)) eta <- eta + offset
  eta
}

gfam_test_means <- function(eta, index) {
  out <- eta
  out[index == 1L, ] <- plogis(eta[index == 1L, , drop = FALSE])
  out[index == 3L, ] <- exp(eta[index == 3L, , drop = FALSE])
  out
}

gfam_test_matrix <- function(samples) {
  matrix(samples$.fitted, ncol = length(unique(samples$.draw)))
}
