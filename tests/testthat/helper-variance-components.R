variance_terms_data <- function() {
  set.seed(184)
  n <- 240L
  d <- data.frame(x = runif(n), w = runif(n),
    fac = factor(rep(c("a", "b"), length.out = n)),
    group = factor(rep(seq_len(12), each = 20)))
  d$z <- 0.8 * d$x + runif(n, -0.15, 0.15)
  d$off <- runif(n, -0.5, 0.5)
  b <- matrix(rnorm(24, sd = 0.8), 12, 2)
  d$y <- 1.5 * d$x + d$z + sin(6 * d$w) + (d$fac == "b") +
    b[d$group, 1] + d$z * b[d$group, 2] + d$off + rnorm(n, sd = 0.25)
  d
}

# Recompute the native terms after replacing the full coefficient vector. Every
# term in a replicate therefore uses the same posterior coefficient draw.
reference_term_variance_draws <- function(model, n_draws, seed,
    unconditional = FALSE) {
  model <- model_used_rows(model)
  betas <- gaussian_draws(model, n_draws = n_draws, seed = seed,
    unconditional = unconditional, mvn_method = "mgcv")
  n_terms <- ncol(predict_model(model, type = "terms"))
  n_predictors <- ncol(as.matrix(predict_model(model, type = "link")))
  t(matrix(vapply(seq_len(n_draws), function(i) {
    draw <- model
    draw$coefficients <- betas[i, ]
    values <- predict_model(draw, type = "terms")
    link <- as.matrix(predict_model(draw, type = "link"))
    totals <- vapply(seq_len(n_predictors), function(j) {
      offset <- if (is.list(draw$offset)) draw$offset[j][[1L]] else draw$offset
      var(link[, j] - (offset %||% 0))
    }, numeric(1L))
    c(apply(values, 2L, var), totals)
  }, numeric(n_terms + n_predictors)), nrow = n_terms + n_predictors))
}
