skip_if_not_installed("GJRM", minimum_version = "0.2-6")

test_that("Cholesky dependence smooths retain their fitted parameter meaning", {
  s <- mgcv::s
  d <- withr::with_seed(2086, {
    n <- 150L
    d <- data.frame(x = runif(n), z = runif(n))
    e <- matrix(rnorm(3 * n), ncol = 3)
    d$y1 <- as.numeric(sin(4 * d$x) + e[, 1] > 0)
    d$y2 <- as.numeric(cos(4 * d$x) + .2 * e[, 1] + e[, 2] > 0)
    d$y3 <- as.numeric(d$x + .2 * e[, 2] + e[, 3] > 0)
    d
  })
  model <- withr::with_preserve_seed(do.call(GJRM::gjrm, list(
    formula = list(y1 ~ s(x, k = 4), y2 ~ s(x, k = 4),
      y3 ~ s(x, k = 4), ~ s(z, k = 4), ~ 1, ~ 1), data = d,
    margins = rep("probit", 3), model = "T", Chol = TRUE)))
  estimates <- smooth_estimates(model, select = 4, data = d[1:5, ],
    overall_uncertainty = FALSE)
  expect_identical(unique(estimates$.model), "Cholesky parameter 1")
  expect_identical(vapply(4:6, function(j) gjrm_predictor_label(model, j), character(1)),
    paste("Cholesky parameter", 1:3))
  sm <- model$gam4$smooth[[1]]
  X <- mgcv::PredictMat(sm, d[1:5, ])
  offset <- model$X1.d2 + model$X2.d2 + model$X3.d2
  index <- offset + sm$first.para:sm$last.para
  expect_equal(estimates$.estimate, drop(X %*% model$coefficients[index]))
  expect_equal(estimates$.se,
    sqrt(rowSums((X %*% model$Vb[index, index]) * X)))
})

test_that("unrecognised joint margin roles use equation labels", {
  model <- list(margins = c("future_family", "N"), univar.gamlss = FALSE)
  expect_identical(gjrm_predictor_label(model, 3), "Linear predictor 3")
})

test_that("fixed-margin copulas retain the dependence equation label", {
  for (margins in list(c("N", "N"), c("P", "P"))) {
    model <- list(margins = margins, fixed.margins = TRUE, copula = "T",
      univar.gamlss = FALSE)
    expect_identical(gjrm_predictor_label(model, 5), "Dependence (theta)")
  }
})
