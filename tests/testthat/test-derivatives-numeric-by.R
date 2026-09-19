test_that("numeric-by derivatives hold the multiplier fixed", {
  withr::local_seed(3003)
  d <- data.frame(x = runif(240), z = runif(240, 0.5, 2.5), w = runif(240))
  d$y <- d$z * sin(2 * pi * d$x) + cos(3 * d$w) + rnorm(240, sd = 0.1)
  model <- mgcv::gam(y ~ s(x, by = z, k = 8) + s(w, k = 6),
    data = d, method = "REML")
  supplied <- expand.grid(x = seq(0.2, 0.8, length.out = 5), z = c(0, 1, 2))
  supplied$w <- 0.5
  h <- 1e-4 # avoid cancellation in second differences while keeping steps small
  level <- 0.8

  # Build an independent prediction-matrix difference, perturbing only the
  # target covariate and retaining only the target smooth's coefficients.
  reference <- function(data, id, type, order) {
    sm <- model$smooth[[id]]
    focal <- sm$term
    stencil <- if (order == 1L) {
      switch(type, forward = c(0, 1), backward = c(-1, 0),
        central = c(-0.5, 0.5))
    } else {
      switch(type, forward = c(0, 1, 2), backward = c(-2, -1, 0),
        central = c(-1, 0, 1))
    }
    weights <- if (order == 1L) c(-1, 1) else c(1, -2, 1)
    matrices <- lapply(seq_along(stencil), function(i) {
      nd <- data
      nd[[focal]] <- data[[focal]] + stencil[i] * h
      weights[i] * predict(model, newdata = nd, type = "lpmatrix")
    })
    cols <- sm$first.para:sm$last.para
    X <- (Reduce(`+`, matrices) / h^order)[, cols, drop = FALSE]
    list(estimate = drop(X %*% coef(model)[cols]),
      se = sqrt(rowSums((X %*% vcov(model)[cols, cols]) * X)))
  }

  for (order in 1:2) {
    for (type in c("forward", "backward", "central")) {
      for (generated in c(FALSE, TRUE)) {
        result <- derivatives(model,
          data = if (generated) NULL else supplied,
          n = 7, order = order, type = type, eps = h, level = level)
        # Both smooths must use their own covariate, including when generating
        # grids separately in successive iterations of the smooth loop.
        for (id in seq_along(model$smooth)) {
          sm <- model$smooth[[id]]
          actual <- result[result$.smooth == sm$label, ]
          expect_gt(nrow(actual), 0L)
          nd <- data.frame(x = rep(0.5, nrow(actual)), z = 1, w = 0.5)
          nd[[sm$term]] <- actual[[sm$term]]
          if (sm$by == "z") {
            nd$z <- actual$z
          }
          expected <- reference(nd, id, type, order)
          # Equivalent matrix operations can differ slightly through rounding.
          expect_equal(actual$.derivative, expected$estimate, tolerance = 1e-6)
          expect_equal(actual$.se, expected$se, tolerance = 1e-6)
          critical <- qnorm((1 + level) / 2)
          expect_equal(actual$.lower_ci, expected$estimate - critical * expected$se,
            tolerance = 1e-6)
          expect_equal(actual$.upper_ci, expected$estimate + critical * expected$se,
            tolerance = 1e-6)
          if (!generated && sm$by == "z") {
            zero <- actual$z == 0
            expect_equal(unname(actual$.derivative[zero]), rep(0, sum(zero)))
            expect_equal(unname(actual$.se[zero]), rep(0, sum(zero)))
            expect_equal(unname(actual$.derivative[actual$z == 2]),
              2 * unname(actual$.derivative[actual$z == 1]), tolerance = 1e-6)
          }
        }
      }
    }
  }
})
