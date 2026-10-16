offset_sampling_fixture <- local({
  models <- list()
  function(poisson = FALSE) {
    key <- if (poisson) "poisson" else "gaussian"
    if (is.null(models[[key]])) {
      bnd <- list(data.frame(x = cos(seq(0, 2 * pi, length.out = 61)),
        y = sin(seq(0, 2 * pi, length.out = 61)),
        f = cos(seq(0, 2 * pi, length.out = 61))))
      d <- withr::with_seed(25, {
        d <- data.frame(x = runif(160, -1, 1), y = runif(160, -1, 1))
        d <- d[mgcv::in.out(bnd, as.matrix(d)), ]
        eta <- d$x + d$y^2 + .2 * d$x
        d$z <- if (poisson) rpois(nrow(d), exp(eta)) else
          eta + rnorm(nrow(d), sd = .03)
        d
      })
      knots <- expand.grid(x = seq(-.8, .8, length.out = 5),
        y = seq(-.8, .8, length.out = 5))
      knots <- knots[mgcv::in.out(bnd, as.matrix(knots)), ]
      models[[key]] <<- mgcv::gam(z ~ s(x, y, bs = "so", k = 12,
        xt = list(bnd = bnd, nmax = 40)) + offset(.2 * x),
        data = d, knots = knots, method = "REML",
        family = if (poisson) stats::poisson() else gaussian())
    }
    models[[key]]
  }
})

test_that("fitted and smooth draws retain fixed boundaries once", {
  nd <- data.frame(x = seq(-.5, .5, length.out = 8), y = .2)
  for (poisson in c(FALSE, TRUE)) {
    m <- offset_sampling_fixture(poisson)
    sm <- m$smooth[[1]]
    j <- sm$first.para:sm$last.para
    X <- mgcv::PredictMat(sm, nd)
    B <- rbind(coef(m), coef(m) + .03)
    term <- X %*% t(B[, j]) + attr(X, "offset")
    expect_true(any(abs(attr(X, "offset")) > 0))
    eta <- sweep(term, 2L, B[, 1L], "+") + .2 * nd$x
    for (scale in c("linear_predictor", "response")) {
      expected <- if (scale == "response") family(m)$linkinv(eta) else eta
      a <- fitted_samples(m, data = nd, method = "user", draws = B, scale = scale)
      expect_equal(a$.fitted, as.numeric(expected))
      a <- add_fitted_samples(nd, m, method = "user", draws = B, scale = scale)
      expect_equal(a$.fitted[order(a$.draw, a$.row)], as.numeric(expected))
    }
    a <- smooth_samples(m, data = nd, method = "user", draws = B)
    expect_equal(a$.value[order(a$.draw, a$.row)], as.numeric(term))
    a <- fitted_samples(m, data = nd, method = "user", draws = B,
      scale = "linear_predictor", exclude = sm$label)
    expect_equal(a$.fitted, as.numeric(outer(.2 * nd$x, B[, 1], "+")))
    a <- fitted_samples(m, data = nd, method = "user", draws = B,
      scale = "linear_predictor", terms = sm$label)
    expect_equal(a$.fitted, as.numeric(term + .2 * nd$x))
    a <- fitted_samples(m, method = "user", draws = B[1, , drop = FALSE],
      scale = "linear_predictor")
    expect_equal(a$.fitted, as.numeric(predict(m, type = "link")))
  }
})

test_that("complete response derivative draws differentiate fixed boundaries", {
  nd <- data.frame(x = seq(-.5, .5, length.out = 8), y = .2)
  h <- .01
  for (poisson in c(FALSE, TRUE)) {
    m <- offset_sampling_fixture(poisson)
    B <- matrix(rep(coef(m), 2), nrow = 2L, byrow = TRUE)
    for (order in 1:2) for (scale in c("linear_predictor", "response")) {
      at <- function(delta) {
        d <- nd; d$x <- d$x + delta
        as.numeric(predict(m, d, type = if (scale == "response") "response" else "link"))
      }
      ref <- if (order == 1L) (at(h / 2) - at(-h / 2)) / h else
        (at(h) - 2 * at(0) + at(-h)) / h^2
      a <- derivative_samples(m, focal = "x", data = nd, order = order,
        type = "central", scale = scale, eps = h, method = "user", draws = B)
      expect_equal(a$.derivative[a$.draw == 1], ref, tolerance = 1e-8)
    }
  }
})

test_that("fixed contributions follow their predictor coefficient indices", {
  m <- offset_sampling_fixture()
  nd <- data.frame(x = c(-.3, .3), y = .2)
  X <- predict(m, nd, type = "lpmatrix")
  off <- attr(X, "offset")
  # A second predictor shares the smooth coefficients; both receive its fixed part.
  attr(X, "lpi") <- list(seq_len(ncol(X)), m$smooth[[1]]$first.para:m$smooth[[1]]$last.para)
  attr(X, "model.offset") <- list(c(.1, .2), c(.3, .4))
  a <- prediction_offsets(m, X)
  expect_equal(a[[1]], c(.1, .2) + rowSums(off))
  expect_equal(a[[2]], c(.3, .4) + rowSums(off))
})
