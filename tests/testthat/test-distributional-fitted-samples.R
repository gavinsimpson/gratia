distributional_samples_data <- function() {
  withr::with_seed(408, {
    n <- 180L
    d <- data.frame(x = runif(n, -.7, .7), z = runif(n, -.8, .8))
    d$off <- .2 + d$x / 3
    d$off2 <- -.3 + d$z / 4
    d$y <- sample(0:2, n, replace = TRUE)
    d
  })
}

test_that("multinomial fitted draws honour scale and each predictor's offset", {
  d <- distributional_samples_data()
  m <- mgcv::gam(list(y ~ x + offset(off), ~ z + offset(off2)),
    data = d, family = mgcv::multinom(K = 2))
  B <- rbind(coef(m), coef(m) + c(.15, -.08, -.2, .12))
  for (training in c(TRUE, FALSE)) {
    nd <- if (training) d else d[c(3, 27, 95), ]
    if (!training) {
      nd$off <- nd$off + c(.5, -.2, .7)
      nd$off2 <- nd$off2 + c(-.3, .6, -.1)
    }
    input <- if (training) NULL else nd
    eta <- lapply(seq_len(nrow(B)), function(j) {
      cbind(0, B[j, 1] + B[j, 2] * nd$x + nd$off,
        B[j, 3] + B[j, 4] * nd$z + nd$off2)
    })
    # The first coefficient draw reproduces native prediction, including offsets.
    expect_equal(unname(eta[[1]][, -1]),
      unname(predict(m, newdata = nd, type = "link")))
    for (scale in c("linear_predictor", "response")) {
      expect_silent(a <- fitted_samples(m, data = input, method = "user",
        draws = B, scale = scale))
      expect_identical(nrow(a), nrow(nd) * 3L * nrow(B))
      expect_identical(unique(a$.parameter), paste0("response", 1:3))
      expect_identical(unique(a$.draw), 1:2)
      for (j in seq_len(nrow(B))) {
        expected <- eta[[j]]
        if (scale == "response") {
          expected <- exp(expected) / rowSums(exp(expected))
        }
        actual <- a[a$.draw == j, ]
        actual <- actual[order(actual$.parameter, actual$.row), ]
        expect_equal(actual$.fitted, as.numeric(expected))
      }
    }
  }
})

test_that("one-predictor multinomial draws include the reference category", {
  d <- distributional_samples_data()
  d$y <- as.integer(d$y > 0)
  m <- mgcv::gam(list(y ~ x + offset(off)), data = d,
    family = mgcv::multinom(K = 1))
  B <- matrix(coef(m), nrow = 1L)
  nd <- d[c(4, 12), ]
  eta <- cbind(0, as.numeric(predict(m, nd, type = "link")))
  for (scale in c("linear_predictor", "response")) {
    a <- fitted_samples(m, data = nd, method = "user", draws = B, scale = scale)
    a <- a[order(a$.parameter, a$.row), ]
    expected <- if (scale == "response") exp(eta) / rowSums(exp(eta)) else eta
    expect_identical(unique(a$.parameter), paste0("response", 1:2))
    expect_identical(nrow(a), 4L)
    expect_equal(a$.fitted, as.numeric(expected))
  }
})

test_that("Gaussian location-scale fitted draws retain their mean interpretation", {
  for (location_link in c("identity", "log")) {
    d <- distributional_samples_data()
    mu <- 1 + d$x + d$off
    if (location_link == "log") mu <- exp(mu)
    d$y <- withr::with_seed(430, rnorm(nrow(d), mu,
      sd = .01 + exp(-.8 + .3 * d$z + d$off2)))
    m <- mgcv::gam(list(y ~ x + offset(off), ~ z + offset(off2)),
      data = d, family = mgcv::gaulss(link = list(location_link, "logb")))
    B <- rbind(coef(m), coef(m) + c(.2, -.1, .4, -.2))
    for (training in c(TRUE, FALSE)) {
      nd <- if (training) d else d[c(5, 23, 31), ]
      nd$off2 <- nd$off2 + if (training) 0 else .7
      input <- if (training) NULL else nd
      eta <- outer(nd$off, rep(1, nrow(B))) +
        outer(rep(1, nrow(nd)), B[, 1]) + outer(nd$x, B[, 2])
      for (scale in c("linear_predictor", "response")) {
        expect_silent(a <- fitted_samples(m, data = input, method = "user",
          draws = B, scale = scale))
        expected <- if (scale == "response" && location_link == "log") {
          exp(eta)
        } else eta
        expect_identical(unique(a$.parameter), "location")
        expect_equal(a$.fitted, as.numeric(expected))
      }
    }
  }
})

test_that("zero-inflated Poisson fitted draws use both predictors for the mean", {
  d <- distributional_samples_data()
  d$y <- withr::with_seed(431, rpois(nrow(d), exp(.8 + d$x)))
  m <- mgcv::gam(list(y ~ x + offset(off), ~ z + offset(off2)),
    data = d, family = mgcv::ziplss())
  B <- rbind(coef(m), coef(m) + c(.2, -.1, .4, -.2))
  nd <- d[c(8, 35, 71), ]
  nd$off2 <- nd$off2 + .4
  for (input in list(NULL, nd)) {
    rows <- if (is.null(input)) d else input
    eta1 <- outer(rows$off, rep(1, nrow(B))) +
      outer(rep(1, nrow(rows)), B[, 1]) + outer(rows$x, B[, 2])
    eta2 <- outer(rows$off2, rep(1, nrow(B))) +
      outer(rep(1, nrow(rows)), B[, 3]) + outer(rows$z, B[, 4])
    # ziplss models a positive-count probability and a truncated Poisson.
    lambda <- exp(eta1)
    expected <- (-expm1(-exp(eta2))) * lambda / (-expm1(-lambda))
    expect_silent(a <- fitted_samples(m, data = input, method = "user",
      draws = B, scale = "response"))
    expect_identical(unique(a$.parameter), "location")
    expect_equal(a$.fitted, as.numeric(expected))
    expect_silent(a <- fitted_samples(m, data = input, method = "user",
      draws = B, scale = "linear_predictor"))
    expect_identical(unique(a$.parameter), "location")
    expect_equal(a$.fitted, as.numeric(eta1))
    expect_silent(a <- posterior_samples(m, data = input, n_draws = nrow(B),
      method = "user", draws = B, seed = 14))
    expect_identical(nrow(a), nrow(rows) * nrow(B))
    expect_true(all(a$.response >= 0))
  }
  # Supporting known LSS families does not supply a mean for unknown ones.
  unknown <- m
  unknown$family$family <- "unknown_lss"
  for (input in list(NULL, nd)) {
    expect_error(fitted_samples(unknown, data = input, method = "user", draws = B),
      "Response-scale fitted samples are not supported for general family")
  }
})
