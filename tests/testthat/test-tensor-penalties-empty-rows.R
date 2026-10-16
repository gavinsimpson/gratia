tensor_penalty_fixture <- function() {
  withr::local_seed(842)
  d <- data.frame(x = runif(180), z = runif(180),
    fac = factor(rep(c("a", "b"), 90)))
  d$y <- d$x + d$z + rnorm(180, sd = 0.04)
  d
}

penalty_as_matrix <- function(x) {
  nr <- length(unique(x$.row))
  matrix(x$.value, nrow = nr, byrow = TRUE)
}

test_that("tensor penalties reproduce stored matrices and names", {
  d <- tensor_penalty_fixture()
  for (spec in list(mgcv::te(x, z, k = 4), mgcv::ti(x, z, k = 4),
    mgcv::t2(x, z, k = 4))) {
    sm <- mgcv::smoothCon(spec, data = d, absorb.cons = TRUE)[[1]]
    p <- penalty(sm)
    expect_s3_class(p, "penalty_df")
    expect_named(p, c(".smooth", ".type", ".penalty", ".row", ".col", ".value"))
    labs <- unique(p$.penalty)
    expected_names <- names(sm$sp)
    if (is.null(expected_names)) expected_names <- names(sm$S)
    if (is.null(expected_names)) {
      expected_names <- paste(sm$label, seq_along(sm$S), sep = ".")
    }
    expect_identical(labs, expected_names)
    for (i in seq_along(sm$S)) {
      expect_equal(penalty_as_matrix(p[p$.penalty == labs[i], ]),
        unname(sm$S[[i]]))
      pr <- penalty(sm, rescale = TRUE)
      scale <- if (length(sm$S.scale)) sm$S.scale[i] else 1
      expect_equal(penalty_as_matrix(pr[pr$.penalty == labs[i], ]),
        unname(sm$S[[i]] * scale))
    }
    # Specification methods must forward both switches to the constructed smooth.
    expect_equal(penalty(spec, data = d, constraints = TRUE, rescale = TRUE),
      penalty(sm, rescale = TRUE))
    expect_equal(penalty(spec, data = d, constraints = TRUE,
      margins = TRUE, rescale = TRUE),
      penalty(sm, margins = TRUE, rescale = TRUE))
    expect_silent(ggplot2::ggplot_build(draw(p)))
  }
})

test_that("tensor marginal labels distinguish parents and margins", {
  d <- tensor_penalty_fixture()
  m <- mgcv::gam(y ~ te(x, z, k = 4) + t2(x, z, k = 4), data = d)
  p <- penalty(m, margins = TRUE, rescale = TRUE)
  expect_length(unique(p$.smooth), 4L)
  expect_identical(anyDuplicated(unique(p$.penalty)), 0L)
  for (sm in m$smooth) {
    for (j in seq_along(sm$margin)) {
      lab <- paste0(sm$label, ": margin ", j, " (", sm$margin[[j]]$label, ")")
      actual <- p[p$.smooth == lab, ]
      for (i in seq_along(sm$margin[[j]]$S)) {
        values <- actual[actual$.penalty == unique(actual$.penalty)[i], ]
        scale <- sm$margin[[j]]$S.scale
        scale <- if (length(scale)) scale[i] else 1
        expect_equal(penalty_as_matrix(values),
          unname(sm$margin[[j]]$S[[i]] * scale))
      }
      expect_true(all(is.finite(actual$.value)))
    }
  }
  expect_silent(ggplot2::ggplot_build(draw(p)))
  sm <- m$smooth[[1]]
  for (i in seq_along(sm$margin)) {
    sm$margin[[i]]$sp <- c(native = -1)
  }
  p <- penalty(sm, margins = TRUE)
  expect_length(unique(p$.penalty), 2L)
  expect_true(all(endsWith(unique(p$.penalty), ": native")))
})

test_that("fixed tensors and zero penalties retain the tidy schema", {
  d <- tensor_penalty_fixture()
  m <- mgcv::gam(y ~ te(x, z, k = 4, fx = TRUE), data = d)
  p <- penalty(m)
  expect_s3_class(p, "penalty_df")
  expect_named(p, c(".smooth", ".type", ".penalty", ".row", ".col", ".value"))
  expect_equal(nrow(p), 0L)
  expect_silent(ggplot2::ggplot_build(draw(p)))
  sm <- mgcv::smoothCon(mgcv::te(x, z, k = 4), data = d)[[1]]
  sm$S[[1]][] <- 0
  p <- penalty(sm, rescale = TRUE)
  expect_true(all(p$.value[p$.penalty == unique(p$.penalty)[1]] == 0))
  expect_silent(ggplot2::ggplot_build(draw(p)))
  sm$S <- list()
  expect_equal(penalty(sm), penalty(sm, rescale = TRUE))
})

test_that("empty B-spline prediction matrices preserve coefficient dimensions", {
  d <- tensor_penalty_fixture()
  m <- mgcv::gam(y ~ s(x, bs = "bs", k = 6), data = d)
  sm <- m$smooth[[1]]
  nc <- sm$last.para - sm$first.para + 1L
  x <- predict_mat_rows(sm, d[FALSE, ])
  expect_identical(dim(x), as.integer(c(0L, nc)))
  expect_type(x, "double")
  missing <- d[1:3, ]
  missing$x[] <- NA_real_
  expect_equal(predict_mat_rows(sm, missing), matrix(NA_real_, 3L, nc))
  missing$x[2] <- d$x[2]
  actual <- predict_mat_rows(sm, missing)
  expect_true(all(is.na(actual[c(1, 3), ])))
  expect_equal(actual[2, ], as.numeric(mgcv::PredictMat(sm, d[2, ])))
})

test_that("factor-by B-splines omit absent levels without native spline errors", {
  d <- tensor_penalty_fixture()
  m <- mgcv::gam(y ~ fac + s(x, by = fac, bs = "bs", k = 6), data = d)
  one_level <- d[d$fac == "a", ]
  expect_silent(est <- smooth_estimates(m, data = one_level,
    overall_uncertainty = FALSE))
  expect_identical(unique(est$.smooth), m$smooth[[1]]$label)
  expect_equal(nrow(est), nrow(one_level))
  expected <- smooth_estimates(m, data = one_level,
    select = m$smooth[[1]]$label, overall_uncertainty = FALSE)
  expect_equal(est$.estimate, expected$.estimate)
  expect_equal(est$.se, expected$.se)
  expect_silent(empty <- smooth_estimates(m, data = d[FALSE, ],
    overall_uncertainty = FALSE))
  expect_equal(nrow(empty), 0L)
})

test_that("constrained tensors and factor-by curves allow empty smooth rows", {
  skip_if_not("scasm" %in% getNamespaceExports("mgcv"))
  d <- tensor_penalty_fixture()
  forms <- list(
    y ~ fac + s(x, by = fac, bs = "sc", xt = "m+", k = 6),
    y ~ fac + te(x, z, by = fac, bs = c("sc", "sc"),
      xt = list("m+", "m+"), k = 6),
    y ~ fac + te(x, z, by = fac, bs = c("sc", "cr"),
      xt = list("m+", NULL), k = c(6, 4))
  )
  for (form in forms) {
    m <- mgcv::scasm(form, data = d)
    one_level <- d[d$fac == "a", ]
    expect_silent(est <- smooth_estimates(m, data = one_level,
      overall_uncertainty = FALSE))
    expect_identical(unique(est$.smooth), m$smooth[[1]]$label)
    expect_equal(nrow(est), nrow(one_level))
    expected <- smooth_estimates(m, data = one_level,
      select = m$smooth[[1]]$label, overall_uncertainty = FALSE)
    expect_equal(est$.estimate, expected$.estimate)
    expect_equal(est$.se, expected$.se)
    missing <- one_level[1:4, ]
    missing$x[c(1, 4)] <- NA_real_
    expect_silent(incomplete <- smooth_estimates(m, data = missing,
      overall_uncertainty = FALSE))
    expect_identical(is.na(incomplete$.estimate), c(TRUE, FALSE, FALSE, TRUE))
    expect_identical(is.na(incomplete$.se), c(TRUE, FALSE, FALSE, TRUE))
    expect_silent(empty <- smooth_estimates(m, data = d[FALSE, ],
      overall_uncertainty = FALSE))
    expect_equal(nrow(empty), 0L)
    sm <- m$smooth[[1]]
    expect_identical(dim(predict_mat_rows(sm, d[FALSE, ])),
      as.integer(c(0L, sm$last.para - sm$first.para + 1L)))
    p <- penalty(m)
    labs <- unique(p$.penalty)
    for (i in seq_along(sm$S)) {
      expect_equal(penalty_as_matrix(p[p$.penalty == labs[i], ]),
        unname(sm$S[[i]]))
    }
    expect_true(all(is.finite(penalty(m, margins = TRUE, rescale = TRUE)$.value)))
  }
})
