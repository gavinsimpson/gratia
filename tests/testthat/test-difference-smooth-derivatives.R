difference_derivative_fixture <- local({
  fixtures <- list()
  function(surface = FALSE, transformed = FALSE) {
    key <- paste(surface, transformed)
    if (is.null(fixtures[[key]])) {
      d <- withr::with_seed(264, {
        d <- data.frame(x = runif(240, 1, 3), z = runif(240),
          group = factor(rep(letters[1:3], 80)))
        d$y <- as.integer(d$group) * sin(3 * d$x) +
          cos(3 * d$z) + rnorm(240, sd = .3)
        d
      })
      formula <- if (surface) {
        y ~ group + te(x, z, by = group, k = c(4, 4))
      } else if (transformed) {
        y ~ group + s(log(x), by = group, k = 6) + s(z, k = 5)
      } else y ~ group + s(x, by = group, k = 6) + s(z, k = 5)
      model <- mgcv::gam(formula, data = d, method = "REML")
      data <- expand.grid(x = c(1.3, 1.9, 2.5),
        z = if (surface) c(.2, .7) else .4, group = levels(d$group))
      fixtures[[key]] <<- list(model = model, data = data, training = d,
        select = if (surface) "te(x,z)" else if (transformed) "s(log(x))" else "s(x)")
    }
    fixtures[[key]]
  }
})

# Differentiate native predictions independently of gratia's smooth helpers.
difference_derivative_reference <- function(model, data, order, type, h,
    prediction = "lpmatrix", focal = "x") {
  at <- function(delta) {
    d <- data
    d[[focal]] <- d[[focal]] + delta
    predict(model, d, type = prediction)
  }
  result <- if (order == 1L) switch(type,
    forward = (at(h) - at(0)) / h,
    backward = (at(0) - at(-h)) / h,
    central = (at(h / 2) - at(-h / 2)) / h
  ) else switch(type,
    forward = (at(2 * h) - 2 * at(h) + at(0)) / h^2,
    backward = (at(0) - 2 * at(-h) + at(-2 * h)) / h^2,
    central = (at(h) - 2 * at(0) + at(-h)) / h^2
  )
  unname(result)
}

# Use full native prediction matrices to check all returned comparisons.
expect_difference_derivative_native <- function(actual, model, data, order,
    type = "central", focal = "x") {
  D <- difference_derivative_reference(model, data, order, type, .001,
    focal = focal)
  pairs <- combn(levels(data$group), 2, simplify = FALSE)
  K <- do.call(rbind, lapply(pairs, function(pair) {
    D[data$group == pair[1], , drop = FALSE] -
      D[data$group == pair[2], , drop = FALSE]
  }))
  V <- vcov(model)
  expected <- drop(K %*% coef(model))
  se <- sqrt(rowSums((K %*% V) * K))
  # Full and per-smooth predictions can subtract in different orders. Use an
  # absolute error floor for curvature and uncertainty near zero at boundaries.
  expect_lte(max(abs(actual$.diff - expected)), 1e-7 * max(1, abs(expected)))
  expect_lte(max(abs(actual$.se - se)), 1e-7 * max(1, abs(se)))
}

test_that("smooth difference derivatives retain joint coefficient covariance", {
  for (surface in c(FALSE, TRUE)) {
    f <- difference_derivative_fixture(surface)
    m <- f$model
    for (order in 1:2) {
      for (type in c("central", "forward", "backward")) {
        D <- difference_derivative_reference(m, f$data, order, type, .001)
        for (freq in c(FALSE, TRUE)) {
          for (unc in c(FALSE, TRUE)) {
            actual <- difference_smooths(m, select = f$select, data = f$data,
              order = order, focal = "x", type = type, eps = .001,
              frequentist = freq, unconditional = unc)
            V <- get_vcov(m, frequentist = freq, unconditional = unc)
            pairs <- combn(levels(f$data$group), 2, simplify = FALSE)
            K <- do.call(rbind, lapply(pairs, function(pair) {
              D[f$data$group == pair[1], , drop = FALSE] -
                D[f$data$group == pair[2], , drop = FALSE]
            }))
            expect_equal(unname(actual$.diff), drop(K %*% coef(m)), tolerance = 1e-7)
            expect_equal(unname(actual$.se), sqrt(rowSums((K %*% V) * K)), tolerance = 1e-7)
            expect_equal(actual$.lower_ci, actual$.diff - qnorm(.975) * actual$.se)
            expect_equal(actual$.upper_ci, actual$.diff + qnorm(.975) * actual$.se)
            expect_identical(actual$.focal, rep("x", nrow(actual)))
            expect_identical(attr(actual, "derivative_info"),
              list(order = as.integer(order), focal = "x", wrt = "smooth", type = type, eps = .001))
          }
        }
      }
    }
  }
  f <- difference_derivative_fixture()
  D <- difference_derivative_reference(f$model, f$data, 1L, "central", .001)
  a <- D[f$data$group == "a", , drop = FALSE]
  b <- D[f$data$group == "b", , drop = FALSE]
  V <- vcov(f$model)
  K <- a - b
  joint <- sqrt(rowSums((K %*% V) * K))
  separate <- sqrt(rowSums((a %*% V) * a) + rowSums((b %*% V) * b))
  expect_gt(max(abs(joint - separate)), 1e-5)
})

test_that("smooth difference derivatives align full grids and restrict selections", {
  f <- difference_derivative_fixture(TRUE)
  calculate <- function(data, ...) difference_smooths(f$model,
    select = f$select, data = data, order = 1, focal = "x", ...)
  a <- calculate(f$data)
  shuffled <- f$data
  rows <- which(shuffled$group == "b")
  shuffled[rows, ] <- shuffled[rev(rows), ]
  sorted <- function(x) dplyr::arrange(x, .level_1, .level_2, x, z)
  expect_equal(sorted(a), sorted(calculate(shuffled)), tolerance = 1e-12)
  nd <- droplevels(subset(f$data, group != "c"))
  a <- calculate(nd)
  nd$group <- factor(nd$group, levels = rev(levels(nd$group)))
  b <- calculate(nd)
  expect_equal(b$.diff, -a$.diff)
  expect_equal(b$.se, a$.se)
  expect_equal(b$.lower_ci, -a$.upper_ci)
  expect_error(calculate(f$data[-1, ]), "same prediction grid")
  expect_error(calculate(rbind(f$data, f$data[1, ])), "unique covariate combinations")
  expect_equal(nrow(calculate(f$data)), 18L)
  slice <- subset(f$data, z == .2)
  expect_equal(nrow(calculate(slice)), 9L)
  one <- subset(f$data, x == 1.3 & z == .2)
  expect_equal(nrow(calculate(one)), 3L)
  sms <- smooths(f$model)[c(1, 3)]
  explicit <- difference_smooths(f$model, select = sms, partial_match = FALSE,
    data = f$data, order = 1, focal = "x")
  expect_identical(unique(explicit$.level_1), "a")
  expect_identical(unique(explicit$.level_2), "c")
  expect_identical(unique(explicit$.smooth), "te(x,z)")
  expect_identical(explicit, difference_smooths(f$model, select = c(1, 3),
    data = f$data, order = 1, focal = "x"))
  expect_identical(explicit, difference_smooths(f$model, select = c(TRUE, FALSE, TRUE),
    data = f$data, order = 1, focal = "x"))
})

test_that("transformed differences obey the chain rule and retain raw coordinates", {
  f <- difference_derivative_fixture(transformed = TRUE)
  a <- difference_smooths(f$model, select = f$select, data = f$data, order = 1)
  b <- difference_smooths(f$model, select = f$select, data = f$data, order = 1,
    wrt = "covariate")
  x <- rep(f$data$x[f$data$group == "a"], 3)
  expect_equal(b$.diff, a$.diff / x, tolerance = 1e-6)
  expect_equal(b$.se, a$.se / x, tolerance = 1e-6)
  expect_equal(b$x, x)
  expect_identical(unique(a$.focal), "log(x)")
  expect_identical(unique(b$.focal), "x")
  for (order in 1:2) {
    actual <- difference_smooths(f$model, select = f$select, data = f$data,
      order = order, wrt = "covariate", eps = .001)
    D <- difference_derivative_reference(f$model, f$data, order, "central", .001)
    pairs <- combn(levels(f$data$group), 2, simplify = FALSE)
    expected <- unlist(lapply(pairs, function(pair) {
      drop((D[f$data$group == pair[1], , drop = FALSE] -
        D[f$data$group == pair[2], , drop = FALSE]) %*% coef(f$model))
    }))
    expect_equal(unname(actual$.diff), expected, tolerance = 1e-7)
    expect_s3_class(difference_smooths(f$model, select = f$select,
      order = order, wrt = "covariate", n = 5,
      envir = list2env(list(d = f$training))), "difference_smooth")
  }
  expect_error(difference_smooths(f$model, select = f$select, data = a,
    order = 1, wrt = "covariate"), "numeric|raw covariates")
})

test_that("automatic derivative grids accommodate the stencil and preserve supplied points", {
  for (surface in c(FALSE, TRUE)) {
    f <- difference_derivative_fixture(surface)
    for (order in 1:2) {
      for (type in c("central", "forward", "backward")) {
        a <- difference_smooths(f$model, select = f$select, order = order,
          focal = "x", type = type, eps = .001, n = 7, n_2d = 5)
        expect_equal(nrow(a), 3L * if (surface) 25L else 7L)
        expected <- seq_min_max_eps(range(f$model$model$x),
          n = if (surface) 5 else 7, order = order, type = type, eps = .001)
        expect_equal(sort(unique(a$x)), expected)
      }
    }
    supplied <- difference_smooths(f$model, select = f$select,
      data = f$data, order = 2, focal = "x")
    expect_identical(sort(unique(supplied$x)), sort(unique(f$data$x)))
    expect_gt(attr(supplied, "derivative_info")$eps, 0)
  }
})

test_that("derivative intervals use each comparison design and preserve seed scope", {
  f <- difference_derivative_fixture(TRUE)
  m <- f$model
  calculate <- function(...) difference_smooths(m, select = f$select,
    data = f$data, order = 1, focal = "x", eps = .001, n_sim = 200, ...)
  withr::local_seed(264)
  before <- .Random.seed
  point <- calculate()
  expect_identical(point, calculate(seed = NA, n_cores = NA))
  expect_identical(.Random.seed, before)
  actual <- calculate(interval = "simultaneous", seed = 42)
  expect_identical(actual, calculate(interval = "simultaneous", seed = 42))
  expect_identical(.Random.seed, before)
  expect_identical(actual, withr::with_seed(42, calculate(interval = "simultaneous")))
  D <- difference_derivative_reference(m, f$data, 1L, "central", .001)
  expected <- withr::with_seed(42, lapply(
    combn(levels(f$data$group), 2, simplify = FALSE), function(pair) {
      K <- D[f$data$group == pair[1], , drop = FALSE] -
        D[f$data$group == pair[2], , drop = FALSE]
      estimate <- drop(K %*% coef(m))
      se <- sqrt(rowSums((K %*% m$Vp) * K))
      simultaneous_intervals(estimate, se, K, m$Vp, n_sim = 200)
    }))
  expect_equal(unname(actual$.lower_ci), unname(unlist(lapply(expected, `[[`, "lower"))), tolerance = 1e-7)
  expect_equal(unname(actual$.upper_ci), unname(unlist(lapply(expected, `[[`, "upper"))), tolerance = 1e-7)
  calculate(interval = "simultaneous")
  expect_false(identical(.Random.seed, before))
  m$Vp[] <- 0
  zero <- calculate(interval = "simultaneous", seed = 42)
  expect_equal(zero$.se, rep(0, nrow(zero)))
  expect_equal(zero$.lower_ci, zero$.diff)
  expect_equal(zero$.upper_ci, zero$.diff)
})

test_that("derivative arguments reject unsupported requests without altering order zero", {
  f <- difference_derivative_fixture(TRUE)
  calculate <- function(...) difference_smooths(f$model, select = f$select, n_2d = 3, ...)
  expect_identical(calculate(), calculate(order = 0L))
  for (order in list(NULL, NA, Inf, 1.5, -1, 3, c(1, 2), "1")) {
    expect_error(calculate(order = order), "order")
  }
  expect_error(calculate(order = 1), "focal")
  for (focal in list(NA, c("x", "z"), "w", 1)) {
    expect_error(calculate(order = 1, focal = focal), "focal|Focal")
  }
  expect_error(calculate(order = 1, focal = "x", group_means = TRUE), "group_means")
  expect_error(calculate(order = 1, focal = "x", interval = "simultaneous",
    simultaneous_method = "tube"), "Tube intervals")
  for (eps in list(0, -1, NA, Inf, c(.01, .02))) {
    expect_error(calculate(order = 1, focal = "x", eps = eps), "eps")
  }
  expect_error(calculate(order = 1, focal = "x", eps = 10), "too large")
  expect_error(difference_smooths(f$model, select = 1, order = 1), "at least two")
  expect_error(calculate(order = 1, focal = "x", interval = "simultaneous", seed = NA), "seed")
})

test_that("existing model adapters forward smooth difference derivatives", {
  for (model in list(su_m_factor_by_bam, su_m_factor_by_gamm, su_m_factor_by_gamm4)) {
    gam <- if (inherits(model, "gamm")) model$gam else if (inherits(model, "gamm4")) gamm4_gam(model) else model
    args <- list(select = "s(x2)", order = 1, n = 5,
      interval = "simultaneous", seed = 264, n_sim = 100)
    expect_identical(do.call(difference_smooths, c(list(model = model), args)),
      do.call(difference_smooths.gam, c(list(model = gam), args)))
  }
})

test_that("known smooth offsets shift derivative differences without adding uncertainty", {
  f <- difference_derivative_fixture()
  model <- f$model
  rlang::local_bindings(.env = .GlobalEnv,
    Predict.matrix.gratia_difference_offset = function(object, data) {
      X <- NextMethod()
      multiplier <- switch(object$by.level, a = 2, b = -1, c = 0)
      attr(X, "offset") <- multiplier * data$x^2
      X
    })
  for (i in seq_along(model$smooth)) {
    if (is_factor_by_smooth(model$smooth[[i]])) {
      class(model$smooth[[i]]) <- c("gratia_difference_offset", class(model$smooth[[i]]))
    }
  }
  for (order in 1:2) {
    for (interval in c("confidence", "simultaneous")) {
      args <- list(select = f$select, data = f$data, order = order,
        eps = .001, interval = interval, n_sim = 100, seed = 264)
      original <- do.call(difference_smooths, c(list(model = f$model), args))
      actual <- do.call(difference_smooths, c(list(model = model), args))
      multipliers <- c(a = 2, b = -1, c = 0)
      change <- unname(multipliers[actual$.level_1] - multipliers[actual$.level_2]) *
        if (order == 1) 2 * actual$x else 2
      expect_equal(actual$.diff, original$.diff + change, tolerance = 1e-7)
      expect_equal(actual$.se, original$.se)
      expect_equal(actual$.lower_ci, original$.lower_ci + change, tolerance = 1e-7)
      expect_equal(actual$.upper_ci, original$.upper_ci + change, tolerance = 1e-7)
    }
  }
})

test_that("raw differentiation matches inputs even when transformed coordinates repeat", {
  d <- difference_derivative_fixture()$training
  d$x <- 2 * (d$x - 2)
  m <- mgcv::gam(y ~ group + s(I(x^2), by = group, k = 6) + s(z, k = 5),
    data = d, method = "REML")
  nd <- expand.grid(x = c(-1.5, -.5, .5, 1.5), z = .4, group = levels(d$group))
  a <- difference_smooths(m, select = "s(I(x^2))", data = nd,
    order = 1, wrt = "covariate", eps = .001)
  D <- difference_derivative_reference(m, nd, 1L, "central", .001)
  pairs <- combn(levels(d$group), 2, simplify = FALSE)
  expected <- unlist(lapply(pairs, function(pair) {
    drop((D[nd$group == pair[1], , drop = FALSE] -
      D[nd$group == pair[2], , drop = FALSE]) %*% coef(m))
  }))
  expect_equal(a$.diff, expected, tolerance = 1e-7)
  expect_identical(a$x, rep(c(-1.5, -.5, .5, 1.5), 3))
  nd$x[nd$group == "b"] <- nd$x[nd$group == "b"] + .01
  expect_error(difference_smooths(m, select = "s(I(x^2))", data = nd,
    order = 1, wrt = "covariate"), "same prediction grid")
})

test_that("local expressions and composite raw coordinates are evaluated forwards", {
  d <- difference_derivative_fixture()$training
  shift <- .2
  fun <- function(x) log(x + shift)
  m <- mgcv::gam(y ~ group + te(fun(x), z, by = group, k = c(3, 3)),
    data = d, method = "REML")
  nd <- difference_derivative_fixture(TRUE)$data
  a <- difference_smooths(m, select = "te(fun(x),z)", data = nd,
    order = 1, focal = "x", wrt = "covariate", envir = environment())
  expect_equal(nrow(a), 18L)
  b <- difference_smooths(m, select = "te(fun(x),z)", n_2d = 4,
    order = 1, focal = "x", wrt = "covariate", envir = environment())
  expect_equal(nrow(b), 48L)
  m <- mgcv::gam(y ~ group + s(I(x / (z + 1)), by = group, k = 6),
    data = d, method = "REML")
  expect_error(difference_smooths(m, select = "s(I(x/(z + 1)))",
    order = 1, wrt = "covariate", data = nd), "focal")
  a <- difference_smooths(m, select = "s(I(x/(z + 1)))", data = nd,
    order = 1, focal = "x", wrt = "covariate")
  expect_equal(nrow(a), 18L)
  expect_error(difference_smooths(m, select = "s(I(x/(z + 1)))", n = 5,
    order = 1, focal = "x", wrt = "covariate"), "one original model covariate")
})

test_that("empty factor-level names retain both derivative contributions", {
  d <- difference_derivative_fixture()$training
  d$group <- factor(d$group, labels = c("", "b", "c"))
  m <- mgcv::gam(y ~ group + s(x, by = group, k = 6) + s(z, k = 5),
    data = d, method = "REML")
  nd <- expand.grid(x = c(1.3, 1.9, 2.5), z = .4, group = levels(d$group))
  for (order in 1:2) {
    for (interval in c("confidence", "simultaneous")) {
      actual <- difference_smooths(m, select = "s(x)", data = nd,
        order = order, eps = .001, interval = interval,
        n_sim = 100, seed = 264)
      expect_difference_derivative_native(actual, m, nd, order)
      expect_identical(unique(actual$.level_1), c("", "b"))
      expect_identical(unique(actual$.level_2), c("b", "c"))
      expect_true(all(is.finite(actual$.lower_ci)))
      expect_true(all(is.finite(actual$.upper_ci)))
    }
  }
})

test_that("raw derivative comparisons allow group-dependent transformations", {
  d <- difference_derivative_fixture()$training
  m <- mgcv::gam(y ~ group +
    te(I(x * as.numeric(group)), z, by = group, k = c(4, 4)),
    data = d, method = "REML")
  select <- smooths(m)[1:3]
  nd <- expand.grid(x = c(1.3, 1.9, 2.5), z = c(.2, .7),
    group = levels(d$group))
  for (order in 1:2) {
    for (type in c("central", "forward", "backward")) {
      for (interval in c("confidence", "simultaneous")) {
        actual <- difference_smooths(m, select = select, partial_match = FALSE,
          data = nd, order = order, focal = "x", wrt = "covariate",
          type = type, eps = .001, interval = interval, n_sim = 100, seed = 264)
        expect_difference_derivative_native(actual, m, nd, order, type)
        expect_identical(actual$x, rep(nd$x[nd$group == "a"], 3))
        expect_identical(actual$z, rep(nd$z[nd$group == "a"], 3))
        expect_true(all(is.finite(actual$.lower_ci)))
        expect_true(all(is.finite(actual$.upper_ci)))
      }
    }
  }
  calculate <- function(data) difference_smooths(m, select = select,
    partial_match = FALSE, data = data, order = 1, focal = "x", wrt = "covariate")
  characters <- nd
  characters$group <- as.character(characters$group)
  expect_silent(character_result <- calculate(characters))
  expect_identical(character_result, calculate(nd))
  # Different row orders must still compare the same raw x and z values.
  shuffled <- nd
  rows <- which(shuffled$group == "b")
  shuffled[rows, ] <- shuffled[rev(rows), ]
  sorted <- function(x) dplyr::arrange(x, .level_1, .level_2, x, z)
  expect_equal(sorted(calculate(nd)), sorted(calculate(shuffled)), tolerance = 1e-10)
  bad <- nd
  bad$z[bad$group == "b"] <- bad$z[bad$group == "b"] + .01
  expect_error(calculate(bad), "same prediction grid")
  expect_error(calculate(rbind(nd, nd[1, ])), "unique covariate combinations")
})

test_that("singleton automatic derivative grids validate the full fitted range", {
  f <- difference_derivative_fixture()
  d <- f$training
  d$w <- withr::with_seed(265, runif(nrow(d), .05, .95))
  surface <- mgcv::gam(y ~ group + te(x, z, w, by = group, k = c(3, 3, 3)),
    data = d, method = "REML")
  for (order in 1:2) {
    for (type in c("central", "forward", "backward")) {
      for (wrt in c("smooth", "covariate")) {
        args <- list(order = order, type = type, wrt = wrt, eps = .001)
        curve <- do.call(difference_smooths, c(list(model = f$model,
          select = f$select, n = 1), args))
        expected <- seq_min_max_eps(range(d$x), 1L, order, type, .001)
        expect_equal(curve$x, rep(expected, 3))
        nd <- expand.grid(x = expected, z = .4, group = levels(d$group))
        expect_difference_derivative_native(curve, f$model, nd, order, type)
        partial <- do.call(difference_smooths, c(list(model = surface,
          select = "te(x,z,w)", focal = "w", n_2d = 2, n_3d = 1), args))
        expected <- seq_min_max_eps(range(d$w), 1L, order, type, .001)
        expect_equal(unique(partial$w), expected)
        nd <- expand.grid(x = sort(unique(partial$x)),
          z = sort(unique(partial$z)), w = expected, group = levels(d$group))
        # Match the surface's expansion order before constructing contrasts.
        nd <- nd[order(nd$group, nd$x, nd$z), ]
        expect_difference_derivative_native(partial, surface, nd, order, type,
          focal = "w")
        args$eps <- 10
        expect_error(do.call(difference_smooths, c(list(model = f$model,
          select = f$select, n = 1), args)), "too large")
        expect_error(do.call(difference_smooths, c(list(model = surface,
          select = "te(x,z,w)", focal = "w", n_2d = 2, n_3d = 1), args)),
          "too large")
      }
    }
  }
})

test_that("derivative difference plots label the values on curves and surfaces", {
  for (surface in c(FALSE, TRUE)) {
    f <- difference_derivative_fixture(surface)
    for (order in 1:2) {
      a <- difference_smooths(f$model, select = f$select, data = f$data,
        order = order, focal = "x")
      plot <- draw(a)[[1]]
      built <- ggplot2::ggplot_build(plot)
      label <- if (surface) built$plot$guides$get_guide("fill")$params$title else built$plot$labels$y
      expect_match(label, if (order == 1) "First derivative" else "Second derivative")
      expect_match(label, "with respect to x")
      expect_equal(plot$data$.diff, a$.diff[a$.level_1 == "a" & a$.level_2 == "b"])
      custom <- draw(a, ylab = "Custom", subtitle = "Comparison", caption = "Caption")[[1]]
      expect_identical(custom$labels$y, "Custom")
      expect_identical(custom$labels$subtitle, "Comparison")
      expect_identical(custom$labels$caption, "Caption")
      expect_silent(ggplot2::ggplot_build(draw(a, scales = "fixed")))
    }
  }
})
