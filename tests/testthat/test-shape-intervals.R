# Independent finite-grid reference: at each node the lower convex envelope is
# the smallest interpolated value among all chords enclosing that node.
shape_test_chord_minorant <- function(x, y) {
  vapply(seq_along(x), function(j) {
    candidates <- y[j]
    for (a in seq_len(j)) {
      for (b in seq.int(j, length(x))) {
        if (a == b) next
        weight <- (x[j] - x[a]) / (x[b] - x[a])
        candidates <- c(candidates, (1 - weight) * y[a] + weight * y[b])
      }
    }
    min(candidates)
  }, numeric(1))
}

shape_test_expect_shapes <- function(value, data, restrictions) {
  if (any(vapply(restrictions, function(x) "+" %in% x, logical(1)))) {
    expect_gte(min(value), -1e-11)
  }
  for (nm in names(restrictions)) {
    others <- setdiff(names(data), nm)
    groups <- if (!length(others)) rep(1, nrow(data)) else {
      do.call(interaction, c(data[others], list(drop = TRUE)))
    }
    for (idx in split(seq_len(nrow(data)), groups)) {
      idx <- idx[order(data[[nm]][idx])]
      codes <- restrictions[[nm]]
      differences <- diff(value[idx])
      if ("m+" %in% codes) expect_true(all(differences >= -1e-10))
      if ("m-" %in% codes) expect_true(all(differences <= 1e-10))
      slopes <- differences / diff(data[[nm]][idx])
      if ("c+" %in% codes) expect_true(all(diff(slopes) >= -1e-9))
      if ("c-" %in% codes) expect_true(all(diff(slopes) <= 1e-9))
    }
  }
}

shape_test_scasm_component <- function(codes, degree = 3, knots = NULL) {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  d <- data.frame(x = seq(0, 1, length.out = 41))
  sm <- mgcv::smoothCon(mgcv::s(x, bs = "sc", k = 6, xt = codes,
    m = c(degree, min(2, degree))), d, knots = knots,
    absorb.cons = FALSE)[[1L]]
  p <- ncol(sm$Ain)
  model <- structure(list(coefficients = numeric(p), Ain = sm$Ain,
    bin = sm$bin, beta0 = numeric(p)), class = "gam")
  info <- list(retained = TRUE, margins = list(list(codes = codes,
    retained = TRUE)))
  list(model = model, smooth = sm, info = info)
}

test_that("convex minorants match an independent all-chord reference", {
  withr::with_seed(510, {
    for (n in c(1L, 2L, 3L, 6L, 13L)) {
      for (replicate in seq_len(8)) {
        x <- sort(runif(n, -2, 5))
        y <- rnorm(n)
        expect_equal(shape_convex_minorant(x, y),
          shape_test_chord_minorant(x, y), tolerance = 1e-12)
      }
    }
  })
  x <- c(-2, -.9, -.89, .01, 4)
  y <- c(2, 8, -1, 3, 5)
  expected <- shape_test_chord_minorant(x, y)
  expect_equal(shape_convex_minorant(x, y), expected)
  expect_equal(shape_convex_minorant(7 + 11 * x, y), expected)
  expect_equal(shape_convex_minorant(x, 4 + 3 * y), 4 + 3 * expected)
  expect_equal(shape_convex_minorant(x, x^2), x^2)
  expect_equal(shape_convex_minorant(x, 3 - 4 * x), 3 - 4 * x)
})

test_that("convex minorants preserve finite linear data at extreme scales", {
  x <- c(-1e308, 0, 1e308)
  expect_equal(shape_convex_minorant(x, c(0, 1, 2)), c(0, 1, 2))
  result <- tryCatch(shape_convex_minorant(x, c(0, 3, 2)), error = identity)
  if (inherits(result, "error")) {
    expect_match(conditionMessage(result), "precision|reliable")
  } else {
    expect_equal(result, c(0, 1, 2))
  }
  expect_equal(shape_convex_minorant(c(0, 1, 2), c(-1e308, 0, 1e308)),
    c(-1e308, 0, 1e308))
})

test_that("convexification preserves coordinate distances at large offsets", {
  x <- 1e300 + c(0, 2, 4) * 1e284
  y <- (x - x[1L]) / (x[2L] - x[1L])
  expect_equal(shape_convex_minorant(x, y), y, tolerance = 1e-12)
})

test_that("convexification preserves small changes over a large value offset", {
  y <- 1e300 + c(0, 2, 4, 6) * 1e284
  unit <- y[2L] - y[1L]
  x <- (y - y[1L]) / unit
  result <- shape_convex_minorant(x, y)
  expect_equal((result - y) / unit, rep(0, length(y)), tolerance = 1e-12)
})

test_that("integer coordinates use double precision for slopes and interpolation", {
  x <- c(-2000000000L, 0L, 2000000000L)
  y <- c(-2000000000L, 1000000000L, 2000000000L)
  expect_silent(actual <- shape_convex_minorant(x, y))
  expect_equal(actual, c(-2e9, 0, 2e9))
  expect_identical(shape_convex_minorant(x, c(-2000000000L, 0L, 2000000000L)),
    c(-2000000000L, 0L, 2000000000L))
})

test_that("overflowing spans retain close endpoint geometry or fail explicitly", {
  x <- c(-1e308, 1e308 - c(6, 4, 0) * 1e292)
  distances <- diff(x[-1L])
  y <- c(-2, 0, cumsum(distances / min(distances)))
  result <- tryCatch(shape_convex_minorant(x, y), error = identity)
  if (inherits(result, "error")) {
    expect_match(conditionMessage(result), "precision|reliable")
  } else {
    expect_equal(result, y, tolerance = 1e-12)
  }
})

test_that("convex hull comparisons handle small finite magnitudes", {
  x <- c(0, 1e-300, 2e-300)
  y <- c(0, 1e-300, 4e-300)
  expect_equal(shape_convex_minorant(x, y) / 1e-300, c(0, 1, 4))
  # Extreme variation within one vector may be refused explicitly rather
  # than producing a different answer through floating-point underflow.
  x <- c(0, 1e-200, 2e-200, 1)
  y <- c(0, 1e-200, 4e-200, 4)
  result <- tryCatch(shape_convex_minorant(x, y), error = identity)
  if (inherits(result, "error")) {
    expect_match(conditionMessage(result), "precision|reliable")
  } else {
    scale <- pmax(abs(y), .Machine$double.xmin)
    expect_equal(result / scale, y / scale, tolerance = 1e-12)
  }
})

test_that("nearly flat value differences do not cause spurious precision errors", {
  x <- c(0, 1, 2)
  y <- c(-1, 1, 1 + .Machine$double.eps)
  expect_silent(result <- shape_convex_minorant(x, y))
  expect_equal(result, shape_test_chord_minorant(x, y), tolerance = 1e-15)
  expect_true(all(diff(diff(result)) >= -1e-15))
})

test_that("irregular-grid monotone rearrangement uses evaluation-node ranks", {
  d <- data.frame(x = c(.9, 0, .01, .2, 1))
  estimate <- c(4, 5, 1, 3, 2)
  increasing <- shape_intervals(estimate, estimate - 1, estimate + 1,
    d, list(x = "m+"))
  expect_identical(increasing$x, d$x)
  expect_equal(increasing$.estimate[order(d$x)], 1:5)
  decreasing <- shape_intervals(estimate, estimate - 1, estimate + 1,
    d, list(x = "m-"))
  expect_equal(decreasing$.estimate[order(d$x)], 5:1)
  expect_equal(increasing$.lower_ci, increasing$.estimate - 1)
  expect_equal(increasing$.upper_ci, increasing$.estimate + 1)
})

test_that("all monotonicity and curvature signs compose with positivity", {
  d <- data.frame(x = c(0, .01, .1, .4, .85, 1))
  withr::with_seed(391, {
    for (mono in c("m+", "m-")) {
      for (curve in c("c+", "c-")) {
        for (replicate in seq_len(6)) {
          estimate <- rnorm(nrow(d))
          lower <- estimate - runif(nrow(d), .1, 2)
          upper <- estimate + runif(nrow(d), .1, 2)
          restrictions <- list(x = c("+", mono, curve))
          result <- shape_intervals(estimate, lower, upper, d, restrictions)
          for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
            shape_test_expect_shapes(result[[column]], d, restrictions)
          }
          expect_true(all(result$.lower_ci <= result$.upper_ci + 1e-12))
          expect_lte(max(result$.upper_ci - result$.lower_ci),
            max(upper - lower) + 1e-12)
        }
      }
    }
  })
})

test_that("shape enforcement fixes boundary functions and preserves whole-grid events", {
  d <- data.frame(x = c(-1, -.9, -.5, 0, .4, 1))
  targets <- list(rep(0, nrow(d)), 2 + d$x, 2 - d$x,
    (d$x + 1)^2, (1 - d$x)^2, 4 - (1 - d$x)^2,
    4 - (1 + d$x)^2)
  restrictions <- list(c("+", "m+", "c+"), c("+", "m+", "c+"),
    c("+", "m-", "c+"), c("+", "m+", "c+"),
    c("+", "m-", "c+"), c("+", "m+", "c-"),
    c("+", "m-", "c-"))
  withr::with_seed(92, {
    for (i in seq_along(targets)) {
      truth <- targets[[i]]
      exact <- shape_intervals(truth, truth, truth, d,
        list(x = restrictions[[i]]))
      expect_equal(exact$.estimate, truth, tolerance = 1e-12)
      expect_equal(exact$.lower_ci, truth, tolerance = 1e-12)
      expect_equal(exact$.upper_ci, truth, tolerance = 1e-12)
      for (replicate in seq_len(5)) {
        result <- shape_intervals(truth + rnorm(length(truth)),
          truth - rexp(length(truth)), truth + rexp(length(truth)),
          d, list(x = restrictions[[i]]))
        expect_true(all(result$.lower_ci <= truth + 1e-11))
        expect_true(all(result$.upper_ci >= truth - 1e-11))
      }
    }
  })
})

test_that("tensor compositions retain every directional restriction", {
  data <- expand.grid(x = c(0, .1, .6, 1), z = c(0, .01, .8, 1),
    w = c(-1, 2))
  withr::with_seed(682, {
    permutation <- sample.int(nrow(data))
    data <- data[permutation, ]
    for (mono in c("m+", "m-")) {
      for (curve in c("c+", "c-")) {
        restrictions <- list(z = "m-", w = "m+", x = c("+", mono, curve))
        estimate <- rnorm(nrow(data))
        lower <- estimate - rexp(nrow(data))
        upper <- estimate + rexp(nrow(data))
        result <- shape_intervals(estimate, lower, upper, data, restrictions)
        expect_equal(as.data.frame(result[names(data)]), data,
          ignore_attr = TRUE)
        for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
          shape_test_expect_shapes(result[[column]], data, restrictions)
        }
        expect_true(all(result$.lower_ci <= result$.upper_ci + 1e-12))
        expect_lte(max(result$.upper_ci - result$.lower_ci),
          max(upper - lower) + 1e-12)
        second <- shape_intervals(result$.estimate, result$.lower_ci,
          result$.upper_ci, data, restrictions)
        expect_equal(second, result, tolerance = 1e-10)
      }
    }
    # This truth is convex and increasing in x, decreasing in z, increasing in w.
    truth <- 10 + data$x^2 - data$z + data$w
    result <- shape_intervals(truth, truth - rexp(nrow(data)),
      truth + rexp(nrow(data)), data,
      list(x = c("+", "m+", "c+"), z = "m-", w = "m+"))
    expect_true(all(result$.lower_ci <= truth + 1e-11))
    expect_true(all(result$.upper_ci >= truth - 1e-11))
  })
})

test_that("enforcement is deterministic and records source coverage scope", {
  d <- data.frame(x = c(0, .1, 1))
  withr::with_seed(5, {
    before <- .Random.seed
    a <- shape_intervals(c(2, -1, 1), c(1, -2, 0), c(3, 0, 2),
      d, list(x = c("+", "m+")), interval = "pointwise", level = .9)
    b <- shape_intervals(c(2, -1, 1), c(1, -2, 0), c(3, 0, 2),
      d, list(x = c("+", "m+")), interval = "pointwise", level = .9)
    expect_identical(a, b)
    expect_identical(.Random.seed, before)
    metadata <- attr(a, "interval_info")
    expect_identical(metadata$interval, "confidence")
    expect_identical(metadata$pointwise_calibration, "not guaranteed")
    expect_identical(metadata$coverage, "whole-grid containment preserved")
    expect_identical(metadata$level, .9)
    expect_identical(metadata$source, "supplied")
  })
})

test_that("whole-grid preservation does not imply pointwise preservation", {
  result <- shape_intervals(c(0, 1), c(0, -2), c(0, -1),
    data.frame(x = c(0, 1)), list(x = "m+"), interval = "pointwise")
  expect_equal(result$.lower_ci, c(-2, 0))
  expect_equal(result$.upper_ci, c(-1, 0))
  expect_lt(result$.upper_ci[1L], 0)
  expect_identical(attr(result, "interval_info")$pointwise_calibration,
    "not guaranteed")
})

test_that("two curvature directions are rejected because one pass can undo the first", {
  x <- 1:3
  values <- matrix(c(2, 1, 0, 0, 1, 2, 2, 1, 0), 3, 3)
  first <- apply(values, 2, function(y) shape_test_chord_minorant(x, y))
  second <- t(apply(first, 1, function(y) shape_test_chord_minorant(x, y)))
  expect_equal(first, values)
  expect_equal(second[, 2], c(0, 1, 0))
  expect_lt(diff(diff(second[, 2])), 0)
  d <- expand.grid(x = x, z = x)
  expect_error(shape_intervals(as.vector(values), as.vector(values) - 1,
    as.vector(values) + 1, d, list(x = "c+", z = "c+")),
    "Curvature in several coordinates")
})

test_that("invalid grids, restrictions and endpoints fail explicitly", {
  call <- function(data = data.frame(x = 1:3), constraints = list(x = "m+"),
    estimate = 1:3, lower = estimate - 1, upper = estimate + 1, level = .95) {
    shape_intervals(estimate, lower, upper, data, constraints, level = level)
  }
  expect_error(call(data = list(x = 1:3)), "data")
  expect_error(call(data = data.frame(x = c(1, Inf, 3))), "finite numeric")
  expect_error(call(data = data.frame(x = c("1", "2", "3"))), "finite numeric")
  expect_error(call(data = data.frame(x = c(1, 1, 3))), "duplicate")
  expect_error(call(data = data.frame(x = c(1, 1, 2), z = c(1, 2, 2))),
    "complete Cartesian grid")
  bad_names <- data.frame(x = 1:3, z = 1:3)
  names(bad_names) <- c("x", "x")
  expect_error(call(data = bad_names), "distinct")
  expect_error(call(data = data.frame(.estimate = 1:3),
    constraints = list(.estimate = "m+")), "conflict")
  expect_error(call(constraints = list()), "nonempty list")
  expect_error(call(constraints = list("m+")), "named")
  expect_error(call(constraints = list(z = "m+")), "coordinate columns")
  expect_error(call(constraints = list(x = character())), "compatible codes")
  expect_error(call(constraints = list(x = "unknown")), "compatible codes")
  expect_error(call(constraints = list(x = c("m+", "m-"))), "compatible codes")
  expect_error(call(constraints = list(x = c("c+", "c-"))), "compatible codes")
  expect_error(call(estimate = c(1, NA, 3)), "finite numeric vectors")
  expect_error(call(estimate = 1:2), "one value per grid row")
  expect_error(call(estimate = matrix(1:3)), "numeric vectors")
  expect_error(call(lower = 4:6), "Lower endpoints")
  expect_error(call(level = 1), "level")
  expect_error(call(level = 0), "level")
  expect_error(call(level = NA_real_), "level")
  expect_error(call(level = c(.9, .95)), "level")
})

test_that("automatic scasm eligibility rejects unsupported named curvature", {
  fixture <- shape_test_scasm_component("c+", degree = 0)
  expect_error(scasm_shape_spec(fixture$model, fixture$smooth, fixture$info),
    "degree of at least two")
  fixture <- shape_test_scasm_component(c("c+", "c-"))
  expect_error(scasm_shape_spec(fixture$model, fixture$smooth, fixture$info),
    "compatible|contradict|both")
  fixture <- shape_test_scasm_component(c("m+", "m-"))
  expect_error(scasm_shape_spec(fixture$model, fixture$smooth, fixture$info),
    "compatible|contradict|both")
  fixture <- shape_test_scasm_component("c+",
    knots = list(x = c(-3, -2, -1, 0, .05, .4, 1, 2, 3, 4)))
  expect_error(scasm_shape_spec(fixture$model, fixture$smooth, fixture$info),
    "irregular knots")
})

test_that("model plots honor the covariance requested for shape intervals", {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  withr::with_seed(31, {
    d <- data.frame(x = runif(100))
    d$y <- d$x + rnorm(100, sd = .3)
    model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 5),
      data = d, bs = 8)
    expected <- smooth_estimates(model, interval = "simultaneous",
      interval_method = "shape", n = 12, n_sim = 30, seed = 2,
      frequentist = TRUE)
    assembled <- assemble(model, interval = "simultaneous",
      interval_method = "shape", n = 12, n_sim = 30, seed = 2,
      frequentist = TRUE, rug = FALSE)
    expect_equal(assembled[[1]]$data$.lower_ci, expected$.lower_ci)
    expect_equal(assembled[[1]]$data$.upper_ci, expected$.upper_ci)
    drawn <- draw(model, interval = "simultaneous", interval_method = "shape",
      n = 12, n_sim = 30, seed = 2, frequentist = TRUE, rug = FALSE)
    expect_equal(drawn[[1]]$data$.lower_ci, expected$.lower_ci)
    expect_equal(drawn[[1]]$data$.upper_ci, expected$.upper_ci)
    expect_error(assemble(model, interval_method = "shape",
      shape_source = "bootstrap", frequentist = TRUE, rug = FALSE),
      "require frequentist = FALSE")
    expect_error(draw(model, interval_method = "shape",
      shape_source = "bootstrap", frequentist = TRUE, rug = FALSE),
      "require frequentist = FALSE")
  })
})
