test_that("simultaneous delta contrasts agree with the linear interval helper", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  for (scale in c("link", "response")) {
    cd <- conditional_differences(m, "a", c("x", "b"), scale = scale,
      n_vals = 5, interval = "simultaneous", n_sim = 500, seed = 7,
      unconditional = TRUE)
    ep <- conditional_difference_endpoints(cd, m)
    X1 <- predict(m, ep$a, type = "lpmatrix")
    X2 <- predict(m, ep$b, type = "lpmatrix")
    if (scale == "response") {
      X1 <- X1 * as.numeric(predict(m, ep$a, type = "response"))
      X2 <- X2 * as.numeric(predict(m, ep$b, type = "response"))
    }
    V <- vcov(m, unconditional = TRUE)
    for (contrast in unique(cd$.contrast)) {
      rows <- which(cd$.contrast == contrast)
      D <- X1[rows, , drop = FALSE] - X2[rows, , drop = FALSE]
      ref <- withr::with_seed(7, simultaneous_intervals(cd$.diff[rows],
        cd$.se[rows], D, V, n_sim = 500))
      expect_equal(cd$.crit[rows], rep(unname(ref$critical), length(rows)))
      expect_equal(cd$.lower_ci[rows], ref$lower)
      expect_equal(cd$.upper_ci[rows], ref$upper)
    }
    # The maximum includes both conditioning strata, not one band per curve.
    expect_equal(length(unique(cd$.crit)), 3)
    joint <- conditional_differences(m, "a", c("x", "b"), scale = scale,
      n_vals = 5, interval = "simultaneous", simultaneous_scope = "all",
      n_sim = 500, seed = 7, unconditional = TRUE)
    ref <- withr::with_seed(7, simultaneous_intervals(joint$.diff, joint$.se,
      X1 - X2, V, n_sim = 500))
    expect_equal(joint$.crit, rep(unname(ref$critical), nrow(joint)))
    expect_true(all(joint$.crit >= cd$.crit))
    expect_true(all(joint$.lower_ci <= cd$.lower_ci))
    expect_true(all(joint$.upper_ci >= cd$.upper_ci))
  }
})

test_that("response simulation calibrates paired draws about the fitted difference", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  # Shift the supplied draws to distinguish the fitted centre from their mean.
  betas <- generate_draws(m, n = 501, seed = 4, mvn_method = "mgcv")
  betas[, 1] <- betas[, 1] + 0.3
  for (scale in c("link", "response")) {
    cd <- conditional_differences(m, "a", c("x", "b"), scale = scale,
      uncertainty = "simulation", method = "user", draws = betas,
      n_vals = 4, interval = "simultaneous", ci_level = 0.9)
    ep <- conditional_difference_endpoints(cd, m)
    eta1 <- predict(m, ep$a, type = "lpmatrix") %*% t(betas) + log(ep$a$exposure)
    eta2 <- predict(m, ep$b, type = "lpmatrix") %*% t(betas) + log(ep$b$exposure)
    ds <- if (scale == "response") exp(eta1) - exp(eta2) else eta1 - eta2
    sd <- unname(apply(ds, 1, stats::sd))
    errors <- abs(sweep(sweep(ds, 1, cd$.diff, `-`), 1, sd, `/`))
    expect_equal(cd$.se, sd)
    for (contrast in unique(cd$.contrast)) {
      rows <- which(cd$.contrast == contrast)
      crit <- unname(quantile(apply(errors[rows, , drop = FALSE], 2, max), .9, type = 8))
      expect_equal(cd$.crit[rows], rep(crit, length(rows)))
      expect_equal(cd$.lower_ci[rows], cd$.diff[rows] - crit * cd$.se[rows])
      expect_equal(cd$.upper_ci[rows], cd$.diff[rows] + crit * cd$.se[rows])
    }
    all <- conditional_differences(m, "a", c("x", "b"), scale = scale,
      uncertainty = "simulation", method = "user", draws = betas,
      n_vals = 4, interval = "simultaneous", simultaneous_scope = "all", ci_level = .9)
    crit <- unname(quantile(apply(errors, 2, max), .9, type = 8))
    expect_equal(all$.crit, rep(crit, nrow(all)))
    expect_true(all(all$.crit >= cd$.crit))
    expect_equal(all$.diff, cd$.diff)
    expect_equal(all$.se, cd$.se)
  }
})

test_that("pointwise results and identity-link equivalence are preserved", {
  f <- conditional_difference_fixture()
  for (uncertainty in c("delta", "simulation")) {
    args <- list(model = f$model, by = "a", condition = list(x = c(.2, .8)),
      uncertainty = uncertainty, n_sim = 300, seed = 34)
    point <- do.call(conditional_differences, args)
    expect_identical(point, do.call(conditional_differences,
      c(args, list(interval = "confidence"))))
    expect_false(".crit" %in% names(point))
    a <- do.call(conditional_differences, c(args, list(interval = "simultaneous")))
    b <- do.call(conditional_differences,
      c(args, list(interval = "simultaneous", scale = "link")))
    for (nm in c(".diff", ".se", ".crit", ".lower_ci", ".upper_ci")) {
      expect_equal(a[[nm]], b[[nm]])
    }
    expect_equal(a$.diff, point$.diff)
    expect_equal(a$.se, point$.se)
    expect_identical(attr(a[1:2, ], "interval"), "simultaneous")
    expect_identical(attr(a[1:2, ], "simultaneous_scope"), "contrast")
    expect_silent(ggplot2::ggplot_build(draw(a)))
  }
})

test_that("blocked calibration preserves shared draws and row ordering", {
  X <- rbind(c(1, -1), c(1, 2), c(1, .5), c(0, 0))
  V <- matrix(c(1, .2, .2, 2), 2)
  dev <- withr::with_seed(42, mgcv::rmvn(200, c(0, 0), V))
  samples <- exp(X %*% t(dev))
  a <- c(1L, 2L, 3L)
  b <- c(4L, 4L, 4L)
  for (simulation in c(FALSE, TRUE)) {
    args <- if (simulation) list(samples = samples) else list(X = X, V = V, deviations = dev)
    ref <- do.call(difference_simultaneous_summary,
      c(list(estimate = c(0, 0, 0), a = a, b = b), args))
    blocked <- do.call(difference_simultaneous_summary,
      c(list(estimate = c(0, 0, 0), a = a, b = b, block_size = 1), args))
    expect_identical(ref, blocked)
    idx <- c(3, 1, 2, 1)
    permuted <- do.call(difference_simultaneous_summary,
      c(list(estimate = rep(0, 4), a = a[idx], b = b[idx], block_size = 2), args))
    expect_equal(permuted$maxima, ref$maxima)
    expect_equal(permuted$se, ref$se[idx])
  }
})

test_that("deterministic differences collapse and inconsistent draws fail", {
  f <- conditional_difference_fixture()
  m <- f$model
  betas <- matrix(rep(coef(m), each = 5), nrow = 5)
  fixed <- conditional_differences(m, "a", list(x = .5),
    uncertainty = "simulation", method = "user", draws = betas,
    interval = "simultaneous")
  expect_equal(fixed$.se, rep(0, nrow(fixed)))
  expect_equal(fixed$.crit, rep(0, nrow(fixed)))
  expect_equal(fixed$.lower_ci, fixed$.diff)
  expect_equal(fixed$.upper_ci, fixed$.diff)
  betas[, 2] <- betas[, 2] + 2
  expect_error(conditional_differences(m, "a", list(x = .5),
    uncertainty = "simulation", method = "user", draws = betas,
    interval = "simultaneous"), "Zero-variance draws disagree")
  # A comparison that changes only an excluded effect is exactly zero.
  d <- f$data
  mr <- gam(y ~ s(x, k = 5) + s(a, bs = "re"), data = d, method = "REML")
  for (uncertainty in c("delta", "simulation")) {
    cd <- conditional_differences(mr, "a", "x", exclude = "s(a)", n_vals = 3,
      uncertainty = uncertainty, interval = "simultaneous", n_sim = 20, seed = 1)
    expect_equal(cd$.diff, rep(0, nrow(cd)))
    expect_equal(cd$.lower_ci, cd$.diff)
    expect_equal(cd$.upper_ci, cd$.diff)
    expect_true(all(is.finite(cd$.crit)))
  }
})

test_that("simultaneous response draws respect inverse-link domains", {
  expect_error(difference_inverse_link(matrix(c(-1, 2), 1), poisson("sqrt")), "domain")
  expect_error(difference_inverse_link(0, Gamma("inverse")), "domain")
  expect_error(difference_inverse_link(-1, Gamma("inverse")), "valid response means")
  expect_error(difference_inverse_link(-1, inverse.gaussian()), "domain")
  expect_error(difference_inverse_link(1000, gaussian("log")), "finite, valid")
  expect_equal(difference_inverse_link(matrix(c(0, 1), 1), binomial()),
    matrix(plogis(c(0, 1)), 1))
  f <- conditional_difference_fixture(poisson("sqrt"))
  m <- f$model
  draws <- matrix(rep(coef(m), each = 3), nrow = 3)
  draws[, 1] <- -100
  expect_error(conditional_differences(m, "a", list(x = .5),
    uncertainty = "simulation", method = "user", draws = draws,
    interval = "simultaneous"), "invalid inverse-link domain")
})

test_that("joint groups and incomplete strata retain coverage membership", {
  f <- conditional_difference_fixture()
  d <- subset(f$data, !(a == "C" & b == "high"))
  m <- gam(y ~ a + b + s(x, by = a, k = 5), data = d, method = "REML")
  for (by in list("a", c("a", "b"))) {
    cd <- conditional_differences(m, by, c("x", "b"), complete = FALSE,
      n_vals = 3, interval = "simultaneous", n_sim = 50, seed = 3)
    expect_equal(nrow(cd), if (length(by) == 1L) 12 else 30)
    expect_true(all(is.finite(cd$.crit)))
    all <- conditional_differences(m, by, c("x", "b"), complete = FALSE,
      n_vals = 3, interval = "simultaneous", simultaneous_scope = "all",
      n_sim = 50, seed = 3)
    expect_true(all(all$.crit >= cd$.crit))
    expect_equal(all$.diff, cd$.diff)
  }
})

test_that("simultaneous validation and RNG handling apply to both uncertainty methods", {
  f <- conditional_difference_fixture()
  withr::local_seed(86)
  rng <- .Random.seed
  for (uncertainty in c("delta", "simulation")) {
    args <- list(model = f$model, by = "a", condition = list(x = .5),
      uncertainty = uncertainty, interval = "simultaneous", seed = 4, n_sim = 100)
    a <- do.call(conditional_differences, args)
    expect_identical(.Random.seed, rng)
    expect_identical(a, do.call(conditional_differences, args))
    for (bad in list(list(n_sim = 1), list(n_cores = 0), list(seed = NA_real_))) {
      expect_error(do.call(conditional_differences, utils::modifyList(args, bad)), names(bad))
    }
  }
  expect_error(conditional_differences(f$model, "a", "x", interval = "bad"), "arg")
  expect_error(conditional_differences(f$model, "a", "x", simultaneous_scope = "bad"), "arg")
  withr::local_preserve_seed()
  rm(".Random.seed", envir = .GlobalEnv)
  conditional_differences(f$model, "a", list(x = .5), interval = "simultaneous",
    n_sim = 10, seed = 1)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("response bands cover an independent batch of posterior curves", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  for (scope in c("contrast", "all")) {
    cd <- conditional_differences(m, "a", "x", n_vals = 10,
      uncertainty = "simulation", interval = "simultaneous",
      simultaneous_scope = scope, n_sim = 4000, seed = 21, mvn_method = "mgcv")
    ep <- conditional_difference_endpoints(cd, m)
    betas <- generate_draws(m, n = 4000, seed = 98, mvn_method = "mgcv")
    ds <- exp(predict(m, ep$a, type = "lpmatrix") %*% t(betas) + log(ep$a$exposure)) -
      exp(predict(m, ep$b, type = "lpmatrix") %*% t(betas) + log(ep$b$exposure))
    groups <- if (scope == "all") list(seq_len(nrow(cd))) else split(seq_len(nrow(cd)), cd$.contrast)
    for (idx in groups) {
      inside <- ds[idx, , drop = FALSE] >= cd$.lower_ci[idx] &
        ds[idx, , drop = FALSE] <= cd$.upper_ci[idx]
      coverage <- mean(colSums(inside) == length(idx))
      expect_gt(coverage, .92)
      expect_lt(coverage, .98)
    }
  }
})
