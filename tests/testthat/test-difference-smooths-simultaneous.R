# Construct contrasts independently of calc_difference() using smooth metadata.
difference_joint_reference <- function(model, data, pair, group_means, V) {
  first <- data[data$group == pair[1], , drop = FALSE]
  second <- first
  second$group <- factor(pair[2], levels = levels(data$group))
  X <- predict(model, first, type = "lpmatrix") -
    predict(model, second, type = "lpmatrix")
  keep <- rep(FALSE, ncol(X))
  for (sm in model$smooth) {
    if (sm$by == "group" && sm$by.level %in% pair) {
      keep[sm$first.para:sm$last.para] <- TRUE
    }
  }
  if (group_means) keep[seq_len(model$nsdf)] <- TRUE
  X[, !keep] <- 0
  estimate <- drop(X %*% coef(model))
  se <- sqrt(diag(X %*% V %*% t(X)))
  draws <- mvnfast::rmvn(300, rep(0, ncol(X)), V, ncores = 1)
  maxima <- apply(abs(sweep(draws %*% t(X), 2, se, `/`)), 1, max)
  critical <- quantile(maxima, .9, type = 8)
  list(estimate = unname(estimate), se = unname(se),
    lower = unname(estimate - critical * se),
    upper = unname(estimate + critical * se))
}

difference_joint_fixture <- function(multivariate = FALSE) {
  withr::local_seed(380)
  d <- data.frame(x = runif(180), z = runif(180),
    group = factor(rep(letters[1:3], 60)))
  d$y <- as.integer(d$group) * (sin(4 * d$x) + .5) +
    d$x * d$z + rnorm(180, sd = .3)
  formula <- if (multivariate) {
    y ~ group + te(x, z, by = group, k = c(3, 3))
  } else y ~ group + s(x, by = group, k = 5) + s(z, k = 4)
  model <- mgcv::gam(formula, data = d, method = "REML")
  nd <- expand.grid(x = c(.2, .5, .8), group = levels(d$group), z = .4)
  list(model = model, data = nd,
    select = if (multivariate) "te(x,z)" else "s(x)")
}

test_that("smooth differences reuse joint covariance separately for each pair", {
  f <- difference_joint_fixture()
  for (group_means in c(FALSE, TRUE)) {
    for (freq in c(FALSE, TRUE)) {
      for (unc in c(FALSE, TRUE)) {
        model <- f$model
        V <- if (freq) model$Ve else if (unc) model$Vc else model$Vp
        expect_false(is.null(V))
        expected <- withr::with_seed(381, lapply(
          combn(levels(f$data$group), 2, simplify = FALSE),
          function(pair) difference_joint_reference(model, f$data, pair, group_means, V)))
        actual <- difference_smooths(model, select = f$select, data = f$data,
          group_means = group_means, frequentist = freq, unconditional = unc,
          interval = "simultaneous", ci_level = .9, n_sim = 300, seed = 381)
        expect_s3_class(actual, "difference_smooth")
        expect_named(actual, c(".smooth", ".by", ".level_1", ".level_2",
          ".diff", ".se", ".lower_ci", ".upper_ci", "x"))
        expect_equal(unname(actual$.diff), unlist(lapply(expected, `[[`, "estimate")))
        expect_equal(unname(actual$.se), unlist(lapply(expected, `[[`, "se")))
        expect_equal(unname(actual$.lower_ci), unlist(lapply(expected, `[[`, "lower")))
        expect_equal(unname(actual$.upper_ci), unlist(lapply(expected, `[[`, "upper")))
      }
    }
  }
})

test_that("simultaneous differences align grids and reverse contrast direction", {
  for (multi in c(FALSE, TRUE)) {
    f <- difference_joint_fixture(multi)
    nd <- f$data[f$data$group != "c", ]
    nd$group <- droplevels(nd$group)
    calculate <- function(data) difference_smooths(f$model,
      select = f$select, data = data, interval = "simultaneous",
      n_sim = 300, seed = 382)
    forward <- calculate(nd)
    shuffled <- calculate(nd[c(1:3, 6:4), ])
    expect_identical(forward, shuffled)
    reversed <- nd
    reversed$group <- factor(reversed$group, levels = rev(levels(nd$group)))
    backward <- calculate(reversed)
    expect_equal(unname(backward$.diff), -unname(forward$.diff))
    expect_equal(unname(backward$.se), unname(forward$.se))
    expect_equal(unname(backward$.lower_ci), -unname(forward$.upper_ci))
    expect_equal(unname(backward$.upper_ci), -unname(forward$.lower_ci))
    one <- calculate(nd[c(1, 4), ])
    expect_equal(nrow(one), 1)
    expect_true(is.finite(one$.lower_ci))
    expect_error(calculate(nd[-4, ]), "same prediction grid")
    expect_error(calculate(rbind(nd, nd[1, ])), "unique covariate combinations")
  }
})

test_that("pointwise output is unchanged and simulation seeds scope whole calls", {
  f <- difference_joint_fixture()
  withr::local_seed(383)
  before <- .Random.seed
  point <- difference_smooths(f$model, select = f$select, data = f$data)
  explicit <- difference_smooths(f$model, select = f$select, data = f$data,
    interval = "confidence", seed = NA, n_sim = NA, n_cores = NA)
  expect_identical(point, explicit)
  expect_equal(point$.lower_ci, point$.diff - qnorm(.975) * point$.se)
  expect_equal(point$.upper_ci, point$.diff + qnorm(.975) * point$.se)
  expect_identical(.Random.seed, before)
  calculate <- function(...) difference_smooths(f$model, select = f$select,
    data = f$data, interval = "simultaneous", n_sim = 300, ...)
  a <- calculate(seed = 384)
  expect_identical(a, calculate(seed = 384))
  expect_identical(.Random.seed, before)
  # External seed scoping must match the internal scoping over ALL pairs.
  expect_identical(a, withr::with_seed(384, calculate()))
  calculate()
  expect_false(identical(.Random.seed, before))
  expect_error(calculate(seed = NA), "seed")
  expect_error(calculate(seed = c(1, 2)), "seed")
  expect_error(calculate(n_cores = 0), "n_cores")
  expect_error(calculate(ci_level = NA), "level")
  expect_error(difference_smooths(f$model, select = f$select,
    interval = "simultaneous", n_sim = 0), "n_sim")
  expect_error(difference_smooths(f$model, select = f$select,
    interval = "invalid"), "arg")
  rm(".Random.seed", envir = .GlobalEnv)
  expect_identical(a, calculate(seed = 384))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("simultaneous differences delegate through BAM and GAMM methods", {
  for (model in list(su_m_factor_by_bam, su_m_factor_by_gamm)) {
    gam <- if (inherits(model, "gamm")) model$gam else model
    lev <- levels(model.frame(gam)$fac)
    data <- data_slice(gam, x2 = c(.2, .8), fac = factor(lev, levels = lev))
    args <- list(select = "s(x2)", data = data,
      interval = "simultaneous", seed = 385, n_sim = 100)
    actual <- do.call(difference_smooths, c(list(model = model), args))
    expected <- do.call(difference_smooths.gam, c(list(model = gam), args))
    expect_identical(actual, expected)
  }
})
