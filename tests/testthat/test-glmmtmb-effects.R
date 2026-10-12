skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.9000")

tmb_effect_data <- function() {
  set.seed(391)
  d <- data.frame(x = runif(480, -1, 1), z = runif(480, .5, 2),
    w = runif(480), g = factor(rep(seq_len(16), 30)),
    h = factor(rep(seq_len(12), each = 40)), a = factor(rep(letters[1:3], 160)))
  b <- matrix(rnorm(32), 16, 2)
  d$y <- 4 + 2 * d$x + sin(6 * d$w) + b[d$g, 1] +
    b[d$g, 2] * d$x + rnorm(12)[d$h] + rnorm(480, sd = .5)
  d
}

test_that("glmmTMB parametric terms exclude smooth coefficients and recover term contributions", {
  d <- tmb_effect_data()
  for (reml in c(FALSE, TRUE)) {
    m <- glmmTMB::glmmTMB(y ~ x * a + log(z) + poly(w, 2) + s(z, k = 5) +
      (1 + x | g), data = d, REML = reml, contrasts = list(a = "contr.sum"))
    expect_identical(unname(parametric_terms(m)),
      c("x", "a", "log(z)", "poly(w, 2)", "x:a"))
    local <- glmmtmb_smooth_model(m)$models$cond
    X <- model.matrix(delete.response(terms(m)), d,
      contrasts.arg = m$modelInfo$contrasts)
    assignment <- attr(X, "assign")
    beta <- glmmTMB::fixef(m)$cond[colnames(X)]
    V <- as.matrix(vcov(m)$cond)[colnames(X), colnames(X), drop = FALSE]
    for (overall in c(FALSE, TRUE)) {
      pred <- predict_parametric_effects(local, d,
        terms = names(parametric_terms(local)),
        unconditional = FALSE, overall_uncertainty = overall)
      for (j in seq_along(parametric_terms(local))) {
        take <- which(assignment == j)
        expect_equal(unname(pred$fit[, j]), unname(drop(X[, take, drop = FALSE] %*% beta[take])))
        if (overall) take <- union(which(assignment == 0L), take)
        Xi <- X[, take, drop = FALSE]
        expect_equal(unname(pred$se.fit[, j]),
          unname(sqrt(rowSums((Xi %*% V[take, take, drop = FALSE]) * Xi))),
          tolerance = 1e-6)
      }
    }
    e <- parametric_effects(m, data = d, terms = c("log(z)", "x:a"))
    expect_identical(unique(e$.term), c("log(z)", "x:a"))
    expect_true(all(is.finite(e$.partial)))
    expect_true(all(is.finite(e$.se)))
    expect_length(draw(m, parametric = TRUE, terms = "x:a", wrap = FALSE), 4)
    expect_error(parametric_effects(m, terms = "s(z)"), "parametric part")
  }
})

test_that("glmmTMB parametric-only models and transformed prediction data work", {
  d <- tmb_effect_data()
  m <- glmmTMB::glmmTMB(y ~ x + log(z) + poly(w, 2), data = d)
  expect_identical(names(draw(m, wrap = FALSE)), c("log(z)", "poly(w, 2)", "x"))
  expect_identical(names(draw(m, terms = "x", wrap = FALSE)), "x")
  newdata <- d[1:8, ]
  newdata$z <- seq(.6, 1.5, length.out = 8)
  e <- parametric_effects(m, data = newdata, terms = "log(z)", overall_uncertainty = FALSE)
  beta <- glmmTMB::fixef(m)$cond[["log(z)"]]
  expect_equal(e$.partial, log(newdata$z) * beta)
  expect_equal(e$.value, log(newdata$z))
  raw <- parametric_effects(m, data = newdata, terms = "log(z)", back_transform = TRUE)
  expect_equal(raw$.value, newdata$z)
  expect_equal(raw$.partial, e$.partial)
  # Stored polynomial prediction parameters, rather than a new orthogonal basis.
  poly <- parametric_effects(m, data = newdata, terms = "poly(w, 2)")
  X <- model.matrix(delete.response(terms(m)), newdata)
  columns <- grep("poly", colnames(X))
  expect_equal(poly$.partial,
    unname(drop(X[, columns] %*% glmmTMB::fixef(m)$cond[columns])))
  no_intercept <- glmmTMB::glmmTMB(y ~ 0 + x, data = d)
  expect_equal(parametric_effects(no_intercept)$.se,
    parametric_effects(no_intercept, overall_uncertainty = FALSE)$.se)
  expect_error(parametric_effects(m, unconditional = TRUE), "joint covariance")
})

test_that("parametric labels and uncertainty follow each glmmTMB linear predictor", {
  d <- tmb_effect_data()
  set.seed(137)
  d$y <- rnorm(nrow(d), 1 + d$x + sin(6 * d$w), sd = exp(.3 * d$x))
  m <- glmmTMB::glmmTMB(y ~ x + s(w, k = 5), dispformula = ~x,
    data = d, REML = TRUE)
  expect_identical(parametric_terms(m), c(x = "x", x.1 = "x"))
  expect_identical(names(draw(m, wrap = FALSE)), c("s(w)", "x", "x.1"))
  expect_identical(names(draw(m, terms = "x.1", wrap = FALSE)), c("s(w)", "x.1"))
  e <- parametric_effects(m, terms = "x.1", data = d)
  expect_equal(e$.partial, unname(d$x * glmmTMB::fixef(m)$disp[["x"]]))
  V <- as.matrix(vcov(m)$disp)
  X <- cbind(1, d$x)
  expect_equal(e$.se, unname(sqrt(rowSums((X %*% V) * X))), tolerance = 1e-6)
  # A predictor containing only parametric terms must survive a later smooth LP.
  m <- glmmTMB::glmmTMB(y ~ x, dispformula = ~x + s(w, k = 5), data = d)
  expect_identical(names(draw(m, wrap = FALSE)), c("s.1(w)", "x", "x.1"))
})

test_that("native random effects retain block ordering and exclude smooth blocks", {
  d <- tmb_effect_data()
  set.seed(82)
  d$y <- d$y + rnorm(16)[d$g] * d$z
  # Current glmmTMB warns while assigning smooth indices beside repeated
  # grouping factors; gratia uses the actual fitted blocks instead.
  m <- suppressWarnings(glmmTMB::glmmTMB(y ~ x + s(w, k = 5) + (1 | h) +
    (1 + x | g) + (0 + z | g), data = d, REML = TRUE))
  native <- glmmtmb_random_effects(m)
  reference <- glmmTMB::ranef(m, condVar = FALSE)$cond
  expect_length(native, 3)
  expect_equal(unname(native[[1]]$values), unname(as.matrix(reference$h)))
  expect_equal(unname(native[[2]]$values), unname(as.matrix(reference$g[, 1:2])))
  expect_equal(unname(native[[3]]$values), unname(as.matrix(reference$g[, 3, drop = FALSE])))
  expect_identical(names(draw(m, wrap = FALSE)),
    c("s(w)", "(1 | h)", "(1 | g)", "(x | g)", "(z | g)", "x"))
  plots <- draw(m, wrap = FALSE, parametric = FALSE)
  expect_equal(plots[["(x | g)"]]$data$.estimate, unname(reference$g$x))
  expect_s3_class(plots[["(x | g)"]]$layers[[1]]$stat, "StatQq")
  expect_identical(names(draw(m, select = c("s(w)", "(x | g)"), wrap = FALSE)),
    c("s(w)", "(x | g)"))
  expect_identical(names(draw(m, select = c(1, 4), wrap = FALSE)), c("s(w)", "(x | g)"))
  expect_identical(names(draw(m, select = " | g)", partial_match = TRUE, wrap = FALSE)),
    c("(1 | g)", "(x | g)", "(z | g)"))
})

test_that("ranef_lines replaces only simple intercept-slope pairs with random contributions", {
  d <- tmb_effect_data()
  m <- glmmTMB::glmmTMB(y ~ x + (1 + x | g) + (1 | h), data = d, REML = TRUE)
  qq <- draw(m, parametric = FALSE, wrap = FALSE)
  expect_identical(names(qq), c("(1 | g)", "(x | g)", "(1 | h)"))
  expect_true(all(vapply(qq, function(p) is.null(p$labels$subtitle), logical(1))))
  plots <- draw(m, ranef_lines = TRUE, parametric = FALSE, wrap = FALSE,
    select = c("(1 | g)", "(x | g)"), data = data.frame(x = c(-.8, 0, .8)))
  expect_length(plots, 1)
  expect_null(plots[[1]]$labels$subtitle)
  lines <- plots[[1]]$data
  b <- glmmTMB::ranef(m, condVar = FALSE)$cond$g
  expect_identical(unique(lines$.group), rownames(b))
  for (g in rownames(b)) {
    group <- lines[lines$.group == g, ]
    expect_equal(group$.estimate, b[g, "(Intercept)"] + group$.x * b[g, "x"])
  }
  expect_s3_class(plots[[1]]$layers[[1]]$geom, "GeomLine")
  absent <- m
  absent$sdr <- NULL
  expect_equal(draw(absent, ranef_lines = TRUE, parametric = FALSE,
    wrap = FALSE)[[1]]$data,
    draw(m, ranef_lines = TRUE, parametric = FALSE, wrap = FALSE)[[1]]$data)
  expect_null(absent$sdr)
  # Selecting only one coefficient preserves that QQ panel.
  expect_identical(names(draw(m, ranef_lines = TRUE, select = "(x | g)", wrap = FALSE)),
    "(x | g)")
  transformed <- draw(m, ranef_lines = TRUE, parametric = FALSE, wrap = FALSE,
    select = c("(1 | g)", "(x | g)"), data = data.frame(x = 0),
    constant = 2, fun = exp)
  expect_equal(transformed[[1]]$data$.estimate, exp(b[["(Intercept)"]] + 2))
  expect_length(draw(m, ranef_lines = TRUE, wrap = FALSE), 3)
  expect_error(draw(m, ranef_lines = NA), "TRUE or FALSE")
  expect_error(draw(m, ranef_lines = TRUE, select = c("(1 | g)", "(x | g)"),
    data = data.frame(z = 1)), "numeric 'x'")
})

test_that("complex and separate random-effect terms retain QQ panels", {
  d <- tmb_effect_data()
  set.seed(49)
  d$z <- runif(nrow(d), -1, 1)
  b <- matrix(rnorm(64, sd = 2), 16, 4)
  d$y <- 3 + d$x + d$z + b[d$g, 1] + b[d$g, 2] * d$x +
    b[d$g, 3] * d$z + b[d$g, 4] * d$x * d$z + rnorm(nrow(d), sd = .2)
  m <- glmmTMB::glmmTMB(y ~ x * z + (1 + x + z + x:z | g), data = d,
    control = glmmTMB::glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
  plots <- draw(m, ranef_lines = TRUE, parametric = FALSE, wrap = FALSE)
  expect_identical(names(plots), c("(1 | g)", "(x | g)", "(z | g)", "(x:z | g)"))
  expect_identical(plots[["(x:z | g)"]]$labels$title, "(x:z | g)")
  expect_true(all(vapply(plots, function(p) inherits(p$layers[[1]]$stat, "StatQq"), logical(1))))
  m <- glmmTMB::glmmTMB(y ~ x + (1 | g) + (0 + x | g), data = d)
  expect_length(draw(m, ranef_lines = TRUE, parametric = FALSE, wrap = FALSE), 2)
  set.seed(717)
  a_effect <- matrix(rnorm(48, sd = 2), 16, 3)
  d$y <- a_effect[cbind(as.integer(d$g), as.integer(d$a))] + rnorm(nrow(d), sd = .2)
  m <- glmmTMB::glmmTMB(y ~ a + (1 + a | g), data = d)
  expect_length(draw(m, ranef_lines = TRUE, parametric = FALSE, wrap = FALSE), 3)
})

test_that("native and parametric terms are labelled across all three predictors", {
  set.seed(1006)
  d <- data.frame(x = runif(1200, -1, 1),
    g = factor(rep(seq_len(20), 60)), h = factor(rep(seq_len(10), each = 120)))
  bc <- rnorm(20, sd = .6)
  bz <- rnorm(20, sd = .7)
  bd <- rnorm(10, sd = .4)
  d$y <- rnbinom(nrow(d), mu = exp(1 + .3 * d$x + bc[d$g]),
    size = exp(1 + .2 * d$x + bd[d$h]))
  d$y[runif(nrow(d)) < plogis(-.5 + .3 * d$x + bz[d$g])] <- 0
  m <- glmmTMB::glmmTMB(y ~ x + (1 | g), ziformula = ~x + (1 | g),
    dispformula = ~x + (1 | h), family = glmmTMB::nbinom2(), data = d,
    control = glmmTMB::glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
  expect_true(m$sdr$pdHess)
  plots <- draw(m, wrap = FALSE)
  expect_identical(names(plots),
    c("(1 | g)", "(1 | g).1", "(1 | h).2", "x", "x.1", "x.2"))
  expect_identical(vapply(plots[1:3], function(p) p$labels$subtitle, character(1)),
    setNames(c("Conditional model", "Zero-inflation model", "Dispersion model"),
      names(plots)[1:3]))
  re <- glmmTMB::ranef(m, condVar = FALSE)
  expect_equal(plots[["(1 | g).1"]]$data$.estimate,
    re$zi$g[["(Intercept)"]])
  expect_equal(plots[["(1 | h).2"]]$data$.estimate,
    re$disp$h[["(Intercept)"]])
  for (i in seq_along(re)) {
    label <- if (i == 1L) "x" else paste0("x.", i - 1L)
    e <- parametric_effects(m, terms = label, data = d)
    expect_equal(e$.partial, unname(d$x * glmmTMB::fixef(m)[[i]][["x"]]))
  }
})

test_that("effect grids use fitting rows after subsets and missingness", {
  d <- tmb_effect_data()
  d$x[3] <- NA
  d$y[7] <- NA
  m <- glmmTMB::glmmTMB(y ~ x + (1 + x | g), data = d,
    subset = z < 1.8, na.action = na.exclude, REML = TRUE)
  frame <- model.frame(m)
  e <- parametric_effects(m, terms = "x")
  expect_equal(sort(e$.value), sort(unique(frame$x)))
  plots <- draw(m, parametric = FALSE, ranef_lines = TRUE, wrap = FALSE, n = 7)
  expect_length(unique(plots[[1]]$data$.x), 7)
  expect_equal(range(plots[[1]]$data$.x), range(frame$x))
})

test_that("new glmmTMB effect plots have stable rendering", {
  skip_on_cran()
  d <- tmb_effect_data()
  m <- glmmTMB::glmmTMB(y ~ x + s(w, k = 5) + (1 + x | g), data = d, REML = TRUE)
  vdiffr::expect_doppelganger("glmmTMB parametric and random QQ", draw(m))
  vdiffr::expect_doppelganger("glmmTMB random lines", draw(m, ranef_lines = TRUE))
})
