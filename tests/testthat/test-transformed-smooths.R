# All expressions use positive inputs so numerical comparisons are independent
# of domain errors, which are tested separately below.

test_that("transformed smooth matrices match native predictions across fitters", {
  d <- transformed_data()
  formulas <- list(
    y ~ s(log(x), k = 6),
    y ~ s(log(x), sqrt(z), k = 10),
    y ~ te(log(x), sqrt(z), k = c(4, 4)),
    y ~ ti(log(x), sqrt(z), k = c(4, 4)),
    y ~ t2(log(x), sqrt(z), k = c(4, 4)),
    y ~ s(log(x), by = g, k = 5),
    y ~ s(log(x), by = log(w), k = 5),
    y ~ s(log(x), g, bs = "fs", k = 5),
    y ~ s(log(x), g, bs = "sz", k = 5)
  )
  fitters <- list(
    gam = function(f) mgcv::gam(f, data = d),
    bam = function(f) mgcv::bam(f, data = d),
    discrete = function(f) mgcv::bam(f, data = d, discrete = TRUE)
  )
  for (fitter in names(fitters)) for (f in formulas) {
    if (fitter == "discrete" && grepl("t2(", deparse(f), fixed = TRUE)) next
    m <- fitters[[fitter]](f)
    original <- m
    nd <- d[seq_len(12), ]
    X <- predict(m, nd, type = "lpmatrix")
    for (sm in m$smooth) {
      # Factor-by estimates contain just the rows for this factor level.
      rows <- if (is_factor_by_smooth(sm)) nd[[sm$by]] == sm$by.level else rep(TRUE, nrow(nd))
      ans <- smooth_estimates(m, select = sm$label, data = nd,
        overall_uncertainty = FALSE)
      idx <- smooth_coef_indices(sm)
      Xi <- X[rows, idx, drop = FALSE]
      expect_equal(ans$.estimate, unname(drop(Xi %*% coef(m)[idx])), tolerance = 1e-7,
        info = paste(fitter, sm$label))
      expect_equal(ans$.se, unname(sqrt(rowSums((Xi %*% m$Vp[idx, idx, drop = FALSE]) * Xi))),
        tolerance = 1e-7, info = paste(fitter, sm$label))
    }
    expect_identical(m, original)
  }
})

test_that("gamm and gamm4 wrappers evaluate raw transformed data", {
  d <- transformed_data()
  fits <- list(mgcv::gamm(y ~ s(log(x), k = 5), data = d))
  if (requireNamespace("gamm4", quietly = TRUE)) {
    fits <- append(fits, list(gamm4::gamm4(y ~ s(log(x), k = 5), data = d)))
  }
  for (m in fits) {
    ans <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
    ref <- predict(m$gam, d, type = "terms", se.fit = TRUE)
    expect_equal(ans$.estimate, unname(ref$fit[, 1]))
    expect_equal(ans$.se, unname(ref$se.fit[, 1]))
  }
})

test_that("shape constrained fits accept transformed covariates", {
  d <- transformed_data()
  if (exists("scasm", asNamespace("mgcv"))) {
    m <- mgcv::scasm(y ~ s(log(x), bs = "sc", xt = "m+", k = 5), data = d)
    ans <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
    ref <- predict(m, d, type = "terms", se.fit = TRUE)
    expect_equal(ans$.estimate, unname(ref$fit[, 1]))
    expect_equal(ans$.se, unname(ref$se.fit[, 1]))
  }
  skip_if_not_installed("scam")
  for (bs in c("mpi", "mpd", "cv", "cc")) {
    f <- as.formula(paste0('y ~ s(log(x), bs = "', bs, '", k = 5)'))
    m <- scam::scam(f, data = d)
    a <- smooth_estimates(m, data = d)
    b <- smooth_estimates(m, data = data.frame(`log(x)` = log(d$x), check.names = FALSE))
    expect_equal(a, b)
    expect_true(all(is.finite(a$.estimate)))
  }
})

test_that("local functions, basis values, sampling, and direct spline calls agree", {
  d <- transformed_data()
  shift <- 0.5
  fun <- function(x) log(x + shift)
  m <- mgcv::gam(y ~ s(fun(x), k = 5), data = d)
  env <- environment()
  nd <- d[1:8, ]
  evaluated <- data.frame(`fun(x)` = fun(nd$x), check.names = FALSE)
  expect_error(smooth_estimates(m, data = nd), class = "gratia_expression_error")
  expect_equal(smooth_estimates(m, data = nd, envir = env), smooth_estimates(m, data = evaluated))
  expect_equal(basis(m, data = nd, envir = env), basis(m, data = evaluated))
  expect_equal(smooth_samples(m, data = nd, envir = env, seed = 4, n = 3),
    smooth_samples(m, data = evaluated, seed = 4, n = 3))
  sm <- m$smooth[[1]]
  expect_equal(spline_values2(sm, nd, m, FALSE, overall_uncertainty = FALSE, envir = env),
    spline_values(sm, evaluated, m, FALSE, overall_uncertainty = FALSE))
  expect_equal(eval_smooth(sm, model = m, data = nd, envir = env),
    eval_smooth(sm, model = m, data = evaluated))
  grid <- smooth_estimates(m, n = 20)
  expect_equal(diff(grid[["fun(x)"]]), rep(diff(range(fun(d$x))) / 19, 19))
})

test_that("selected smooths ignore unrelated data and preserve missing rows", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ s(log(x), k = 5) + s(z, k = 5) + offset(log(w)), data = d)
  nd <- data.frame(x = c(1, NA, 3))
  ans <- smooth_estimates(m, select = "s(log(x))", data = nd)
  expect_equal(nrow(ans), 3L)
  expect_true(is.na(ans$.estimate[2]))
  expect_true(all(is.finite(ans$.estimate[c(1, 3)])))
})

test_that("standalone bases use explicit environments for local functions", {
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  spec <- mgcv::s(fun(x), k = 5)
  a <- basis(spec, data = d, envir = environment())
  evaluated <- d; evaluated[["fun(x)"]] <- fun(d$x)
  # Supply only evaluated coordinates so the function is not needed again.
  b <- basis(spec, data = evaluated["fun(x)"])
  expect_equal(a, b)
  m <- mgcv::gam(y ~ s(fun(x), k = 5), data = d)
  expect_s3_class(tidy_basis(m$smooth[[1]], at = d, envir = environment()), "tbl_df")
})
