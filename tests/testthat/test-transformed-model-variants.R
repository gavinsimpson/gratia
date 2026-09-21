test_that("other scalar bases and ordered by factors accept raw expressions", {
  d <- transformed_data()
  for (bs in c("tp", "ts", "cr", "cs", "ps", "cc")) {
    f <- as.formula(paste0('y ~ s(log(x), bs = "', bs, '", k = 6)'))
    m <- mgcv::gam(f, data = d)
    a <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
    ref <- predict(m, d, type = "terms", se.fit = TRUE)
    expect_equal(a$.estimate, as.numeric(ref$fit))
    expect_equal(a$.se, as.numeric(ref$se.fit))
  }
  d$g <- ordered(d$g)
  m <- mgcv::gam(y ~ g + s(log(x), by = g, k = 5), data = d)
  a <- smooth_estimates(m, data = d)
  expect_equal(nrow(a), sum(d$g != levels(d$g)[1]))
  expect_s3_class(a$g, "ordered")
})

test_that("matrix covariates retain their rows and summation convention", {
  d <- transformed_data()
  d$X <- I(cbind(d$x, d$x + 1))
  d$L <- I(matrix(0.5, nrow(d), 2))
  m <- mgcv::gam(y ~ s(log(X), by = L, k = 6), data = d)
  a <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
  ref <- predict(m, d, type = "terms", se.fit = TRUE)
  expect_equal(a$.estimate, as.numeric(ref$fit))
  expect_equal(a$.se, as.numeric(ref$se.fit))
  expect_equal(unname(a[["log(X)"]]), unname(log(d$X)))
})

test_that("literal expression-like names are distinct from calls in adapters", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ s(x, k = 5), data = d)
  # Test the extraction contract independently of upstream formula parsers,
  # some of which do not support literal names containing parentheses.
  names(m$var.summary) <- "log(x)"
  input <- data.frame(`log(x)` = d$x, check.names = FALSE)
  expect_identical(term_expression("log(x)", m), as.name("log(x)"))
  expect_equal(evaluate_terms(input, "log(x)", m), input)
})

test_that("multi-predictor models preserve expression and offset membership", {
  d <- transformed_data()
  m <- mgcv::gam(list(y ~ s(log(x), k = 5), ~ s(sqrt(z), k = 5)),
    family = mgcv::gaulss(), data = d)
  a <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
  ref <- predict(m, d, type = "terms", se.fit = TRUE)
  for (j in seq_along(m$smooth)) {
    take <- a$.smooth == m$smooth[[j]]$label
    expect_equal(a$.estimate[take], as.numeric(ref$fit[, j]))
    expect_equal(a$.se[take], as.numeric(ref$se.fit[, j]))
  }
  a <- fitted_values(m, data = d[1:5, ], scale = "link")
  expect_equal(nrow(a), 10L)
  expect_true(all(is.finite(a$.fitted)))
})

test_that("GJRM drawing forwards environments to embedded GAMs", {
  skip_if_not_installed("GJRM")
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  m <- GJRM::gamlss(list(y ~ s(fun(x), k = 5), ~ 1), data = d, family = "N")
  ctx <- model_context(m, "gam1")
  expect_identical(ctx$component, "gam1")
  expect_identical(ctx$call$data, m$call$data)
  a <- draw(m, data = d, envir = environment(), n = 8)
  expect_s3_class(a, "patchwork")
})

test_that("comparisons and factor differences accept transformed coordinates", {
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  m <- mgcv::gam(y ~ g + s(fun(x), by = g, k = 5), data = d)
  nd <- expand.grid(x = seq(1, 5, length.out = 8), g = levels(d$g))
  nd$g <- factor(nd$g, levels = levels(d$g))
  expect_s3_class(compare_smooths(m, m, data = nd, envir = environment()), "compare_smooths")
  a <- difference_smooths(m, select = smooths(m)[1:2], partial_match = FALSE,
    data = nd, envir = environment())
  expect_true(nrow(a) > 0)
  expect_true(all(is.finite(a$.diff)))
})

test_that("scasm tensor constraints and discrete random effects are retained", {
  d <- transformed_data()
  m <- mgcv::bam(y ~ s(g, bs = "re") + s(log(x), k = 5), data = d, discrete = TRUE)
  a <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
  expect_true(all(is.finite(a$.estimate)))
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  m <- mgcv::scasm(y ~ te(log(x), sqrt(z), bs = c("sc", "sc"),
    xt = list("m+", "m+"), k = c(6, 6)), data = d)
  a <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
  ref <- predict(m, d, type = "terms", se.fit = TRUE)
  expect_equal(a$.estimate, as.numeric(ref$fit))
  expect_equal(a$.se, as.numeric(ref$se.fit))
})

test_that("Poisson predictions retain transformed offsets across fitters", {
  d <- transformed_data()
  d$y <- withr::with_seed(20, rpois(nrow(d), d$w * exp(sin(log(d$x)))))
  fits <- list(
    mgcv::gam(y ~ s(log(x), k = 5) + offset(log(w)), family = poisson(), data = d),
    mgcv::bam(y ~ s(log(x), k = 5) + offset(log(w)), family = poisson(), data = d),
    mgcv::bam(y ~ s(log(x), k = 5) + offset(log(w)), family = poisson(), data = d, discrete = TRUE)
  )
  if (exists("scasm", asNamespace("mgcv"))) fits <- append(fits, list(
    mgcv::scasm(y ~ s(log(x), bs = "sc", xt = "m+", k = 6) + offset(log(w)), family = poisson(), data = d)))
  if (requireNamespace("scam", quietly = TRUE)) fits <- append(fits, list(
    scam::scam(y ~ s(log(x), bs = "mpi", k = 5) + offset(log(w)), family = poisson(), data = d)))
  nd <- d[1:10, ]; nd$w <- nd$w * 2
  for (m in fits) {
    a <- fitted_values(m, data = nd, scale = "link")
    ref <- predict(m, nd, type = "link", se.fit = TRUE)
    expect_equal(a$.fitted, as.numeric(ref$fit))
    expect_equal(a$.se, as.numeric(ref$se.fit))
  }
})

test_that("higher dimensional and spherical smooths evaluate expressions", {
  d <- transformed_data()
  formulas <- list(
    y ~ te(log(x), sqrt(z), log(w), k = c(4, 4, 4)),
    y ~ t2(log(x), sqrt(z), log(w), k = c(4, 4, 4)),
    y ~ te(log(x), sqrt(z), log(w), d = c(2, 1), k = c(8, 4)),
    y ~ s(I(10 * x), I(10 * z), bs = "sos", k = 9)
  )
  for (f in formulas) {
    m <- mgcv::gam(f, data = d)
    a <- smooth_estimates(m, data = d[1:8, ], overall_uncertainty = FALSE)
    ref <- predict(m, d[1:8, ], type = "terms", se.fit = TRUE)
    expect_equal(a$.estimate, as.numeric(ref$fit), tolerance = 1e-7)
    expect_equal(a$.se, as.numeric(ref$se.fit), tolerance = 1e-7)
  }
})

test_that("scam smooth reparameterization restores missing transformed rows", {
  skip_if_not_installed("scam")
  d <- transformed_data()
  m <- scam::scam(y ~ s(log(x), bs = "mpi", k = 5), data = d)
  nd <- d; nd$x[2] <- NA
  a <- smooth_estimates(m, data = nd)
  b <- smooth_estimates(m, data = nd[-2, ])
  expect_true(is.na(a$.estimate[2]))
  expect_equal(a$.estimate[-2], b$.estimate)
  expect_equal(a$.se[-2], b$.se)
  a <- smooth_estimates(m, data = data.frame(x = c(NA_real_, NA_real_)))
  expect_true(all(is.na(a$.estimate)))
})
