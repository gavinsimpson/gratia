scasm_integration_fixture <- function() {
  withr::with_seed(29, {
    d <- data.frame(x = runif(90), off = seq(-0.2, 0.2, length.out = 90))
    d$y <- d$x + d$off + rnorm(90, sd = 0.3)
    mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6) + offset(off),
      data = d, bs = 8)
  })
}

test_that("native scasm drawing dispatch preserves classes and warns once", {
  m <- scasm_integration_fixture()
  original <- m
  expect_false(formals(draw.scasm)$overall_uncertainty)
  expect_silent(p <- draw(m, n = 12, rug = FALSE))
  expect_s3_class(p, "ggplot")
  expect_true(is.expression(p[[1L]]$labels$caption))
  warnings <- character()
  withCallingHandlers(draw(m, n = 12, rug = FALSE, overall_uncertainty = TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(warnings, 1)
  expect_match(warnings, "Overall mean uncertainty is unavailable")
  expect_identical(m, original)
  expect_silent(assemble(m, n = 12, rug = FALSE))
  expect_silent(smooth_estimates(m, n = 12))
  ordinary <- mgcv::gam(y ~ s(x, k = 5), data = m$model)
  expect_silent(draw(ordinary, n = 12, rug = FALSE))
})

test_that("bootstrap fitted limits match native mgcv and retain Gaussian SEs", {
  m <- scasm_integration_fixture()
  d <- m$model[1:12, ]
  native <- predict(m, d, se.fit = 0.9)
  a <- fitted_values(m, data = d, scale = "link", ci_level = 0.9)
  b <- fitted_values(m, data = d, scale = "link", ci_level = 0.9,
    interval_method = "gaussian")
  expect_equal(a$.lower_ci, native$ll, ignore_attr = TRUE)
  expect_equal(a$.upper_ci, native$ul, ignore_attr = TRUE)
  expect_equal(a$.se, b$.se)
  expect_equal(a$.fitted, b$.fitted)
  expect_identical(attr(a, "interval_info")$method, "bootstrap")
  expect_identical(attr(a, "interval_info")$correction, "svar.only")
  d$x[2] <- NA_real_
  missing <- fitted_values(m, data = d)
  expect_true(is.na(missing$.lower_ci[2]))
  expect_equal(nrow(missing), nrow(d))
  m$bs <- NULL
  expect_error(fitted_values(m, interval_method = "bootstrap"), "valid stored")
})

test_that("location-scale bootstrap limits use existing parameter transformations", {
  d <- data_sim("sc_location_scale", n = 120, seed = 2)
  m <- withr::with_seed(2, mgcv::scasm(list(
    y ~ s(x, bs = "sc", xt = "c-", k = 6),
    ~s(z, bs = "sc", xt = "m+", k = 6)), family = mgcv::gammals(),
    data = d, bs = 6))
  nd <- d[1:8, ]
  native <- predict(m, nd, type = "link", se.fit = 0.95)
  link <- fitted_values(m, data = nd, scale = "link")
  response <- fitted_values(m, data = nd)
  expect_equal(link$.lower_ci, as.vector(t(native$ll)))
  expect_equal(link$.upper_ci, as.vector(t(native$ul)))
  expect_equal(response$.lower_ci[response$.parameter == "location"],
    exp(native$ll[, 1]), ignore_attr = TRUE)
  expect_true(all(response$.lower_ci[response$.parameter == "scale"] > 0))
  expect_equal(link$.se, response$.se)
  expect_error(fitted_values(m, interval = "simultaneous",
    interval_method = "bootstrap"), "Simultaneous bootstrap")
})

test_that("saved smooth estimates preserve shape and interval metadata", {
  m <- scasm_integration_fixture()
  a <- smooth_estimates(m, n = 12, interval = "confidence")
  b <- a[1:6, ]
  expect_s3_class(b, "smooth_estimates")
  expect_identical(attr(b, "interval_info"), attr(a, "interval_info"))
  expect_identical(attr(b, "shape_constraints"), attr(a, "shape_constraints"))
  p <- draw(b)
  expect_true(is.expression(p[[1L]]$labels$caption))
  expect_match(paste(deparse(p[[1L]]$labels$caption), collapse = " "),
    "bootstrap pointwise")
  nested <- smooth_estimates(m, n = 12, interval = "confidence", unnest = FALSE)
  expect_silent(draw(nested))
  expect_equal(nested$data[[1]]$.lower_ci, a$.lower_ci)
  expect_true(all(is.na(a$.crit)))
  expect_equal(a$.type, rep("B spline", nrow(a)))
})

test_that("simultaneous selection is explicit about unused bootstrap draws", {
  m <- scasm_integration_fixture()
  expect_warning(smooth_estimates(m, n = 10, interval = "simultaneous",
    n_sim = 50, seed = 1), "stored scasm bootstrap draws are not used")
  expect_warning(fitted_values(m, data = m$model[1:5, ],
    interval = "simultaneous", n_sim = 50, seed = 1), "stored scasm bootstrap")
  expect_silent(draw(m, n = 10, rug = FALSE, interval = "simultaneous",
    interval_method = "gaussian", n_sim = 50, seed = 1))
  expect_error(draw(m, n = 10, rug = FALSE, interval = "simultaneous",
    interval_method = "bootstrap"), "Simultaneous bootstrap")
})

test_that("confint preserves bootstrap bounds through shifts and transformations", {
  m <- scasm_integration_fixture()
  a <- smooth_estimates(m, select = "s(x)", n = 12,
    interval = "confidence", ci_level = 0.9)
  expect_silent(ci <- confint(m, parm = "s(x)", n = 12, level = 0.9))
  expect_equal(ci$.estimate, a$.estimate)
  expect_equal(ci$.lower_ci, a$.lower_ci)
  expect_equal(ci$.upper_ci, a$.upper_ci)
  expect_true(all(is.finite(ci$.lower_ci)))
  expect_true(all(is.na(ci$.crit)))
  expect_identical(attr(ci, "interval_info"), attr(a, "interval_info"))
  expect_identical(attr(ci, "shape_constraints"), attr(a, "shape_constraints"))
  shifted <- confint(m, parm = "s(x)", n = 12, level = 0.9,
    shift = TRUE, transform = function(x) exp(-x))
  intercept <- unname(coef(m)["(Intercept)"])
  expect_equal(shifted$.estimate, exp(-(a$.estimate + intercept)))
  expect_equal(shifted$.lower_ci, exp(-(a$.upper_ci + intercept)))
  expect_equal(shifted$.upper_ci, exp(-(a$.lower_ci + intercept)))
  expect_identical(shifted$.se, a$.se)
  gaussian <- confint(m, parm = "s(x)", n = 12, level = 0.9,
    interval_method = "gaussian")
  expect_equal(gaussian$.lower_ci, gaussian$.estimate - qnorm(0.95) * gaussian$.se)
  expect_identical(attr(gaussian, "interval_info")$method, "gaussian")
  expect_warning(confint(m, parm = "s(x)", n = 12,
    overall_uncertainty = TRUE), "Overall mean uncertainty is unavailable")
})

test_that("replacing saved intervals records their Gaussian source and coverage", {
  m <- scasm_integration_fixture()
  bootstrap <- smooth_estimates(m, n = 12, interval = "confidence")
  simultaneous <- smooth_estimates(m, n = 12, interval = "simultaneous",
    interval_method = "gaussian", n_sim = 50, seed = 1)
  for (a in list(bootstrap, simultaneous)) {
    b <- add_confint(a, coverage = 0.9)
    info <- attr(b, "interval_info")
    expect_identical(info$interval, "confidence")
    expect_identical(info$level, 0.9)
    expect_identical(info$method, "gaussian")
    expect_identical(info$correction, "none")
    expect_equal(b$.crit, rep(qnorm(0.95), nrow(b)))
    expect_equal(b$.lower_ci, b$.estimate - b$.crit * b$.se)
    expect_equal(b$.upper_ci, b$.estimate + b$.crit * b$.se)
    expect_identical(attr(b, "shape_constraints"), attr(a, "shape_constraints"))
    caption <- draw(b)[[1L]]$labels$caption
    expect_false(grepl("bootstrap|simultaneous", paste(deparse(caption), collapse = "")))
  }
})

test_that("bootstrap intervals omit absent factor levels without warnings", {
  m <- scasm_integration_fixture()
  d <- m$model
  d$fac <- factor(rep(c("a", "b"), length.out = nrow(d)))
  m <- withr::with_seed(29, mgcv::scasm(y ~ fac +
    s(x, by = fac, bs = "sc", xt = "m+", k = 6), data = d, bs = 6))
  expect_silent(a <- smooth_estimates(m, data = d[d$fac == "a", ],
    interval = "confidence"))
  expect_equal(nrow(a), sum(d$fac == "a"))
  expect_silent(smooth_estimates(m, data = d[FALSE, ], interval = "confidence"))
  expect_silent(smooth_estimates(m, data = d[FALSE, ], interval = "simultaneous",
    interval_method = "gaussian", n_sim = 20))
})
