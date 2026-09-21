test_that("full predictions evaluate local functions and offsets at new rows", {
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  env <- environment()
  m <- mgcv::gam(y ~ s(fun(x), k = 6) + offset(log(w * z)), data = d)
  nd <- d[1:10, ]
  nd$w <- nd$w * 2
  reference_data <- nd
  reference_data[["fun(x)"]] <- fun(nd$x)
  ref <- predict(m, reference_data, type = "link", se.fit = TRUE, newdata.guaranteed = TRUE)
  expect_error(fitted_values(m, data = nd), class = "gratia_expression_error")
  a <- fitted_values(m, data = nd, scale = "link", envir = env)
  expect_equal(a$.fitted, as.numeric(ref$fit))
  expect_equal(a$.se, as.numeric(ref$se.fit))
  nd$w <- nd$w * 3
  b <- fitted_values(m, data = nd, scale = "link", envir = env)
  expect_equal(b$.fitted - a$.fitted, rep(log(3), nrow(nd)))
  expect_error(fitted_values(m, data = data.frame(unrelated = seq_len(nrow(m$model)))),
    class = "gratia_expression_error")
})

test_that("prediction and sampling preserve transformed missing observations", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ s(log(x), k = 5) + offset(log(w)), data = d)
  nd <- d[1:5, ]; nd$x[2] <- NA; nd$y[3] <- NA
  a <- fitted_values(m, data = nd)
  expect_true(is.na(a$.fitted[2]))
  expect_true(is.finite(a$.fitted[3]))
  b <- fitted_samples(m, data = nd, n = 3, seed = 4)
  expect_equal(nrow(b), 15L)
  expect_true(all(is.na(b$.fitted[b$.row == 2])))
  d$x[3] <- NA
  m <- mgcv::gam(y ~ s(log(x), k = 5), data = d, na.action = na.exclude)
  expect_equal(nrow(fitted_values(m)), nrow(d))
  expect_true(is.na(fitted_values(m)$.fitted[3]))
})

test_that("data slices recover raw covariates without reversing transformations", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ s(log(x), k = 5) + offset(log(w)), data = d)
  ds <- data_slice(m, x = evenly(x, n = 7), w = 2, data = d)
  expect_named(ds, c("x", "w"))
  expect_equal(ds$x, seq(min(d$x), max(d$x), length.out = 7))
  expect_equal(ds$w, rep(2, 7))
  expect_equal(data_slice(m, x = evenly(x, n = 7), w = 2, envir = environment()), ds)
  expect_error(data_slice(m, x = evenly(x), data = m$model), class = "gratia_data_recovery_error")
})

test_that("offset overrides are structural and do not rename transformed values", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ s(log(x), k = 5) + offset(log(w)), data = d)
  ans <- fix_offset(m, m$model, offset_val = 2)
  expect_equal(ans$w, rep(2, nrow(d)))
  expect_false("offset(log(w))" %in% names(ans))
  m2 <- mgcv::gam(y ~ s(x, k = 5) + offset(log(w * z)), data = d)
  expect_error(fix_offset(m2, m2$model, offset_val = 2), "multiple inputs")
  expect_equal(smooth_estimates(m2, data = d["x"])$.estimate,
    smooth_estimates(m2, data = d)$.estimate)
})

test_that("prediction metadata retains fitted polynomial transformations", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ poly(x, 2) + s(log(z), k = 5), data = d)
  nd <- d[1:10, ]
  a <- fitted_values(m, data = nd, scale = "link")
  ref <- predict(m, nd, se.fit = TRUE)
  expect_equal(a$.fitted, as.numeric(ref$fit))
  expect_equal(a$.se, as.numeric(ref$se.fit))
})
