expression_wrapper_fixture <- function() {
  d <- data.frame(x = seq(1, 4, length.out = 60),
    exposure = seq(.5, 2, length.out = 60))
  d$y <- withr::with_seed(28, sin(d$x) + log(d$exposure) + rnorm(60, sd = .15))
  shift <- .6
  audit_smooth_expression <- function(x) log(x + shift)
  audit_offset_expression <- function(exposure) log(exposure) + .3
  m <- mgcv::gam(y ~ s(audit_smooth_expression(x), k = 5) +
    offset(audit_offset_expression(exposure)), data = d)
  ref_data <- d
  ref_data$coordinate <- audit_smooth_expression(d$x)
  ref_data$off <- audit_offset_expression(d$exposure)
  ref <- mgcv::gam(y ~ s(coordinate, k = 5) + offset(off), data = ref_data)
  list(model = m, reference = ref, data = d, envir = environment())
}

expression_wrapper_reference <- function(bundle, data, ...) {
  data$coordinate <- log(data$x + .6)
  data$off <- log(data$exposure) + .3
  as.numeric(predict(bundle$reference, data, ...))
}

test_that("prediction wrappers evaluate smooth and offset functions in envir", {
  b <- expression_wrapper_fixture()
  nd <- b$data[seq_len(9), ]
  nd$x <- nd$x + .2
  nd$exposure <- nd$exposure * 2
  mu <- expression_wrapper_reference(b, nd, type = "response")
  before <- serialize(b$model, NULL)
  a <- add_fitted(nd, b$model, envir = b$envir)
  expect_equal(a$.fitted, mu)
  sims <- simulate(b$model, data = nd, nsim = 2, seed = 6, envir = b$envir)
  expected <- withr::with_seed(6,
    replicate(2, rnorm(nrow(nd), mu, sqrt(b$model$sig2))))
  expect_equal(as.matrix(sims), expected, ignore_attr = TRUE)
  expect_identical(serialize(b$model, NULL), before)

  nd$exposure <- nd$exposure * 3
  expect_equal(add_fitted(nd, b$model, envir = b$envir)$.fitted - a$.fitted,
    rep(log(3), nrow(nd)))
  changed <- simulate(b$model, data = nd, nsim = 2, seed = 6, envir = b$envir)
  expect_equal(as.matrix(changed) - as.matrix(sims),
    matrix(log(3), nrow(nd), 2), ignore_attr = TRUE)
})

test_that("prediction wrappers do not borrow fitting values for new observations", {
  b <- expression_wrapper_fixture()
  nd <- b$data
  nd$exposure <- nd$exposure * 2
  for (env in list(NULL, new.env(parent = baseenv()))) {
    expect_error(add_fitted(nd, b$model, envir = env),
      class = "gratia_expression_error")
    expect_error(simulate(b$model, data = nd, seed = 6, envir = env),
      class = "gratia_expression_error")
  }
  expect_error(add_fitted(nd, b$model, envir = 1), "environment")
  expect_error(simulate(b$model, data = nd, envir = 1), "environment")
  nd$exposure <- NULL
  expect_error(add_fitted(nd, b$model, envir = b$envir),
    class = "gratia_expression_error")
  expect_error(simulate(b$model, data = nd, seed = 6, envir = b$envir),
    class = "gratia_expression_error")
})

test_that("expression environments preserve fitting-data wrapper behaviour", {
  b <- expression_wrapper_fixture()
  expected <- withr::with_seed(12, rnorm(nrow(b$data),
    as.numeric(predict(b$model, type = "response")), sqrt(b$model$sig2)))
  a <- simulate(b$model, seed = 12)
  z <- simulate(b$model, seed = 12, envir = b$envir)
  expect_equal(a$sim_1, expected)
  expect_equal(z$sim_1, expected)
  a <- add_fitted(NULL, b$model)
  z <- add_fitted(NULL, b$model, envir = b$envir)
  expect_equal(a$.fitted, as.numeric(predict(b$model)))
  expect_equal(z$.fitted, a$.fitted)
})

test_that("expression wrappers retain prediction options and missing rows", {
  b <- expression_wrapper_fixture()
  nd <- b$data[seq_len(7), ]
  nd$x[2] <- NA
  nd$y[3] <- NA
  label <- b$model$smooth[[1]]$label
  a <- add_fitted(nd, b$model, value = "prediction", type = "link",
    exclude = label, se.fit = TRUE, na.action = na.exclude, envir = b$envir)
  mu <- unname(coef(b$model)[1]) + log(nd$exposure) + .3
  mu[2] <- NA_real_
  expect_equal(a$prediction, mu)
  expect_equal(nrow(a), nrow(nd))
  expect_true(is.finite(a$prediction[3]))
  a <- add_fitted(nd, b$model, type = "terms", terms = label,
    na.action = na.omit, envir = b$envir)
  expected <- expression_wrapper_reference(b, nd, type = "terms",
    terms = b$reference$smooth[[1]]$label, na.action = na.omit)
  expect_equal(as.numeric(a[[label]]), expected)
  expect_equal(nrow(a), 6L)
  a <- simulate(b$model, data = nd, seed = 14, exclude = label,
    na.action = na.exclude, envir = b$envir)
  expected <- rep(NA_real_, nrow(nd))
  expected[-2] <- withr::with_seed(14,
    rnorm(6, mu[-2], sqrt(b$model$sig2)))
  expect_equal(a$sim_1, expected)
})

test_that("mixed GAM-component simulation forwards expression environments", {
  b <- expression_wrapper_fixture()
  # The GAM-component path only needs the embedded GAM and does not refit it.
  mixed <- structure(list(gam = b$model), class = "gamm")
  nd <- b$data[seq_len(6), ]
  a <- simulate(b$model, data = nd, seed = 25, envir = b$envir)
  z <- simulate(mixed, data = nd, seed = 25,
    conditioning = "gam_component", envir = b$envir)
  expect_equal(z, a)
})

test_that("SCAM simulation forwards expression environments", {
  skip_if_not_installed("scam")
  b <- expression_wrapper_fixture()
  m <- eval(quote(scam::scam(
    y ~ s(audit_smooth_expression(x), k = 5, bs = "mpi") +
      offset(audit_offset_expression(exposure)), data = d)), b$envir)
  nd <- b$data[seq_len(6), ]
  a <- simulate.scam(m, data = nd, seed = 25, envir = b$envir)
  z <- simulate.gam(m, data = nd, seed = 25, envir = b$envir)
  expect_equal(as.numeric(a), z$sim_1)
})
