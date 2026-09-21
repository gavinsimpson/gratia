context_fit <- function() {
  dat <- data.frame(x = seq(1, 4, length.out = 40), y = sin(seq(1, 4, length.out = 40)))
  mgcv::gam(y ~ s(log(x), k = 5), data = dat)
}

test_that("model context separates stored expressions from raw covariates", {
  m <- context_fit()
  ctx <- model_context(m)
  expect_identical(ctx$provenance, "evaluated")
  expect_named(ctx$data, c("y", "log(x)"))
  expect_error(recover_raw_data(m), class = "gratia_data_recovery_error")
  expect_equal(prepare_smooth_data(m, m$smooth[[1]], data.frame(x = 2:4))[["log(x)"]], log(2:4))
  expect_error(prepare_smooth_data(m, m$smooth[[1]], data.frame(z = 2:4)), class = "gratia_expression_error")
})

test_that("explicit environments resolve closures without modifying the model", {
  bundle <- local({
    shift <- 1
    fun <- function(x) log(x + shift)
    d <- data.frame(x = seq(1, 4, length.out = 40), y = sin(seq(1, 4, length.out = 40)))
    list(model = mgcv::gam(y ~ s(fun(x), k = 5), data = d), env = environment())
  })
  m <- bundle$model
  before <- serialize(m, NULL)
  expect_error(prepare_smooth_data(m, m$smooth[[1]], data.frame(x = 2:4)), class = "gratia_expression_error")
  out <- prepare_smooth_data(m, m$smooth[[1]], data.frame(x = 2:4), bundle$env)
  expect_equal(out[["fun(x)"]], log(3:5))
  expect_identical(serialize(m, NULL), before)
  expect_error(with_model_envir(m, 1), "environment")
})

test_that("evaluated values never substitute for raw new observations", {
  m <- context_fit()
  d <- data.frame(x = 2:4, check.names = FALSE)
  d[["log(x)"]] <- 99
  expect_equal(prepare_smooth_data(m, m$smooth[[1]], d)[["log(x)"]], log(2:4))
  expect_equal(prepare_smooth_data(m, m$smooth[[1]], d["log(x)"])[["log(x)"]], rep(99, 3))
  expect_error(prepare_smooth_data(m, m$smooth[[1]], data.frame(z = seq_len(nrow(m$model)))), class = "gratia_expression_error")
})

test_that("raw recovery verifies data and retained fitting rows", {
  d <- data.frame(x = seq(1, 4, length.out = 40), y = sin(seq(1, 4, length.out = 40)))
  m <- mgcv::gam(y ~ s(log(x), k = 5), data = d, subset = x > 2)
  env <- environment()
  expect_equal(recover_raw_data(m, envir = env)$x, d$x[d$x > 2])
  d$x <- d$x + 1
  expect_error(recover_raw_data(m, envir = env), class = "gratia_data_recovery_error")
})
