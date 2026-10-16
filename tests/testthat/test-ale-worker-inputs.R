test_that("ALE worker formulas require known deterministic function bindings", {
  env <- new.env(parent = asNamespace("stats"))
  safe <- function(expr) ale_safe_expression(expr, env, c("x", "z"))
  expect_true(safe(quote(log(x) + I(x * z))))
  expect_true(safe(quote(stats::plogis(x))))
  expect_true(safe(quote(base::log1p(x))))
  expect_false(safe(quote(log(x, base = ))))
  expect_false(safe(quote(stats::dnorm(x, sd = ))))
  for (expr in list(quote(rnorm(x)), quote(stats::rnorm(x)),
      quote(sample(x)), quote(base::jitter(x)), quote(get("x")),
      quote(base::get("x")), quote(splines::ns(x)), quote(x + shift),
      quote(local_log(x)), quote((get("log"))(x)))) {
    expect_false(safe(expr), info = deparse1(expr))
  }
  env$local_log <- base::log
  expect_false(safe(quote(local_log(x))))
  env$log <- function(x) x + 1
  environment(env$log) <- baseenv()
  expect_false(safe(quote(log(x))))
  expect_true(safe(quote(base::log(x))))
  reads <- 0L
  active <- new.env(parent = asNamespace("stats"))
  makeActiveBinding("log", function(value) {
    reads <<- reads + 1L
    base::log
  }, active)
  expect_false(ale_safe_expression(quote(log(x)), active, "x"))
  expect_identical(reads, 0L)
  lazy <- new.env(parent = asNamespace("stats"))
  counter <- new.env(parent = emptyenv())
  counter$reads <- 0L
  delayedAssign("log", {
    counter$reads <- counter$reads + 1L
    base::log
  }, assign.env = lazy)
  expect_false(ale_safe_expression(quote(log(x)), lazy, "x"))
  expect_identical(counter$reads, 0L)
})

test_that("ALE keeps custom contrasts, predictor classes and controls in the caller", {
  withr::local_seed(8406)
  d <- data.frame(x = runif(60), z = runif(60),
    f = factor(rep(letters[1:3], 20)), y = rnorm(60))
  m <- mgcv::gam(y ~ x + z + f, data = d)
  expect_false(ale_predict_in_caller(m, d))
  tagged <- m
  class(tagged) <- c("ale_custom_model", class(m))
  expect_true(ale_predict_in_caller(tagged, d))
  numeric_contrasts <- m
  numeric_contrasts$contrasts$f <- stats::contr.sum(3)
  expect_false(ale_predict_in_caller(numeric_contrasts, d))
  custom_contrasts <- m
  custom_contrasts$contrasts$f <- "ale_local_contrasts"
  expect_true(ale_predict_in_caller(custom_contrasts, d))
  custom_column <- d
  attr(custom_column$f, "contrasts") <- "ale_local_contrasts"
  expect_true(ale_predict_in_caller(m, custom_column))
  custom_column <- d
  class(custom_column$z) <- "ale_custom_predictor"
  expect_true(ale_predict_in_caller(m, custom_column))
  # Unused columns cannot alter prediction and need not enter worker batches.
  d$unused <- structure(d$z, class = "ale_custom_predictor")
  expect_false(ale_predict_in_caller(m, d))
  expect_true(ale_predict_in_caller(m, d, dots = list(cluster = new.env())))
  expect_true(ale_predict_in_caller(m, d, dots = list(control = function() 1)))
})

test_that("ALE worker models release fitting environments without changing predictions", {
  m <- local({
    sentinel <- raw(500000)
    d <- withr::with_seed(8407, data.frame(x = runif(60), z = runif(60),
      f = factor(rep(letters[1:3], 20)), y = rnorm(60)))
    mgcv::gam(y ~ s(x, k = 5) + s(f, bs = "re") + z, data = d)
  })
  original_environment <- environment(m$formula)
  m$call$formula <- m$formula
  nd <- m$model[c(7, 2, 7, 10), ]
  worker <- ale_worker_model(m)
  expect_identical(environment(m$formula), original_environment)
  expect_identical(environment(worker$formula), asNamespace("stats"))
  expect_identical(environment(worker$terms), asNamespace("stats"))
  expect_identical(environment(worker$call$formula), asNamespace("stats"))
  expect_identical(environment(worker$smooth[[2]]$form), baseenv())
  expect_identical(attr(worker, "gratia.envir"), asNamespace("stats"))
  expect_gt(length(serialize(m, NULL)), 500000)
  expect_lt(length(serialize(worker, NULL)), 100000)
  expect_identical(predict_model(worker, nd, type = "link"),
    predict_model(m, nd, type = "link"))
  expect_identical(predict_model(worker, nd, type = "lpmatrix"),
    predict_model(m, nd, type = "lpmatrix"))
})

test_that("ALE inspects S3 transformation bindings without running caller code", {
  withr::local_seed(8414)
  for (kind in c("active", "lazy")) {
    env <- new.env(parent = asNamespace("stats"))
    counter <- new.env(parent = emptyenv())
    counter$reads <- 0L
    if (kind == "active") {
      makeActiveBinding("Math.AsIs", function(value) {
        counter$reads <- counter$reads + 1L
        runif(1)
        function(x, ...) NextMethod()
      }, env)
    } else {
      delayedAssign("Math.AsIs", {
        counter$reads <- counter$reads + 1L
        runif(1)
        function(x, ...) NextMethod()
      }, assign.env = env)
    }
    before <- .Random.seed
    expect_false(ale_transform_methods_safe(env))
    expect_identical(counter$reads, 0L)
    expect_identical(.Random.seed, before)
  }
})
