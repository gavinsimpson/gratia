test_that("prepared worker coordinates retain column metadata without fitting environments", {
  environment <- new.env(parent = baseenv())
  environment$sentinel <- raw(500000)
  data <- tibble::tibble(x = c(.4, .1, .4),
    f = factor(c("b", "a", "b")), matrix = I(matrix(1:6, ncol = 2)))
  contrasts(data$f) <- stats::contr.sum(2)
  attr(data, "terms") <- stats::terms(stats::as.formula("~ x + f", env = environment))
  attr(data, "gratia.envir") <- environment
  compact <- smooth_task_data(data)
  expect_identical(class(compact), class(data))
  expect_identical(rownames(compact), rownames(data))
  expect_identical(lapply(compact, identity), lapply(data, identity))
  expect_null(attr(compact, "terms"))
  expect_null(attr(compact, "gratia.envir"))
  expect_gt(length(serialize(data, NULL)), 500000)
  expect_lt(length(serialize(compact, NULL)), 10000)
})

test_that("worker coordinates normalize grouped and other data-frame subclasses", {
  data <- tibble::tibble(x = c(.4, .1, .4), z = 1:3)
  inputs <- list(dplyr::group_by(data, x), dplyr::rowwise(data))
  for (input in inputs) {
    compact <- smooth_task_data(input)
    expect_identical(class(compact), class(data))
    expect_identical(lapply(compact, identity), lapply(data, identity))
    expect_identical(compact["x"]$x, data$x)
    expect_null(attr(compact, "groups"))
  }
  custom <- structure(as.data.frame(data), class = c("custom_frame", "data.frame"))
  expect_identical(class(smooth_task_data(custom)), "data.frame")
})

test_that("random-effect margins release fitting environments without changing predictions", {
  withr::local_seed(348)
  # The formula environment contains both this model and unrelated local data.
  sentinel <- raw(500000)
  data <- data.frame(x = runif(90), z = runif(90),
    f = factor(rep(letters[1:3], 30)))
  data$y <- sin(data$x) + rnorm(90)
  model <- mgcv::gam(y ~ te(x, f, bs = c("tp", "re"), k = c(4, 3)) +
    s(z, k = 4), data = data)
  smooth <- model$smooth[[1]]
  original_environment <- environment(smooth$margin[[2]]$form)
  compact <- smooth_task_smooth(smooth)
  expect_identical(environment(smooth$margin[[2]]$form), original_environment)
  expect_identical(environment(compact$margin[[2]]$form), baseenv())
  expect_gt(length(serialize(smooth, NULL)), length(sentinel))
  expect_lt(length(serialize(compact, NULL)), 50000)
  rows <- data[c(7, 2, 7, 10), ]
  expect_equal(mgcv::PredictMat(compact, rows), mgcv::PredictMat(smooth, rows))
  expect_identical(smooth_coef_indices(compact), smooth_coef_indices(smooth))

  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  withr::defer(mirai::daemons(0))
  mirai::daemons(0)
  draws <- post_draws(model, n_draws = 8, seed = 348)
  supplied <- list(dplyr::group_by(tibble::as_tibble(rows), x),
    dplyr::rowwise(tibble::as_tibble(rows)))
  sample_rows <- function(data) smooth_samples(model, data = data,
    method = "user", draws = draws, seed = 348)
  reference_samples <- sample_rows(rows)
  for (input in supplied) expect_equal(sample_rows(input), reference_samples)
  tasks <- lapply(model$smooth, function(sm) list(
    sm = smooth_task_smooth(sm),
    data = smooth_task_data(rows[, terms_in_smooth(sm), drop = FALSE]),
    model = smooth_matrix_model(model),
    betas = draws[, smooth_coef_indices(sm), drop = FALSE], label = sm$label))
  reference <- lapply(tasks, function(task) do.call(calculate_smooth_samples, task))
  gratia_test_workers(1L)
  expect_equal(smooth_task_map(tasks, "calculate_smooth_samples"), reference,
    tolerance = 1e-12)
  for (input in supplied) {
    expect_equal(sample_rows(input), reference_samples, tolerance = 1e-12)
  }
  expect_true(mirai::daemons_set())
})

test_that("custom random-effect formulas retain functions from their environment", {
  environment <- new.env(parent = baseenv())
  environment$local_function <- function(x) factor(x)
  smooth <- structure(list(term = "x",
    form = stats::as.formula("~ local_function(x) - 1", env = environment)),
    class = "random.effect")
  compact <- smooth_task_smooth(smooth)
  expect_identical(environment(compact$form), environment)
})

test_that("smooth metadata preserves atomic margin descriptions", {
  smooth <- structure(list(term = c("x", "z"), margin = c(2L, 3L)),
    class = "mgcv.smooth")
  expect_identical(smooth_task_smooth(smooth), smooth)
  expect_identical(smooth_task_smooth(c(2L, 3L)), c(2L, 3L))
})
