test_that("named smooth extraction preserves exact matches, order and identities", {
  for (model in list(m_tiny_eg1, m_gamm, m_gamm4, m_scam)) {
    labels <- smooths(model)
    expected <- get_smooths_by_id(model, seq_along(labels))
    expect_identical(n_smooths(model), length(labels))
    for (i in seq_along(labels)) {
      expect_identical(get_smooth(model, labels[i]), expected[[i]])
    }
    expect_length(get_smooth(model, sub("\\(.*", "", labels[1])), 0L)
    expect_length(get_smooth(model, "not a smooth"), 0L)
  }
  model <- m_tiny_eg1
  first <- model$smooth[[2]]
  second <- model$smooth[[1]]
  second$label <- first$label
  model$smooth <- list(first, model$smooth[[3]], second)
  expect_identical(get_smooth(model, first$label), list(first, second))
  model <- m_only_para
  expect_identical(n_smooths(model), 0L)
  expect_length(get_smooth(model, "s(x)"), 0L)
})

test_that("glmmTMB named extraction keeps its indexed smooths", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  model <- glmmtmb_gaussian_fixture()$model
  expect_identical(get_smooth(model, "s(x)"), get_smooths_by_id(model, 1)[[1]])
  expect_identical(n_smooths(model), 1L)
  expect_length(get_smooth(model, "x"), 0L)
})

test_that("add_fitted defaults and forwarding agree with returned predictions", {
  expect_identical(formals(add_fitted)$value, ".fitted")
  expect_identical(formals(add_fitted.gam)$value, ".fitted")
  cases <- list(
    list(model = m_tiny_eg1, data = tiny_eg1[1:3, ]),
    list(model = m_gamm4, data = su_eg1[1:3, ]),
    list(model = m_scam, data = scam_dat[1:3, ])
  )
  for (case in cases) {
    model <- case$model
    data <- case$data
    native <- if (is_gamm4(model)) gamm4_gam(model) else model
    expected <- as.numeric(predict(native, newdata = data, type = "link"))
    out <- add_fitted(data, model, type = "link", se.fit = TRUE)
    expect_named(out, c(names(data), ".fitted"))
    expect_equal(out$.fitted, expected)
    out <- add_fitted(data, model, value = "prediction", type = "link")
    expect_named(out, c(names(data), "prediction"))
    expect_equal(out$prediction, expected)
  }
  for (model in list(m_tiny_eg1, intercept_test_model("gam"))) {
    data <- model$model[1:3, , drop = FALSE]
    native <- predict(model, newdata = data, type = "terms")
    out <- suppressMessages(add_fitted(data, model, value = "prediction", type = "terms"))
    expect_equal(out$.constant, rep(unname(attr(native, "constant") %||% 0), 3))
    columns <- ncol(data) + 1L + seq_len(ncol(native))
    expect_equal(unname(as.matrix(out[columns])), unname(native), ignore_attr = TRUE)
    expect_false("prediction" %in% names(out))
  }
})

test_that("legacy derivative intervals retain valid numerical results", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_preserve_seed()
  fd <- fderiv(m_tiny_eg1, newdata = tiny_eg1[1:3, ], term = "x1")
  derivative <- fd$derivatives[["s(x1)"]]
  for (coverage in c(1e-6, .5, .8, .95, 1 - 1e-6)) {
    ci <- confint(fd, level = coverage)
    critical <- qnorm((1 + coverage) / 2)
    expect_identical(ci$term, rep("s(x1)", 3))
    expect_equal(ci$est, as.numeric(derivative$deriv))
    expect_equal(ci$lower, as.numeric(derivative$deriv - critical * derivative$se.deriv))
    expect_equal(ci$upper, as.numeric(derivative$deriv + critical * derivative$se.deriv))
  }
  expect_warning(vector_ci <- confint(fd, level = c(.8, NA_real_)),
    "Using the first only")
  expect_equal(vector_ci, confint(fd, level = .8))
  ci <- confint(fd, level = .8, type = "simultaneous", nsim = 25L)
  expect_identical(ci$term, rep("s(x1)", 3))
  expect_equal(ci$est, as.numeric(derivative$deriv))
  expect_true(all(is.finite(ci$lower) & ci$lower < ci$est))
  expect_true(all(is.finite(ci$upper) & ci$upper > ci$est))
  expect_equal(ci$est - ci$lower, ci$upper - ci$est)
})

test_that("legacy derivative levels are checked before interval calculations", {
  local_mocked_bindings(
    confidence = function(...) stop("pointwise calculation reached"),
    simultaneous = function(...) stop("simultaneous calculation reached")
  )
  invalid <- list(-Inf, -1, 0, 1, 1.5, Inf, NA_real_, NaN,
    NULL, numeric(), "0.8", TRUE, .8 + 0i)
  fd <- structure(list(terms = "s(x1)"), class = "fderiv")
  for (type in c("confidence", "simultaneous")) {
    for (coverage in invalid) {
      expect_error(confint(fd, level = coverage, type = type),
        "`level` must be a finite number strictly between 0 and 1.", fixed = TRUE)
    }
    expect_warning(expect_error(confint(fd, level = c(NA_real_, .8), type = type),
      "`level` must be"), "Using the first only")
  }
})

test_that("simulation counts are checked before RNG and model work", {
  withr::local_seed(82)
  models <- list(m_tiny_eg1, gfam_model, m_gamm, sw)
  state <- .Random.seed
  expect_identical(sampling_count(.Machine$integer.max, "nsim"), .Machine$integer.max)
  local_mocked_bindings(
    with_model_envir = function(...) stop("model calculation reached"),
    marginal_context = function(...) stop("marginal calculation reached")
  )
  invalid <- list(0, -1, .5, Inf, -Inf, NA_real_, NaN, NULL, numeric(),
    c(1, 2), "2", TRUE, 1 + 0i, .Machine$integer.max + 1)
  for (count in invalid) {
    for (model in models) {
      expect_error(simulate(model, nsim = count, seed = 12),
        "'nsim' must be a positive integer.", fixed = TRUE)
      expect_identical(.Random.seed, state)
    }
    expect_error(simulate(m_tiny_eg1, nsim = count, marginalise = "s(x0)"),
      "'nsim' must be a positive integer.", fixed = TRUE)
  }
  rm(".Random.seed", envir = .GlobalEnv)
  expect_error(simulate(m_tiny_eg1, nsim = 0), "'nsim' must be")
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("numeric whole simulation counts preserve output and metadata", {
  withr::local_preserve_seed()
  for (model in list(m_tiny_eg1, gfam_model, m_gamm, sw)) {
    out <- simulate(model, nsim = 2, seed = 42)
    expect_identical(out, simulate(model, nsim = 2L, seed = 42))
    expect_s3_class(out, "simulate_gratia")
    expect_identical(ncol(out), 2L)
    expect_identical(nrow(out), nrow(model.frame(if (is_gamm(model)) model$gam else model)))
    expect_identical(attr(out, "seed"), structure(42, kind = as.list(RNGkind())))
    if (!inherits(model, "scam")) {
      expect_named(out, c("sim_1", "sim_2"))
      expect_identical(attr(out, "row_ids"),
        if (is_gfam_family(family(model))) NULL else seq_len(nrow(out)))
    }
  }
  data <- tiny_eg1[1:3, ]
  mu <- as.numeric(predict(m_tiny_eg1, newdata = data, type = "response"))
  expected <- withr::with_seed(17, rnorm(3, mu, sqrt(m_tiny_eg1$sig2 / 2)))
  out <- simulate(m_tiny_eg1, nsim = 1, data = data, weights = 2, seed = 17)
  expect_equal(out$sim_1, expected)
  expect_identical(attr(out, "row_ids"), 1:3)
  data$x0[2] <- NA_real_
  out <- simulate(m_tiny_eg1, nsim = 2, data = data, seed = 17)
  expect_true(all(is.na(out[2, ])))
  expect_identical(attr(out, "row_ids"), 1:3)
  out <- simulate(m_mvn, nsim = 2, seed = 17)
  expect_named(out, c(".yvar", "sim_1", "sim_2"))
  expect_identical(dim(out), c(600L, 3L))
  expect_identical(out$.yvar, rep(c("response1", "response2"), each = 300))
})

test_that("explicit simulation seeds restore existing and absent random states", {
  withr::local_preserve_seed()
  # Force lazy model bindings before deliberately removing the global state.
  models <- list(m_tiny_eg1, gfam_model, m_gamm, sw)
  for (model in models) {
    set.seed(72)
    state <- .Random.seed
    first <- simulate(model, nsim = 2, seed = 42)
    expect_identical(.Random.seed, state)
    expect_identical(simulate(model, nsim = 2, seed = 42), first)
    expect_identical(.Random.seed, state)
    rm(".Random.seed", envir = .GlobalEnv)
    expect_identical(simulate(model, nsim = 2, seed = 42), first)
    expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    expect_identical(simulate(model, nsim = 2, seed = 42), first)
    expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
  }
})

test_that("explicit simulation seeds restore state after a response RNG error", {
  withr::local_preserve_seed()
  model <- m_tiny_eg1
  local_mocked_bindings(choose_rd_fun = function(...) {
    function(...) {
      runif(1)
      stop("response draw failed")
    }
  })
  set.seed(72)
  state <- .Random.seed
  expect_error(simulate(model, seed = 42), "response draw failed")
  expect_identical(.Random.seed, state)
  rm(".Random.seed", envir = .GlobalEnv)
  expect_error(simulate(model, seed = 42), "response draw failed")
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("unseeded simulation advances or initializes random state", {
  withr::local_seed(73)
  model <- m_tiny_eg1
  state <- .Random.seed
  first <- simulate(model, nsim = 2)
  expect_identical(attr(first, "seed"), state)
  expect_false(identical(.Random.seed, state))
  next_state <- .Random.seed
  second <- simulate(model, nsim = 2, seed = NULL)
  expect_identical(attr(second, "seed"), next_state)
  expect_false(identical(first$sim_1, second$sim_1))
  rm(".Random.seed", envir = .GlobalEnv)
  out <- simulate(model, nsim = 2)
  expect_true(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
  expect_type(attr(out, "seed"), "integer")
  expect_false(identical(attr(out, "seed"), .Random.seed))
})
