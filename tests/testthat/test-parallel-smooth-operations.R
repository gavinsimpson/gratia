skip_if_not_installed("mirai")
skip_if_not_installed("carrier")

parallel_tensor_fixture <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      model <<- withr::with_seed(349, mgcv::gam(
        y ~ te(x0, x1, k = c(3, 3)) + te(x0, x2, k = c(3, 3)),
        data = quick_eg1))
    }
    model
  }
})

# Compare all outputs, including attributes, and the caller's final RNG state.
# Starting the pool after the reference call exercises the serial fallback.
expect_smooth_workers <- function(calculate) {
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  set.seed(348)
  reference <- calculate()
  reference_state <- .Random.seed
  counts <- if (full_test_run()) c(1L, 2L) else 1L
  for (n in counts) {
    gratia_test_workers(n)
    set.seed(348)
    actual <- calculate()
    expect_equal(actual, reference, tolerance = 1e-12)
    expect_identical(.Random.seed, reference_state)
    expect_true(mirai::daemons_set())
  }
  invisible(reference)
}

test_that("term workers preserve derivatives, selections and supplied row order", {
  withr::local_seed(348)
  nd <- quick_eg1[c(7, 2, 7, 10), ]
  calculate <- function() {
    list(
      derivatives(m_gam, data = nd, select = c("s(x0)", "s(x2)")),
      derivatives(su_m_factor_by, n = 7, interval = "simultaneous", n_sim = 100),
      derivatives(m_gam, select = "s(x0)", n = 5, order = 2),
      derivatives(m_gam, data = nd[FALSE, ], select = c("s(x0)", "s(x2)")),
      derivatives(m_bam, n = 5),
      derivatives(m_gamm4, n = 5),
      derivatives(m_scam, n = 5)
    )
  }
  reference <- expect_smooth_workers(calculate)
  expect_equal(reference[[1]]$x0[1:4], nd$x0)
  expect_equal(reference[[1]]$x2[5:8], nd$x2)
  expect_equal(nrow(reference[[4]]), 0L)
})

test_that("partial derivative workers preserve focal variables and intervals", {
  withr::local_seed(349)
  # Two terms sharing the same varying coordinate allow supplied slice data.
  model <- parallel_tensor_fixture()
  nd <- data.frame(x0 = c(.6, .2, .6, .4), x1 = .3, x2 = .5)
  reference <- expect_smooth_workers(function() list(
    partial_derivatives(model, n = 7),
    partial_derivatives(model, data = nd, focal = c("x0", "x0"),
      interval = "simultaneous", n_sim = 100, seed = 27),
    partial_derivatives(model, n = 5, order = 2, focal = c("x1", "x2"),
      interval = "simultaneous", n_sim = 100),
    partial_derivatives(model, select = "te(x0,x1)", n = 5)
  ))
  expect_equal(reference[[2]]$x0, rep(nd$x0, 2))
})

test_that("parallel calculations evaluate local expressions in the caller", {
  d <- transformed_data()
  shift <- .7
  fun <- function(x) log(x + shift)
  model <- mgcv::gam(y ~ s(fun(x), k = 5) + s(sqrt(z), k = 5), data = d)
  tensor <- mgcv::gam(y ~ te(fun(x), z, k = c(3, 3)) +
    te(fun(x), x, k = c(3, 3)), data = d)
  factor_model <- mgcv::gam(y ~ g + s(fun(x), by = g, k = 5), data = d)
  comparison_data <- expand.grid(x = c(1.5, 3, 4), g = levels(d$g), z = 2)
  env <- environment()
  nd <- d[c(9, 2, 9, 1), ]
  expect_smooth_workers(function() list(
    derivatives(model, data = nd, envir = env, wrt = "covariate",
      focal = c("x", "z"), interval = "simultaneous", n_sim = 100),
    partial_derivatives(tensor, n = 5, envir = env,
      interval = "simultaneous", n_sim = 100),
    smooth_samples(model, data = nd, n_draws = 12, seed = 19, envir = env),
    difference_smooths(factor_model, select = "s(fun(x))",
      data = comparison_data, envir = env, interval = "simultaneous",
      n_sim = 100, seed = 19)
  ))
})

test_that("smooth workers reuse joint draws and preserve chain diagnostics", {
  skip_if_not_installed("posterior")
  withr::local_seed(350)
  nd <- quick_eg1[c(7, 2, 7, 10), ]
  draws <- suppressWarnings(post_draws(m_gam, n_draws = 40, chains = 2, seed = 54,
    diagnostics = TRUE))
  # Supplied identities need not match row positions or arrive in chain order.
  metadata <- attr(draws, "sampling")
  metadata$draw_ids$.draw <- 101:140
  indices <- rev(seq_len(nrow(draws)))
  draws <- draws[indices, , drop = FALSE]
  metadata$draw_ids <- metadata$draw_ids[indices, ]
  metadata$coefficient_draws <- metadata$coefficient_draws[indices, , drop = FALSE]
  attr(draws, "sampling") <- metadata
  reference <- expect_smooth_workers(function() list(
    smooth_samples(m_gam, method = "user", draws = draws, data = nd, seed = 55),
    smooth_samples(m_gam, n_draws = 12, chains = 2, n_grid = 5, seed = 54),
    smooth_samples(m_gam, n_draws = 12, chains = 2, n_grid = 5),
    smooth_samples(su_m_factor_by, n_draws = 12, n_grid = 5, seed = 54),
    smooth_samples(m_gamm4, n_draws = 12, n_grid = 5, seed = 54),
    smooth_samples(m_scam, n_draws = 12, n_grid = 5, seed = 54),
    smooth_samples(m_gam, select = "s(x0)", method = "user", draws = draws,
      data = nd, seed = 55)
  ))
  sims <- reference[[1]]
  expect_identical(sampling_diagnostics(sims), sampling_diagnostics(draws))
  expect_identical(attr(sims, "sampling"), metadata)
  expect_identical(sort(unique(sims$.chain)), 1:2)
  expect_identical(sort(unique(sims$.draw)), 101:140)
  rows <- match(sims$.draw, metadata$draw_ids$.draw)
  expect_identical(sims$.chain, metadata$draw_ids$.chain[rows])
  expect_identical(sims$.iteration, metadata$draw_ids$.iteration[rows])
  for (sm in m_gam$smooth) {
    expected <- mgcv::PredictMat(sm, nd) %*% t(draws[, smooth_coef_indices(sm)])
    actual <- sims$.value[sims$.term == sm$label]
    expect_equal(actual, as.vector(t(expected)), tolerance = 1e-12)
  }
  expect_equal(reference[[7]]$.value, sims$.value[sims$.term == "s(x0)"])
})

test_that("comparison workers preserve pair order, alignment and uncertainty", {
  withr::local_seed(351)
  model <- su_m_factor_by
  nd <- expand.grid(x2 = c(.8, .2, .5), fac = levels(su_eg4$fac), x0 = .4)
  nd <- nd[c(3, 1, 2, 6, 4, 5, 9, 7, 8), ]
  reference <- expect_smooth_workers(function() list(
    difference_smooths(model, select = "s(x2)", data = nd),
    difference_smooths(model, select = "s(x2)", data = nd, group_means = TRUE,
      interval = "simultaneous", n_sim = 100, seed = 32),
    difference_smooths(model, select = c("s(x2):fac1", "s(x2):fac3"),
      n = 5, interval = "simultaneous", n_sim = 100, partial_match = FALSE),
    difference_smooths(su_m_factor_by_gamm4, select = "s(x2)", n = 5)
  ))
  expect_equal(reference[[1]]$x2, rep(nd$x2[1:3], 3))
  expect_equal(unique(paste(reference[[1]]$.level_1, reference[[1]]$.level_2)),
    c("1 2", "1 3", "2 3"))
})

test_that("term calculations run in worker processes and failures propagate", {
  withr::defer(mirai::daemons(0))
  gratia_test_workers(1L)
  setup <- mirai::everywhere({
    .GlobalEnv$.gratia_term_calls <- character()
    for (worker in c("calculate_smooth_derivative", "calculate_smooth_samples",
        "calculate_difference")) {
      trace(worker, where = asNamespace("gratia"), print = FALSE,
        tracer = bquote({
          .GlobalEnv$.gratia_term_calls <- c(.GlobalEnv$.gratia_term_calls, .(worker))
        }))
    }
    invisible(NULL)
  })
  invisible(mirai::collect_mirai(setup, options = ".stop"))
  derivatives(m_gam, n = 3)
  before_partial <- mirai::mirai(sum(.GlobalEnv$.gratia_term_calls ==
    "calculate_smooth_derivative"))[]
  partial_derivatives(parallel_tensor_fixture(), n = 3)
  after_partial <- mirai::mirai(sum(.GlobalEnv$.gratia_term_calls ==
    "calculate_smooth_derivative"))[]
  expect_equal(after_partial - before_partial, 2)
  smooth_samples(m_gam, n_draws = 4, n_grid = 3, seed = 8)
  difference_smooths(su_m_factor_by, select = "s(x2)", n = 3)
  record <- mirai::mirai(list(pid = Sys.getpid(), calls = .GlobalEnv$.gratia_term_calls))[]
  expect_false(record$pid == Sys.getpid())
  expect_setequal(record$calls,
    c("calculate_smooth_derivative", "calculate_smooth_samples", "calculate_difference"))
  expect_length(smooth_task_map(list(), "calculate_difference"), 0)
  expect_error(smooth_task_map(rep(list(list()), 2), "calculate_smooth_samples"),
    'argument .* is missing')
  # The caller's namespace is unchanged, so a silent serial retry would succeed.
  setup <- mirai::everywhere({
    trace("calculate_smooth_derivative", where = asNamespace("gratia"),
      print = FALSE, tracer = quote(stop("deliberate worker failure")))
    invisible(NULL)
  })
  invisible(mirai::collect_mirai(setup, options = ".stop"))
  expect_error(derivatives(m_gam, n = 3), "deliberate worker failure")
  expect_true(mirai::daemons_set())
})

test_that("parallel calculations preserve an alternative caller RNG kind", {
  withr::local_preserve_seed()
  kind <- RNGkind()
  withr::defer(do.call(RNGkind, as.list(kind)))
  RNGkind("L'Ecuyer-CMRG")
  set.seed(914)
  caller_kind <- RNGkind()
  caller_state <- .Random.seed
  explicit_calls <- list(
    function() partial_derivatives(parallel_tensor_fixture(), n = 3,
      interval = "simultaneous", n_sim = 50, seed = 31),
    function() smooth_samples(m_gam, n_draws = 4, n_grid = 3, seed = 31),
    function() difference_smooths(su_m_factor_by, select = "s(x2)", n = 3,
      interval = "simultaneous", n_sim = 50, seed = 31)
  )
  withr::defer(mirai::daemons(0))
  for (workers in 0:1) {
    if (workers == 0L) mirai::daemons(0) else gratia_test_workers(workers)
    # Worker startup is outside the state-restoration assertion.
    assign(".Random.seed", caller_state, envir = .GlobalEnv)
    for (calculate in explicit_calls) {
      calculate()
      expect_identical(RNGkind(), caller_kind)
      expect_identical(.Random.seed, caller_state)
    }
  }
  expect_smooth_workers(function() derivatives(m_gam, n = 3,
    interval = "simultaneous", n_sim = 50))
  expect_identical(RNGkind(), caller_kind)
})

test_that("empty and deterministic derivatives do not consume interval draws", {
  withr::local_seed(354)
  model <- mgcv::gam(y ~ s(x0, by = x1, k = 5) + s(x2, k = 5),
    data = quick_eg1)
  nd <- quick_eg1[c(3, 7, 9), ]
  nd$x1 <- 0
  expect_smooth_workers(function() {
    before <- .Random.seed
    empty <- derivatives(model, data = nd[FALSE, ],
      interval = "simultaneous", n_sim = 50)
    expect_identical(.Random.seed, before)
    combined <- derivatives(model, data = nd,
      interval = "simultaneous", n_sim = 50)
    after <- .Random.seed
    assign(".Random.seed", before, envir = .GlobalEnv)
    selected <- derivatives(model, select = "s(x2)", data = nd,
      interval = "simultaneous", n_sim = 50)
    expect_identical(.Random.seed, after)
    selected_rows <- combined$.smooth == "s(x2)"
    for (column in c(".derivative", ".se", ".crit", ".lower_ci", ".upper_ci")) {
      expect_equal(combined[[column]][selected_rows], selected[[column]])
    }
    list(empty, combined)
  })
})

test_that("explicit seeds restore an initially absent caller RNG state", {
  withr::local_preserve_seed()
  if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  smooth_samples(m_gam, n_draws = 4, n_grid = 3, seed = 31)
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
  partial_derivatives(su_m_bivar_te, n = 3, seed = 31)
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
  difference_smooths(su_m_factor_by, select = "s(x2)", n = 3,
    interval = "simultaneous", n_sim = 50, seed = 31)
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("MH sampling finishes before term workers and retains its controls", {
  skip_on_cran()
  withr::local_seed(352)
  expect_smooth_workers(function() {
    result <- smooth_samples(su_m_quick_eg1,
      n_draws = 12, chains = 2, method = "mh", burnin = 3,
      n_cores = 2, n_grid = 3, seed = 54)
    sampling <- attr(result, "sampling")
    expect_true(all(is.finite(sampling$chains$elapsed)))
    expect_true(all(sampling$chains$elapsed >= 0))
    # Elapsed seconds depend on scheduling; compare all other sampling fields.
    sampling$chains$elapsed <- NULL
    attr(result, "sampling") <- sampling
    result
  })
})

test_that("bootstrap derivative workers retain smoothing-bias correction", {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  withr::local_seed(353)
  model <- mgcv::scasm(y ~ s(x0, bs = "sc", xt = "m+", k = 5) +
    s(x1, bs = "sc", xt = "m+", k = 5), data = quick_eg1, bs = 6)
  expect_smooth_workers(function() derivatives(model, n = 5))
  attr(model$bs, "svar.only") <- NULL
  expect_smooth_workers(function() derivatives(model, n = 5))
})
