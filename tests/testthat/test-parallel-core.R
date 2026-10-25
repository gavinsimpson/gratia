test_that("one worker preserves numerical results and caller random-number state", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  withr::local_seed(814)
  calculate <- function() list(
    derivatives(m_gam, n = 4, interval = "simultaneous", n_sim = 30),
    partial_derivatives(su_m_bivar_te, n = 3),
    smooth_samples(m_gam, n_grid = 4, n_draws = 4),
    difference_smooths(su_m_factor_by, select = "s(x2)", n = 4),
    ale(m_1_smooth, "x0", data = quick_eg1, n_bins = 4, block_size = 80))
  before <- .Random.seed
  expected <- calculate()
  after <- .Random.seed
  gratia_test_workers(1L)
  assign(".Random.seed", before, envir = .GlobalEnv)
  expect_equal(calculate(), expected, tolerance = 1e-12)
  expect_identical(.Random.seed, after)
  expect_false(mirai::mirai(Sys.getpid())[] == Sys.getpid())
  expect_true(mirai::daemons_set())
  # A failure must reach the caller without silently retrying the serial path.
  setup <- mirai::everywhere({
    trace("calculate_smooth_derivative", where = asNamespace("gratia"),
      print = FALSE, tracer = quote(stop("deliberate core worker failure")))
    invisible(NULL)
  })
  invisible(mirai::collect_mirai(setup, options = ".stop"))
  expect_error(derivatives(m_gam, n = 3), "deliberate core worker failure")
  expect_true(mirai::daemons_set())
})
