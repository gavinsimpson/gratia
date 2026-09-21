test_that("Tweedie diagnostics default to simulation without computing quantiles", {
  withr::local_seed(410)
  dat <- data.frame(x = seq(0, 1, length.out = 30))
  dat$y <- mgcv::rTweedie(exp(dat$x), p = 1.5, phi = 1)
  local_mocked_bindings(
    qq_uniform = function(...) stop("Unexpected numerical quantile calculation")
  )

  for (fam in list(mgcv::tw(), mgcv::Tweedie(p = 1.5))) {
    m <- mgcv::gam(y ~ x, data = dat, family = fam, method = "REML")
    for (plot_fun in list(qq_plot, worm_plot)) {
      automatic <- plot_fun(m, n_simulate = 5, seed = 42)
      explicit <- plot_fun(m, method = "simulate", n_simulate = 5, seed = 42)
      expect_equal(automatic$labels$subtitle, "Method: simulate")
      expect_equal(automatic$data, explicit$data)
      expect_equal(plot_fun(m, method = NULL, n_simulate = 5, seed = 42)$data,
        explicit$data)
      expect_equal(plot_fun(m, method = "normal")$labels$subtitle,
        "Method: normal")
    }
    for (use_worm in c(FALSE, TRUE)) {
      automatic <- appraise(m, use_worm = use_worm, n_simulate = 5, seed = 42)
      explicit <- appraise(m, use_worm = use_worm, method = "simulate",
        n_simulate = 5, seed = 42)
      expect_equal(automatic[[1]]$labels$subtitle, "Method: simulate")
      expect_equal(automatic[[1]]$data, explicit[[1]]$data)
      expect_error(appraise(m, use_worm = use_worm, method = "uniform"),
        "Unexpected numerical quantile calculation")
    }
    for (plot_fun in list(qq_plot, worm_plot)) {
      expect_error(plot_fun(m, method = "uniform"),
        "Unexpected numerical quantile calculation")
      expect_error(plot_fun(m, method = "invalid"), "'arg' should be one of")
    }
  }
})

test_that("automatic methods preserve other families and availability fallbacks", {
  withr::local_seed(410)
  dat <- data.frame(x = seq_len(20), y = rnorm(20))
  m <- mgcv::gam(y ~ x, data = dat)
  for (plot_fun in list(qq_plot, worm_plot)) {
    automatic <- plot_fun(m, seed = 42)
    explicit <- plot_fun(m, method = "uniform", seed = 42)
    expect_equal(automatic$labels$subtitle, "Method: uniform")
    expect_equal(automatic$data, explicit$data)
    expect_message(direct <- plot_fun(m, method = "direct", seed = 42),
      "deprecated")
    expect_equal(direct$data, explicit$data)
  }
  for (use_worm in c(FALSE, TRUE)) {
    expect_equal(appraise(m, use_worm = use_worm)[[1]]$labels$subtitle,
      "Method: uniform")
  }

  # A family without quantiles still falls back to random generation.
  local_mocked_bindings(fix_family_qf = function(family) {
    family$qf <- NULL
    family
  })
  for (plot_fun in list(qq_plot, worm_plot)) {
    expect_equal(plot_fun(m, n_simulate = 5)$labels$subtitle, "Method: simulate")
  }
})
