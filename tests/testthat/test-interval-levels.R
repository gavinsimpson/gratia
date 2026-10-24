test_that("response derivative levels control normal bounds across wrappers", {
  for (case in list(list(su_m_quick_eg1, "x0"),
                   list(su_gamm_univar_4, "x0"), list(m_gamm4, "x0"),
                   list(m_scam, "x1"))) {
    args <- list(case[[1]], focal = case[[2]], n = 3,
      uncertainty = "delta", na.action = na.pass)
    default <- do.call(response_derivatives, args)
    expect_equal(default, do.call(response_derivatives, c(args, list(level = .95))))
    for (level in c(.5, .8)) {
      out <- do.call(response_derivatives, c(args, list(level = level)))
      expect_equal(out$.derivative, default$.derivative)
      expect_equal(out$.se, default$.se)
      crit <- qnorm((1 + level) / 2)
      expect_equal(out$.lower_ci, out$.derivative - crit * out$.se)
      expect_equal(out$.upper_ci, out$.derivative + crit * out$.se)
    }
  }
})

test_that("simulation levels use quantiles of fixed supplied slopes", {
  m <- response_delta_fixture(gaussian())
  slopes <- c(-3, -1, 0, 2, 7)
  draws <- matrix(rep(coef(m), each = length(slopes)), nrow = length(slopes))
  draws[, 2] <- slopes
  args <- list(m, focal = "x", n = 3, type = "central", eps = 1e-4,
    method = "user", draws = draws, na.action = na.pass)
  default <- do.call(response_derivatives, args)
  expect_equal(default, do.call(response_derivatives, c(args, list(level = .95))))
  for (level in c(.5, .8, .95)) {
    out <- do.call(response_derivatives, c(args, list(level = level)))
    expect_equal(out$.derivative, rep(median(slopes), 3), tolerance = 1e-8)
    expect_equal(out$.se, rep(sd(slopes), 3), tolerance = 1e-8)
    expect_equal(out$.derivative, default$.derivative)
    expect_equal(out$.se, default$.se)
    bounds <- quantile(slopes, c((1 - level) / 2, (1 + level) / 2))
    expect_equal(unname(out$.lower_ci), rep(unname(bounds[1]), 3), tolerance = 1e-8)
    expect_equal(unname(out$.upper_ci), rep(unname(bounds[2]), 3), tolerance = 1e-8)
  }
})

test_that("response derivatives reject misplaced interval names before prediction", {
  for (m in list(su_m_quick_eg1, su_gamm_univar_4, m_gamm4, m_scam)) {
    for (uncertainty in c("delta", "simulation")) {
      for (name in c("ci_level", "coverage")) {
        for (level in list(NULL, .8, .95)) {
          focal <- if (inherits(m, "scam")) "x1" else "x0"
          args <- c(list(m, focal = focal, n = 3, uncertainty = uncertainty),
            setNames(list(.8), name))
          if (!is.null(level)) args$level <- level
          expect_error(do.call(response_derivatives, args),
            paste0(name, ".*not supported.*Use.*level"))
        }
      }
      for (bad in list(NULL, numeric(), c(.8, .9), NA_real_, NaN, Inf, -Inf,
                       0, 1, -.1, 1.1, .8 + 0i, "0.8", TRUE)) {
        expect_error(response_derivatives(m, level = bad, uncertainty = uncertainty),
          "level.*one finite numeric value strictly between 0 and 1")
      }
    }
  }
})

test_that("interval handling retains prediction exclusions", {
  m <- m_1_smooth
  for (uncertainty in c("delta", "simulation")) {
    out <- response_derivatives(m, focal = "x0", n = 3, level = .8,
      uncertainty = uncertainty, exclude = "s(x0)", na.action = na.pass,
      n_sim = 10, seed = 2)
    expect_equal(out$.derivative, rep(0, 3), tolerance = 1e-8)
    expect_equal(out$.se, rep(0, 3), tolerance = 1e-8)
    expect_equal(unname(out$.lower_ci), rep(0, 3), tolerance = 1e-8)
    expect_equal(unname(out$.upper_ci), rep(0, 3), tolerance = 1e-8)
  }
})

test_that("comparison plot bounds use ci_level and preserve layout arguments", {
  cs <- compare_smooths(su_m_quick_eg1, su_m_quick_eg1_shrink,
    select = c("s(x0)", "s(x1)"), n = 5)
  default <- draw(cs)
  explicit <- draw(cs, ci_level = .95)
  for (level in c(.5, .8, .95)) {
    p <- draw(cs, ci_level = level, ncol = 2, nrow = 1, guides = "keep",
      widths = c(1, 2))
    expect_equal(p$patches$layout$ncol, 2)
    expect_equal(p$patches$layout$nrow, 1)
    expect_equal(p$patches$layout$guides, "keep")
    expect_equal(p$patches$layout$widths, c(1, 2))
    for (i in 1:2) {
      d <- p[[i]]$data
      expect_equal(d$.estimate, default[[i]]$data$.estimate)
      expect_equal(d$.se, default[[i]]$data$.se)
      crit <- qnorm((1 + level) / 2)
      expect_equal(d$.lower_ci, d$.estimate - crit * d$.se)
      expect_equal(d$.upper_ci, d$.estimate + crit * d$.se)
      expect_equal(default[[i]]$data, explicit[[i]]$data)
      built <- ggplot_build(p[[i]])
      expect_equal(sort(built$data[[1]]$ymin), sort(d$.lower_ci))
      expect_equal(sort(built$data[[1]]$ymax), sort(d$.upper_ci))
    }
  }
})

test_that("comparison plots reject invalid and misplaced levels before plotting", {
  # No plot data: these errors must precede splitting or constructing plots.
  cs <- structure(list(), class = "compare_smooths")
  for (name in c("coverage", "level")) {
    for (level in list(NULL, .8, .95)) {
      args <- c(list(cs), setNames(list(.8), name))
      if (!is.null(level)) args$ci_level <- level
      expect_error(do.call(draw, args),
        paste0(name, ".*not supported.*Use.*ci_level"))
    }
  }
  for (bad in list(NULL, numeric(), c(.8, .9), NA_real_, NaN, Inf, -Inf,
                   0, 1, -.1, 1.1, .8 + 0i, "0.8", TRUE)) {
    expect_error(draw(cs, ci_level = bad),
      "ci_level.*one finite numeric value strictly between 0 and 1")
  }
})
