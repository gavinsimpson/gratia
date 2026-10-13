skip_if_not_installed("sdmTMB", minimum_version = "1.1.0")

sdmtmb_test_data <- function() {
  withr::local_seed(823)
  d <- data.frame(
    x = runif(240, .1, 1), z = runif(240),
    fac = factor(rep(letters[1:3], 80)), g = factor(rep(1:12, each = 20)),
    X = runif(240), Y = runif(240), year = rep(1:3, 80)
  )
  d$y <- 1 + sin(6 * d$x) + cos(6 * d$z) + .4 * as.integer(d$fac) +
    rnorm(12, sd = .5)[d$g] + rnorm(240, sd = .3)
  d$positive <- ifelse(runif(240) < plogis(sin(5 * d$x)),
    exp(1 + sin(6 * d$x) + .4 * d$z + rnorm(240, sd = .3)), 0
  )
  d
}

# The same tests run against the release and development backends in CI.
sdmtmb_test_fit <- function(formula, data, ...) {
  backend <- Sys.getenv("GRATIA_SDMTMB_BACKEND")
  control <- if (nzchar(backend)) {
    sdmTMB::sdmTMBcontrol(backend = backend)
  } else {
    sdmTMB::sdmTMBcontrol()
  }
  sdmTMB::sdmTMB(formula, data = data, spatial = "off", control = control, ...)
}

sdmtmb_expect_native <- function(m, data = m$data, offset = NULL) {
  a <- sdmtmb_smooth_model(m)
  for (i in seq_along(a$models)) {
    local <- a$models[[i]]
    X <- model.matrix(delete.response(local$pterms), data,
      contrasts.arg = local$contrasts
    )
    native_data <- data
    for (sm in local$smooth) {
      sm_data <- prepare_smooth_data(local, sm, data)
      X <- cbind(X, mgcv::PredictMat(sm, sm_data))
      for (nm in terms_in_smooth(sm)) native_data[[nm]] <- sm_data[[nm]]
    }
    p <- predict(m,
      newdata = native_data, type = "link", se_fit = TRUE,
      re_form = NA, re_form_iid = NA, model = i, offset = offset
    )
    adjustment <- if (is.null(offset)) 0 else offset
    expect_equal(unname(drop(X %*% coef(local))), p$est - adjustment,
      tolerance = 1e-6
    )
    expect_equal(unname(sqrt(rowSums((X %*% local$Vp) * X))), p$est_se,
      tolerance = 1e-6
    )
  }
  invisible(a)
}

sdmtmb_expect_build <- function(plots) {
  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_silent(b <- ggplot2::ggplot_build(p))
    expect_gt(length(b$data), 0L)
  }
}

test_that("sdmTMB effects reproduce native ML and REML predictions", {
  d <- sdmtmb_test_data()
  for (reml in c(FALSE, TRUE)) {
    m <- sdmtmb_test_fit(y ~ z + s(x, k = 6), d, reml = reml)
    a <- sdmtmb_expect_native(m, d[1:12, ])
    expect_s3_class(a, "gratia_mixed_smooth")
    sm <- a$smooth[[1L]]
    expect_equal(unname(smooth_coefs(m, "s(x)")),
      unname(coef(a)[smooth_coef_indices(sm)]))
    e <- smooth_estimates(m, data = d[1:12, ], overall_uncertainty = FALSE)
    X <- mgcv::PredictMat(sm, d[1:12, ])
    take <- smooth_coef_indices(sm)
    expect_equal(e$.estimate, unname(drop(X %*% coef(a)[take])))
    expect_equal(e$.se, unname(sqrt(rowSums((X %*% a$Vp[take, take]) * X))))
    # Independent dense inverse on a small fit checks marginal covariance,
    # including covariance between fixed and penalized coefficients.
    V <- as.matrix(solve(m$sd_report$jointPrecision))
    idx <- a$joint_indices
    expect_equal(unname(a$Vp), unname(a$coefficient_transform %*%
      V[idx, idx] %*% t(a$coefficient_transform)), tolerance = 1e-7)
  }
  m <- sdmtmb_test_fit(y ~ s(x, k = 6), d)
  p <- predict(m, newdata = d, se_fit = TRUE, re_form = NA)
  expect_equal(smooth_estimates(m, data = d)$.se, p$est_se, tolerance = 1e-6)
})

test_that("sdmTMB smooths handle multiple penalties and by variables", {
  d <- sdmtmb_test_data()
  formulas <- list(
    y ~ s(x, k = 5) + s(z, k = 6),
    y ~ fac + s(x, by = fac, k = 5),
    y ~ z + s(x, by = z, k = 5, m = c(2, 0)),
    y ~ s(log(x), k = 5), y ~ s(x, z, k = 12),
    y ~ t2(x, z, k = c(4, 4)), y ~ s(x, bs = "ts", k = 5)
  )
  for (f in formulas) {
    m <- sdmtmb_test_fit(f, d)
    sdmtmb_expect_native(m, d[1:12, ])
    e <- smooth_estimates(m, n = 8, n_2d = 5, dist = 0)
    expect_true(all(is.finite(e$.estimate)))
    expect_true(all(is.finite(e$.se)))
    plots <- draw(m, n = 8, n_2d = 5, wrap = FALSE, rug = FALSE, dist = 0)
    sdmtmb_expect_build(plots)
  }
  m <- sdmtmb_test_fit(y ~ fac + s(x, by = fac, k = 5), d)
  expect_identical(smooths(m), paste0("s(x):fac", letters[1:3]))
  sdmtmb_expect_build(assemble(m, grouped_by = TRUE, n = 8))
  m <- sdmtmb_test_fit(y ~ s(log(x), k = 5), d)
  raw <- smooth_estimates(m, n = 8, back_transform = TRUE)
  expect_equal(range(draw(raw, back_transform = TRUE)[[1L]]$data$x), range(d$x))
  sdmtmb_expect_build(assemble(m, back_transform = TRUE, n = 8))
})

test_that("delta plots and uncertainty follow component selection", {
  d <- sdmtmb_test_data()
  for (reml in c(FALSE, TRUE)) {
    m <- sdmtmb_test_fit(positive ~ z + s(x, k = 5), d,
      family = sdmTMB::delta_gamma(), reml = reml
    )
    a <- sdmtmb_expect_native(m, d[1:12, ])
    expect_identical(smooths(m), c("s(x)", "s.1(x)"))
    expect_identical(parametric_terms(m), c(z = "z", z.1 = "z"))
    all <- smooth_estimates(m, n = 8)
    expect_identical(unique(all$.model), paste("Delta component", 1:2))
    expect_equal(
      smooth_estimates(m, n = 8, select = 2),
      smooth_estimates(m, n = 8, select = c(FALSE, TRUE))
    )
    selected <- smooth_estimates(m, select = "s.1", partial_match = TRUE)
    expect_identical(unique(selected$.smooth), "s.1(x)")
    expect_identical(get_smooths_by_id(m, 2)[[1L]]$label, "s.1(x)")
    expect_length(smooth_coefs(m, "s.1(x)"), 4L)
    plots <- assemble(m, n = 8)
    expect_identical(names(plots), c("s(x)", "s.1(x)", "z", "z.1"))
    expect_identical(plots[[1]]$labels$subtitle, "Delta component 1")
    expect_identical(plots[[2]]$labels$subtitle, "Delta component 2")
    sdmtmb_expect_build(plots)
    expect_length(draw(m, select = 2, wrap = FALSE), 1L)
    expect_named(
      draw(m, select = 2, parametric = TRUE, terms = "z.1", wrap = FALSE),
      c("s.1(x)", "z.1")
    )
    # Inflating another component's covariance must not affect this component.
    indices <- lss_eta_index(a)[[1L]]
    a$Vp[indices, indices] <- a$Vp[indices, indices] * 100
    reference <- smooth_estimates(m, select = 2, n = 8)
    expect_equal(smooth_estimates(a, select = 2, n = 8)$.se, reference$.se)
  }
  # Different formulas are supported upstream when neither contains smooths.
  m <- sdmtmb_test_fit(list(positive ~ x, positive ~ z + fac), d,
    family = sdmTMB::delta_gamma()
  )
  expect_length(smooths(m), 0L)
  expect_identical(names(parametric_terms(m)), c("x", "z.1", "fac.1"))
  sdmtmb_expect_build(assemble(m))
})

test_that("parametric effects retain fitted transforms and contrasts", {
  d <- sdmtmb_test_data()
  contrasts(d$fac) <- contr.sum(3)
  for (reml in c(FALSE, TRUE)) {
    m <- withr::with_options(
      list(contrasts = c("contr.sum", "contr.poly")),
      sdmtmb_test_fit(y ~ z * fac + log(x) + poly(X, 2), d, reml = reml)
    )
    a <- sdmtmb_smooth_model(m)$models[[1L]]
    nd <- d[1:15, ]
    X <- model.matrix(delete.response(a$pterms), nd,
      contrasts.arg = a$contrasts)
    for (term in names(parametric_terms(m))) {
      j <- match(term, attr(a$pterms, "term.labels"))
      take <- which(attr(X, "assign") == j)
      e <- parametric_effects(m, terms = term, data = nd,
        overall_uncertainty = FALSE)
      rows <- if (term == "fac") {
        match(e$.level, as.character(nd$fac))
      } else seq_len(nrow(nd))
      expect_equal(e$.partial,
        unname(drop(X[rows, take, drop = FALSE] %*% coef(a)[take])))
      expect_equal(e$.se, unname(sqrt(rowSums((X[rows, take, drop = FALSE] %*%
        a$Vp[take, take, drop = FALSE]) * X[rows, take, drop = FALSE]))))
    }
    raw <- parametric_effects(m, terms = "log(x)", data = nd,
      back_transform = TRUE)
    expect_equal(raw$.value, nd$x)
    sdmtmb_expect_build(assemble(m, data = nd,
      terms = c("fac", "z:fac", "log(x)")))
  }
  m <- sdmtmb_test_fit(y ~ 0 + z, d)
  expect_equal(
    parametric_effects(m)$.se,
    parametric_effects(m, overall_uncertainty = FALSE)$.se
  )
  m <- sdmtmb_test_fit(y ~ 1, d)
  expect_null(assemble(m))
  expect_error(smooth_estimates(m), "No smooths")
  expect_warning(expect_null(parametric_effects(m)), "doesn't contain")
})

test_that("spatial and IID effects are excluded from formula plots", {
  d <- sdmtmb_test_data()
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 15)
  control <- sdmTMB::sdmTMBcontrol()
  backend <- Sys.getenv("GRATIA_SDMTMB_BACKEND")
  if (nzchar(backend)) control <- sdmTMB::sdmTMBcontrol(backend = backend)
  withr::local_seed(245)
  d$y <- d$y + 2 * sin(4 * d$X) + 2 * cos(4 * d$Y)
  m <- sdmTMB::sdmTMB(y ~ z + s(x, k = 5) + (1 | g), d,
    mesh = mesh,
    control = control, offset = rep(.7, nrow(d))
  )
  sdmtmb_expect_native(m, d[1:12, ], offset = rep(.7, 12))
  expect_named(assemble(m, n = 8), c("s(x)", "z"))
  sdmtmb_expect_build(assemble(m, n = 8))
  # A real spatiotemporal field exercises marginalization over all time slices.
  d$y <- d$y + .8 * d$year * sin(4 * d$X)
  m <- sdmTMB::sdmTMB(y ~ z + s(x, k = 5), d,
    mesh = mesh,
    time = "year", spatiotemporal = "iid", control = control
  )
  sdmtmb_expect_native(m, d[1:12, ])
  sdmtmb_expect_build(assemble(m, n = 8))
})

test_that("sdmTMB evaluation and plots use shared GAM controls", {
  d <- sdmtmb_test_data()
  m <- sdmtmb_test_fit(y ~ s(x, k = 5), d)
  nd <- data.frame(x = c(.2, NA, .8))
  e <- smooth_estimates(m, data = nd)
  expect_equal(nrow(e), 3L)
  expect_true(is.na(e$.estimate[2L]))
  expect_true(all(is.finite(e$.estimate[c(1, 3)])))
  plots <- assemble(m, n = 8, rug = TRUE, constant = 1, fun = exp,
    ci_level = .8)
  e <- smooth_estimates(m, n = 8)
  expect_equal(plots[[1]]$data$.estimate, exp(e$.estimate + 1))
  expect_equal(plots[[1]]$data$.lower_ci,
    exp(e$.estimate + 1 - qnorm(.9) * e$.se))
  expect_s3_class(draw(m, n = 8), "patchwork")
  expect_s3_class(assemble(m, n = 8), "assembled_plots")
  expect_s3_class(draw(m, n = 8, envir = globalenv()), "patchwork")
  expect_error(draw(m, envir = "invalid"), "environment")
  expect_error(draw(m, select = "absent"), "match")
  expect_error(draw(m, residuals = TRUE), "partial residual")
  expect_error(assemble(m, unconditional = TRUE), "joint covariance")
  expect_error(smooth_estimates(m, frequentist = TRUE), "joint covariance")
  expect_error(parametric_effects(m, frequentist = TRUE), "joint covariance")
  expect_warning(expect_null(parametric_effects(m, terms = "s(x)")),
    "parametric")
  expect_error(smooth_estimates(m, data = data.frame(z = 1:3)), "x")
})

test_that("unavailable sdmTMB reports and metadata fail explicitly", {
  d <- sdmtmb_test_data()
  m <- sdmtmb_test_fit(y ~ z + s(x, k = 5), d)
  expected <- smooth_estimates(m, n = 8)
  m$sd_report$jointPrecision <- NULL
  expect_equal(smooth_estimates(m, n = 8), expected, tolerance = 1e-6)
  m$sd_report <- NULL
  expect_equal(smooth_estimates(m, n = 8), expected, tolerance = 1e-6)
  m$tmb_obj <- NULL
  expect_error(smooth_estimates(m), "covariance is unavailable")
  expect_silent(smooth_coefs(m, "s(x)"))
  m <- sdmtmb_test_fit(y ~ s(x, k = 5), d)
  m$sd_report$pdHess <- FALSE
  expect_error(draw(m), "not positive definite")
  m$last.par.best <- NULL
  expect_error(draw(m), "Expected a fitted")
  m <- sdmtmb_test_fit(y ~ s(x, k = 5), d)
  m$smoothers$sm_dims <- 100L
  expect_error(draw(m), "smooth random-effect blocks")
  m <- sdmtmb_test_fit(y ~ s(x, k = 5), d)
  m$tmb_map$b_smooth <- factor(rep(1, length(m$parlist$b_smooth)))
  expect_error(draw(m), "mapped.*b_smooth")
  m <- sdmtmb_test_fit(y ~ s(x, k = 5), d)
  m$control$profile <- "b_j"
  expect_error(draw(m), "Profiled")
  m$control$profile <- FALSE
  m$smoothers$basis_out <- NULL
  expect_error(draw(m), "smooth metadata")
})

test_that("sdmTMB plots retain the existing smooth visual conventions", {
  d <- sdmtmb_test_data()
  m <- sdmtmb_test_fit(y ~ z + s(x, k = 5), d)
  expect_doppelganger("sdmTMB smooth and parametric effects", draw(m))
  m <- sdmtmb_test_fit(positive ~ s(x, k = 5), d,
    family = sdmTMB::delta_gamma())
  expect_doppelganger("sdmTMB delta components", draw(m))
  m <- sdmtmb_test_fit(y ~ t2(x, z, k = c(4, 4)), d)
  expect_doppelganger("sdmTMB tensor surface", draw(m, n_2d = 15))
})
