skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")

sdmtmb_threshold_test_curve <- function(x, b, type) {
  if (type == "breakpt") b[1L] * pmin(x, b[2L]) else
    b[3L] * plogis(log(19) * (x - b[1L]) / exp(b[2L]))
}

sdmtmb_threshold_fit <- local({
  fits <- new.env(parent = emptyenv())
  function(type, intercept = TRUE, delta = FALSE, mixed = FALSE) {
    key <- paste(type, intercept, delta, mixed)
    if (exists(key, fits, inherits = FALSE)) return(fits[[key]])
    withr::local_seed(if (delta && type == "breakpt") 393 else 392)
    d <- data.frame(x = runif(if (delta) 700 else 400, 0, 2))
    b <- if (type == "breakpt") c(1.8, .9) else c(.8, log(.6), 2.2)
    f <- sdmtmb_threshold_test_curve(d$x, b, type)
    d$y <- if (delta) {
      ifelse(runif(nrow(d)) < plogis(-2 + 2 * f),
        exp(.3 + f + rnorm(nrow(d), sd = .25)), 0)
    } else {
      as.numeric(intercept) * .4 + f + rnorm(nrow(d), sd = .15)
    }
    d$z <- runif(nrow(d))
    d$w <- runif(nrow(d))
    if (mixed) d$y <- d$y + .5 * d$z + sin(5 * d$w)
    start <- if (type == "breakpt") c(1.5, .8) else c(.7, log(.7), 2)
    if (delta) start <- if (type == "breakpt") {
      c(3, .8, 1.5, .8)
    } else c(.7, log(.7), 4, .7, log(.7), 2)
    formula <- as.formula(paste0("y ~ ", if (!intercept) "0 + ",
      if (mixed) "z + s(w, k = 5) + ", type, "(x)"))
    control <- sdmTMB::sdmTMBcontrol(backend = "rtmb", newton_loops = 2,
      start = list(b_threshold = matrix(start,
        nrow = if (type == "breakpt") 2L else 3L)))
    fits[[key]] <- suppressMessages(sdmTMB::sdmTMB(formula,
      data = d, spatial = "off", control = control,
      family = if (delta) sdmTMB::delta_lognormal() else gaussian()))
    fits[[key]]
  }
})

sdmtmb_threshold_build <- function(plots) {
  for (plot in plots) {
    expect_s3_class(plot, "ggplot")
    expect_silent(built <- ggplot2::ggplot_build(plot))
    expect_gt(length(built$data), 0L)
  }
}

test_that("threshold curves and intervals reproduce native predictions", {
  for (type in c("breakpt", "logistic")) {
    for (intercept in c(FALSE, TRUE)) {
      m <- sdmtmb_threshold_fit(type, intercept = intercept)
      expect_true(m$sd_report$pdHess)
      b <- m$parlist$b_threshold[, 1L]
      x <- if (type == "breakpt") b[2L] + c(-.15, 0, .15) else
        b[1L] + exp(b[2L]) * c(-3, 0, 1, 3)
      nd <- data.frame(x = x)
      e <- parametric_effects(m, data = nd)
      native <- predict(m, newdata = nd, se_fit = TRUE,
        re_form = NA, re_form_iid = NA)
      constant <- if (intercept) m$parlist$b_j[1L] else 0
      expect_equal(e$.value, x)
      expect_equal(e$.partial + constant, native$est, tolerance = 1e-7)
      expect_equal(e$.se, native$est_se, tolerance = 1e-6)
      expect_equal(e$.partial, sdmtmb_threshold_test_curve(x, b, type))
      if (!intercept) expect_equal(e$.se,
        parametric_effects(m, data = nd, overall_uncertainty = FALSE)$.se)
    }
  }
})

test_that("threshold uncertainty uses the joint covariance and fitted scale", {
  for (type in c("breakpt", "logistic")) {
    m <- sdmtmb_threshold_fit(type)
    b <- m$parlist$b_threshold[, 1L]
    x <- if (type == "breakpt") c(.2, .5, 1.2, 1.8) else
      c(-10, .2, .7, 1.2, 10)
    # Central differences independently check gradients on the fitted scale.
    eps <- 1e-6
    G <- vapply(seq_along(b), function(j) {
      plus <- minus <- b
      plus[j] <- plus[j] + eps
      minus[j] <- minus[j] - eps
      (sdmtmb_threshold_test_curve(x, plus, type) -
        sdmtmb_threshold_test_curve(x, minus, type)) / (2 * eps)
    }, numeric(length(x)))
    idx <- which(names(m$last.par.best) == "b_threshold")
    V <- m$sd_report$cov.fixed
    e <- parametric_effects(m, data = data.frame(x = x),
      overall_uncertainty = FALSE)
    expected <- sqrt(rowSums((G %*% V[idx, idx, drop = FALSE]) * G))
    expect_equal(e$.se, expected, tolerance = 1e-6)
    e_overall <- parametric_effects(m, data = data.frame(x = x))
    both <- c(which(names(m$last.par.best) == "b_j")[1L], idx)
    H <- cbind(1, G)
    expected <- sqrt(rowSums((H %*% V[both, both, drop = FALSE]) * H))
    expect_equal(e_overall$.se, expected, tolerance = 1e-6)
    expect_equal(e_overall$.partial, e$.partial)
  }
})

test_that("threshold grids retain the breakpoint and supplied locations", {
  for (type in c("breakpt", "logistic")) {
    m <- sdmtmb_threshold_fit(type)
    label <- paste0(type, "(x)")
    expect_identical(names(parametric_terms(m)), label)
    e <- parametric_effects(m, n = 12)
    expect_s3_class(e, "parametric_effects")
    expect_equal(range(e$.value), range(m$data$x))
    expect_true(all(diff(e$.value) > 0))
    expect_equal(nrow(e), if (type == "breakpt") 13L else 12L)
    if (type == "breakpt") expect_true(m$parlist$b_threshold[2L, 1L] %in% e$.value)
    nd <- data.frame(x = c(.2, NA, 1.8))
    e <- parametric_effects(m, data = nd)
    expect_equal(e$.value, nd$x)
    expect_true(is.na(e$.partial[2L]))
    expect_true(is.na(e$.se[2L]))
    expect_true(all(is.finite(e$.partial[c(1L, 3L)])))
    nested <- parametric_effects(m, n = 12, unnest = FALSE)
    expect_s3_class(nested, "parametric_effects")
    expect_equal(nested$data[[1L]]$.partial,
      parametric_effects(m, n = 12)$.partial)
    expect_error(parametric_effects(m, data = data.frame(z = 1:3)), "x")
  }
})

test_that("threshold plots use parametric selection and existing plot controls", {
  for (type in c("breakpt", "logistic")) {
    m <- sdmtmb_threshold_fit(type, mixed = TRUE)
    label <- paste0(type, "(x)")
    expect_setequal(names(parametric_terms(m)), c("z", label))
    a <- sdmtmb_smooth_model(m)$models[[1L]]
    nd_all <- m$data[1:8, ]
    sm <- a$smooth[[1L]]
    S <- mgcv::PredictMat(sm, nd_all)
    take <- smooth_coef_indices(sm)
    sm_effect <- smooth_estimates(m, data = nd_all,
      overall_uncertainty = FALSE)
    expect_equal(sm_effect$.estimate, unname(drop(S %*% coef(a)[take])))
    expect_equal(sm_effect$.se,
      unname(sqrt(rowSums((S %*% a$Vp[take, take]) * S))))
    z_effect <- parametric_effects(m, terms = "z", data = nd_all,
      overall_uncertainty = FALSE)
    expect_equal(z_effect$.partial, unname(nd_all$z * coef(a)["z"]))
    z_index <- match("z", names(coef(a)))
    expect_equal(z_effect$.se,
      unname(abs(nd_all$z) * sqrt(a$Vp[z_index, z_index])))
    # A dense inverse checks that threshold uncertainty marginalizes over
    # the smooth coefficients rather than holding them fixed.
    Q <- m$sd_report$jointPrecision
    V <- as.matrix(solve(Q))
    idx <- which(names(m$last.par.best) == "b_threshold")
    b <- m$parlist$b_threshold[, 1L]
    eps <- 1e-6
    G <- vapply(seq_along(b), function(j) {
      plus <- minus <- b
      plus[j] <- plus[j] + eps
      minus[j] <- minus[j] - eps
      (sdmtmb_threshold_test_curve(nd_all$x, plus, type) -
        sdmtmb_threshold_test_curve(nd_all$x, minus, type)) / (2 * eps)
    }, numeric(nrow(nd_all)))
    e_term <- parametric_effects(m, terms = label, data = nd_all,
      overall_uncertainty = FALSE)
    expect_equal(e_term$.se,
      sqrt(rowSums((G %*% V[idx, idx, drop = FALSE]) * G)), tolerance = 1e-6)
    expect_setequal(names(assemble(m, n = 12)), c("s(w)", "z", label))
    expect_named(assemble(m, n = 12, parametric = FALSE), "s(w)")
    expect_named(draw(m, select = "s(w)", wrap = FALSE, n = 12), "s(w)")
    expect_setequal(names(draw(m, select = "s(w)", parametric = TRUE,
      terms = label, wrap = FALSE, n = 12)), c("s(w)", label))
    nd <- data.frame(x = seq(.2, 1.8, length.out = 8))
    e <- parametric_effects(m, terms = label, data = nd)
    plots <- assemble(m, select = integer(), parametric = TRUE, terms = label,
      data = nd, ci_level = .8, constant = .5, fun = exp)
    expect_named(plots, label)
    expect_equal(plots[[1L]]$data$.partial, exp(e$.partial + .5))
    expect_equal(plots[[1L]]$data$.lower_ci,
      exp(e$.partial + .5 - qnorm(.9) * e$.se))
    expect_equal(plots[[1L]]$data$.upper_ci,
      exp(e$.partial + .5 + qnorm(.9) * e$.se))
    rug_layers <- Filter(function(layer) any(grepl("Rug", class(layer$geom))),
      plots[[1L]]$layers)
    expect_length(rug_layers, 1L)
    expect_equal(sort(unique(rug_layers[[1L]]$data$.value)),
      sort(unique(m$data$x)))
    sdmtmb_threshold_build(plots)
    expect_s3_class(draw(m, n = 12), "patchwork")
    fixed <- assemble(m, n = 12, scales = "fixed")
    sdmtmb_threshold_build(fixed)
    expect_equal(parametric_effects(m, terms = label, n = 12),
      parametric_effects(m, terms = label, n = 12, back_transform = TRUE),
      ignore_attr = TRUE)
    simultaneous <- assemble(m, select = integer(), parametric = TRUE,
      terms = label, n = 12, interval = "simultaneous")
    expect_equal(simultaneous[[1L]]$data$.lower_ci,
      parametric_effects(m, terms = label, n = 12)$.partial -
        qnorm(.975) * parametric_effects(m, terms = label, n = 12)$.se)
  }
})

test_that("delta threshold plots retain separate components and uncertainty", {
  for (type in c("breakpt", "logistic")) {
    m <- sdmtmb_threshold_fit(type, delta = TRUE)
    expect_true(m$sd_report$pdHess)
    labels <- paste0(type, "(x)", c("", ".1"))
    expect_identical(names(parametric_terms(m)), labels)
    expect_setequal(names(assemble(m, n = 12)), labels)
    expect_named(draw(m, terms = labels[2L], wrap = FALSE, n = 12), labels[2L])
    nd <- data.frame(x = c(.2, .5, 1.1, 1.8))
    for (i in 1:2) {
      e <- parametric_effects(m, terms = labels[i], data = nd)
      native <- predict(m, newdata = nd, se_fit = TRUE, model = i,
        re_form = NA, re_form_iid = NA)
      constant <- m$parlist[[if (i == 1L) "b_j" else "b_j2"]][1L]
      expect_equal(e$.partial + constant, native$est, tolerance = 1e-6)
      expect_equal(e$.se, native$est_se, tolerance = 1e-6)
      expect_equal(e$.partial,
        sdmtmb_threshold_test_curve(nd$x, m$parlist$b_threshold[, i], type))
    }
    sdmtmb_threshold_build(assemble(m, n = 12))
  }
})

test_that("threshold extraction preserves covariance and mapping errors", {
  m <- sdmtmb_threshold_fit("logistic")
  bad <- m
  bad$tmb_map$b_threshold <- factor(rep(1L, length(bad$parlist$b_threshold)))
  expect_error(draw(bad), "mapped.*b_threshold")
  bad <- m
  bad$sd_report$pdHess <- FALSE
  expect_error(assemble(bad), "not positive definite")
  bad <- m
  bad$sd_report <- bad$tmb_obj <- NULL
  expect_error(parametric_effects(bad), "covariance is unavailable")
  bad <- m
  bad$control$profile <- "b_threshold"
  expect_error(draw(bad), "Profiled")
})

test_that("threshold curves reject invalid grids and evaluation columns", {
  m <- sdmtmb_threshold_fit("logistic")
  for (n in list(1, 1.5, NA_real_, Inf, numeric(), c(5, 6), "5",
    .Machine$integer.max + 1)) {
    expect_error(parametric_effects(m, n = n), "'n'.*integer")
  }
  for (values in list("a", factor("a"), Inf, -Inf, matrix(1:3))) {
    d <- data.frame(x = I(values))
    expect_error(parametric_effects(m, data = d), "finite numeric")
  }
  expect_error(parametric_effects(m, data = list(x = 1)), "must contain 'x'")
  expect_equal(parametric_effects(m, data = data.frame(x = .5), n = 0)$.value, .5)
})

test_that("logistic curves remain finite in the tails and allow negative heights", {
  x <- c(-1e6, -2, 0, .5, 1, 2, 1e6)
  b <- c(.5, log(.6), -2)
  curve <- sdmtmb_threshold_curve(x, b, "logistic")
  expect_true(all(is.finite(curve$estimate)))
  expect_true(all(is.finite(curve$gradient)))
  expect_equal(curve$estimate, sdmtmb_threshold_test_curve(x, b, "logistic"))
  expect_true(all(diff(curve$estimate) <= 0))
  expect_equal(curve$estimate[c(1L, 7L)], c(0, -2))
  expect_equal(unname(curve$gradient[c(1L, 7L), 1:2]), matrix(0, 2L, 2L))
  eps <- 1e-6
  numeric_gradient <- vapply(seq_along(b), function(j) {
    plus <- minus <- b
    plus[j] <- plus[j] + eps
    minus[j] <- minus[j] - eps
    (sdmtmb_threshold_test_curve(x, plus, "logistic") -
      sdmtmb_threshold_test_curve(x, minus, "logistic")) / (2 * eps)
  }, numeric(length(x)))
  expect_equal(unname(curve$gradient), numeric_gradient, tolerance = 1e-6)
  for (log_width in c(-1000, 1000)) {
    extreme <- sdmtmb_threshold_curve(c(-1, 0, 1), c(0, log_width, -2), "logistic")
    expect_true(all(is.finite(extreme$estimate)))
    # At the midpoint an arbitrarily narrow curve has an unbounded derivative.
    expect_true(all(is.finite(extreme$gradient[c(1L, 3L), ])))
  }
})

test_that("nested threshold and ordinary effects retain rendering metadata", {
  m <- sdmtmb_threshold_fit("logistic", mixed = TRUE)
  nested <- parametric_effects(m, n = 12, unnest = FALSE)
  expect_setequal(nested$.term, c("z", "logistic(x)"))
  expect_setequal(names(attr(nested, "axis_info")), c("z", "logistic(x)"))
  expect_setequal(names(attr(nested, "factor_levels")), c("z", "logistic(x)"))
  expect_silent(grob <- patchwork::patchworkGrob(draw(nested)))
  expect_s3_class(grob, "gtable")
})
