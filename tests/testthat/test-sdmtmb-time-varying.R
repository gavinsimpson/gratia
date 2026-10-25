# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")

sdmtmb_tvc_fit <- local({
  fits <- new.env(parent = emptyenv())
  function(type = "ar1", design = "slope", extra = FALSE,
    delta = FALSE, baseline = TRUE) {
    key <- paste(type, design, extra, delta, baseline)
    if (exists(key, fits, inherits = FALSE)) return(fits[[key]])
    withr::local_seed(614)
    nt <- 16L
    n <- if (delta) 65L else 45L
    year <- 2001L + seq_len(nt)
    d <- data.frame(year = rep(year, each = n), x = rnorm(nt * n),
      z = rnorm(nt * n), w = runif(nt * n), u = runif(nt * n))
    d$g <- factor(rep(letters[1:3], length.out = nrow(d)))
    process <- function(sd) as.numeric(stats::arima.sim(
      list(ar = .65), n = nt, sd = sd))
    bx <- process(.22)
    bi <- process(.18)
    bz <- process(.16)
    row <- rep(seq_len(nt), each = n)
    base <- baseline && type != "rw"
    tv <- ~ 0 + x
    formula <- if (base) y ~ x else y ~ 1
    eta <- .6 + (.8 * baseline + bx[row]) * d$x
    if (design == "intercept") {
      tv <- ~ 1
      formula <- if (base) y ~ 1 else y ~ 0
      eta <- .6 * baseline + bi[row]
    } else if (design == "multi") {
      tv <- ~ x + z
      formula <- if (base) y ~ x + z else y ~ 0
      eta <- .6 + bi[row] + (.8 + bx[row]) * d$x +
        (-.3 + bz[row]) * d$z
    } else if (design == "transformed") {
      d$x <- exp(d$x)
      tv <- ~ 0 + log(x)
      formula <- if (base) y ~ log(x) else y ~ 1
      eta <- .6 + (.8 + bx[row]) * log(d$x)
    } else if (design %in% c("factor", "interaction")) {
      B <- cbind(bi, bx, bz)
      group_coefficient <- c(.4, .8, 1.2)[d$g] +
        B[cbind(row, as.integer(d$g))]
      if (design == "factor") {
        tv <- ~ 0 + g
        formula <- if (base) y ~ 0 + g else y ~ 0
        eta <- group_coefficient
      } else {
        tv <- ~ 0 + x:g
        formula <- if (base) y ~ x:g else y ~ 1
        eta <- .6 + group_coefficient * d$x
      }
    } else if (design == "mixed") {
      formula <- if (base) y ~ x + z + s(w, k = 5) + breakpt(u) else
        y ~ z + s(w, k = 5) + breakpt(u)
      eta <- eta + .3 * d$z + sin(4 * d$w) + 1.8 * pmin(d$u, .7)
    }
    d$y <- eta + rnorm(nrow(d), sd = .5)
    if (delta) d$y <- ifelse(
      runif(nrow(d)) < plogis(-.4 + (.8 + bx[row]) * d$x),
      exp(.3 + (.6 + bz[row]) * d$x + rnorm(nrow(d), sd = .35)), 0)
    control <- sdmTMB::sdmTMBcontrol(backend = "rtmb", newton_loops = 2,
      start = if (design == "mixed") {
        list(b_threshold = matrix(c(1.5, .65), ncol = 1L))
      })
    fits[[key]] <- suppressMessages(sdmTMB::sdmTMB(formula,
      data = d, time = "year", time_varying = tv, time_varying_type = type,
      spatial = "off", spatiotemporal = "off", control = control,
      extra_time = if (extra) max(year) + 1:2,
      family = if (delta) sdmTMB::delta_lognormal() else gaussian()))
    fits[[key]]
  }
})

# Construct the coefficient's linear combination independently of the adapter.
sdmtmb_tvc_operator <- function(m, column, component = 1L) {
  nt <- dim(m$parlist$b_rw_t)[1L]
  k <- match(column, colnames(m$tmb_data$X_rw_ik))
  latent <- array(which(names(m$last.par.best) == "b_rw_t"),
    dim = dim(m$parlist$b_rw_t))
  G <- matrix(0, nt, length(m$last.par.best))
  G[cbind(seq_len(nt), latent[, k, component])] <- 1
  if (m$tmb_data$random_walk != 1L) {
    beta <- match(column, colnames(m$tmb_data$X_ij[[component]]))
    if (!is.na(beta)) {
      tag <- if (component == 1L) "b_j" else "b_j2"
      fixed <- which(names(m$last.par.best) == tag)
      G[, fixed[beta]] <- 1
    }
  }
  G
}

sdmtmb_tvc_label <- function(column, component = 1L) {
  column <- if (column == "(Intercept)") "Intercept" else
    if (column == "Intercept") "`Intercept`" else column
  paste0("time_varying(", column, ")",
    if (component > 1L) paste0(".", component - 1L))
}

sdmtmb_tvc_expect_covariance <- function(m, column, component = 1L) {
  G <- sdmtmb_tvc_operator(m, column, component)
  V <- as.matrix(solve(m$sd_report$jointPrecision))
  e <- parametric_effects(m, terms = sdmtmb_tvc_label(column, component))
  expect_equal(e$.partial, unname(drop(G %*% m$last.par.best)),
    tolerance = 1e-7)
  expect_equal(e$.se, unname(sqrt(rowSums((G %*% V) * G))),
    tolerance = 1e-6)
  expect_identical(unique(e$.type), "time_varying")
  expect_equal(e$.value, m$time_lu$time_from_data)
  e
}

test_that("full temporal intercepts and slopes reproduce native predictors", {
  for (type in c("rw", "rw0", "ar1")) {
    for (design in c("intercept", "slope")) {
      m <- sdmtmb_tvc_fit(type, design)
      expect_true(m$sd_report$pdHess)
      column <- if (design == "intercept") "(Intercept)" else "x"
      e <- sdmtmb_tvc_expect_covariance(m, column)
      nd <- data.frame(year = e$.value, x = 1)
      native <- predict(m, newdata = nd, se_fit = TRUE,
        re_form = NA, re_form_iid = NA)
      if (design == "intercept") {
        expect_equal(e$.partial, native$est, tolerance = 1e-7)
        expect_equal(e$.se, native$est_se, tolerance = 1e-6)
      } else {
        nd$x <- 0
        zero <- predict(m, newdata = nd, re_form = NA, re_form_iid = NA)
        expect_equal(e$.partial, native$est - zero$est, tolerance = 1e-7)
      }
      without_mean <- parametric_effects(m,
        terms = sdmtmb_tvc_label(column), overall_uncertainty = FALSE)
      expect_equal(without_mean$.partial, e$.partial)
      expect_equal(without_mean$.se, e$.se)
    }
  }
})

test_that("temporal intervals include covariance with the ordinary coefficient", {
  for (type in c("rw0", "ar1")) {
    m <- sdmtmb_tvc_fit(type)
    e <- sdmtmb_tvc_expect_covariance(m, "x")
    V <- as.matrix(solve(m$sd_report$jointPrecision))
    k <- which(names(m$last.par.best) == "b_rw_t")
    beta <- which(names(m$last.par.best) == "b_j")[[2L]]
    naive <- V[beta, beta] + diag(V)[k]
    expect_gt(max(abs(e$.se^2 - naive)), 1e-6)
  }
  m <- sdmtmb_tvc_fit("ar1", baseline = FALSE)
  expect_true(m$sd_report$pdHess)
  sdmtmb_tvc_expect_covariance(m, "x")
})

test_that("multiple and expanded temporal design columns retain their coding", {
  for (design in c("multi", "transformed", "factor", "interaction")) {
    m <- sdmtmb_tvc_fit(design = design)
    expect_true(m$sd_report$pdHess)
    columns <- colnames(m$tmb_data$X_rw_ik)
    expected <- vapply(columns, sdmtmb_tvc_label, character(1))
    expect_true(all(expected %in% names(parametric_terms(m))))
    expect_setequal(unique(parametric_effects(m)$.term), expected)
    for (column in columns) sdmtmb_tvc_expect_covariance(m, column)
    if (design == "factor") {
      for (column in columns) {
        e <- parametric_effects(m, terms = sdmtmb_tvc_label(column))
        nd <- data.frame(year = e$.value,
          g = factor(sub("^g", "", column), levels = levels(m$data$g)))
        native <- predict(m, newdata = nd, se_fit = TRUE,
          re_form = NA, re_form_iid = NA)
        expect_equal(e$.partial, native$est, tolerance = 1e-7)
        expect_equal(e$.se, native$est_se, tolerance = 1e-6)
      }
    }
  }
})

test_that("temporal selectors suppress consumed ordinary effects by default", {
  m <- sdmtmb_tvc_fit()
  expect_identical(names(parametric_terms(m)), c("x", "time_varying(x)"))
  expect_identical(unique(parametric_effects(m)$.term), "time_varying(x)")
  expect_named(assemble(m), "time_varying(x)")
  ordinary <- parametric_effects(m, terms = "x", overall_uncertainty = FALSE)
  expect_identical(unique(ordinary$.type), "numeric")
  expect_equal(ordinary$.partial, ordinary$.value * m$parlist$b_j[2L])
  expect_named(draw(m, terms = "x", wrap = FALSE), "x")
  expect_setequal(names(assemble(m, terms = c("x", "time_varying(x)"))),
    c("x", "time_varying(x)"))
  expect_null(assemble(m, parametric = FALSE))
  expect_error(parametric_effects(m, terms = "time_varying(z)"), "parametric")
})

test_that("temporal evaluation selects only stored modelled time steps", {
  m <- sdmtmb_tvc_fit(extra = TRUE)
  expect_true(m$sd_report$pdHess)
  e <- sdmtmb_tvc_expect_covariance(m, "x")
  expect_equal(tail(e$.value, 2L), max(m$data$year) + 1:2)
  expect_equal(parametric_effects(m, n = 3), parametric_effects(m, n = 1000))
  want <- c(max(e$.value), min(e$.value), min(e$.value))
  selected <- parametric_effects(m, data = data.frame(year = want))
  expect_equal(selected$.value, sort(unique(want)))
  expect_equal(selected$.partial, e$.partial[e$.value %in% want])
  expect_equal(parametric_effects(m, data = data.frame(x = c(-1, 1))), e)
  expect_error(parametric_effects(m, data = data.frame(year = NA_real_)),
    "time|year")
  expect_error(parametric_effects(m, data = data.frame(year = 9999)), "time|year")
})

test_that("delta trajectories retain complete component-specific coefficients", {
  for (type in c("rw", "rw0", "ar1")) {
    m <- sdmtmb_tvc_fit(type, delta = TRUE)
    expect_true(m$sd_report$pdHess)
    labels <- c("time_varying(x)", "time_varying(x).1")
    expect_setequal(unique(parametric_effects(m)$.term), labels)
    expect_setequal(names(assemble(m)), labels)
    for (unnest in c(TRUE, FALSE)) {
      e <- parametric_effects(m, unnest = unnest)
      plots <- draw(e)
      for (component in 1:2) {
        expect_identical(unique(e$.model[e$.term == labels[component]]),
          paste("Delta component", component))
        expect_identical(plots[[component]]$labels$subtitle,
          paste("Delta component", component))
      }
      expect_identical(draw(e, subtitle = "")[[2]]$labels$subtitle, "")
    }
    for (component in 1:2) {
      e <- sdmtmb_tvc_expect_covariance(m, "x", component)
      nd <- data.frame(year = e$.value, x = 1)
      one <- predict(m, newdata = nd, model = component,
        re_form = NA, re_form_iid = NA)
      nd$x <- 0
      zero <- predict(m, newdata = nd, model = component,
        re_form = NA, re_form_iid = NA)
      expect_equal(e$.partial, one$est - zero$est, tolerance = 1e-7)
    }
    expect_named(draw(m, terms = labels[2L], wrap = FALSE), labels[2L])
  }
})

test_that("temporal panels coexist with smooth and threshold effects", {
  m <- sdmtmb_tvc_fit(design = "mixed")
  expect_true(m$sd_report$pdHess)
  expected <- c("s(w)", "z", "breakpt(u)", "time_varying(x)")
  expect_setequal(names(assemble(m, n = 12)), expected)
  expect_named(assemble(m, parametric = FALSE, n = 12), "s(w)")
  expect_named(draw(m, select = "s(w)", wrap = FALSE, n = 12), "s(w)")
  expect_setequal(names(draw(m, select = "s(w)", parametric = TRUE,
    terms = "time_varying(x)", wrap = FALSE, n = 12)),
    c("s(w)", "time_varying(x)"))
  sdmtmb_tvc_expect_covariance(m, "x")
  a <- sdmtmb_smooth_model(m)$models[[1L]]
  nd <- m$data[1:8, ]
  sm <- a$smooth[[1L]]
  X <- mgcv::PredictMat(sm, nd)
  take <- smooth_coef_indices(sm)
  e <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE)
  expect_equal(e$.estimate, unname(drop(X %*% coef(a)[take])))
  expect_equal(e$.se, unname(sqrt(rowSums((X %*% a$Vp[take, take]) * X))))
  e <- parametric_effects(m, terms = "z", data = nd,
    overall_uncertainty = FALSE)
  j <- match("z", names(coef(a)))
  expect_equal(e$.partial, unname(nd$z * coef(a)[j]))
  expect_equal(e$.se, unname(abs(nd$z) * sqrt(a$Vp[j, j])))
  b <- m$parlist$b_threshold[, 1L]
  e <- parametric_effects(m, terms = "breakpt(u)", data = nd)
  expect_equal(e$.partial, b[1L] * pmin(nd$u, b[2L]))
})

test_that("both temporal plot styles retain estimates, intervals and metadata", {
  m <- sdmtmb_tvc_fit()
  e <- parametric_effects(m)
  for (style in c("ribbon", "pointrange")) {
    plots <- assemble(m, time_varying_style = style, ci_level = .8,
      constant = .2, fun = exp)
    plot <- plots[[1L]]
    expect_identical(plot$labels$ylab %||% plot$labels$y, "Coefficient")
    expect_equal(plot$data$.partial, exp(e$.partial + .2))
    expect_equal(plot$data$.lower_ci,
      exp(e$.partial + .2 - qnorm(.9) * e$.se))
    expect_equal(plot$data$.upper_ci,
      exp(e$.partial + .2 + qnorm(.9) * e$.se))
    expect_silent(built <- ggplot2::ggplot_build(plot))
    expect_gt(length(built$data), 0L)
    nested <- parametric_effects(m, unnest = FALSE)
    expect_silent(grob <- patchwork::patchworkGrob(
      draw(nested, time_varying_style = style)))
    expect_s3_class(grob, "gtable")
  }
  expect_equal(assemble(m, interval = "simultaneous")[[1L]]$data$.lower_ci,
    e$.partial - qnorm(.975) * e$.se)
  expect_error(assemble(m, time_varying_style = "invalid"), "arg|style")
  fixed <- assemble(sdmtmb_tvc_fit(design = "multi"), scales = "fixed")
  for (plot in fixed) expect_silent(ggplot2::ggplot_build(plot))
})

test_that("temporal extraction checks fitted coding and parameter alignment", {
  m <- sdmtmb_tvc_fit()
  bad <- m
  bad$tmb_map$b_rw_t <- array(factor(rep(1L, length(m$parlist$b_rw_t))),
    dim = dim(m$parlist$b_rw_t))
  expect_error(parametric_effects(bad), "mapped.*b_rw_t")
  bad <- m
  bad$tmb_data$X_rw_ik[, "x"] <- 2 * bad$tmb_data$X_rw_ik[, "x"]
  expect_error(parametric_effects(bad), "coding|design|align")
  bad <- m
  bad$time_lu <- NULL
  expect_error(parametric_effects(bad), "time|align")
  bad <- m
  bad$parlist$b_rw_t <- bad$parlist$b_rw_t[-1L, , , drop = FALSE]
  expect_error(parametric_effects(bad), "align|dimension|time")
  bad <- m
  bad$sd_report$pdHess <- FALSE
  expect_error(assemble(bad), "not positive definite")
})

test_that("a covariate named Intercept has a distinct temporal selector", {
  withr::local_seed(521)
  nt <- 20L
  n <- 40L
  d <- data.frame(year = rep(2000L + seq_len(nt), each = n),
    Intercept = rnorm(nt * n))
  intercept <- .7 + as.numeric(stats::arima.sim(list(ar = .65),
    n = nt, sd = .22))
  slope <- -.4 + as.numeric(stats::arima.sim(list(ar = .7),
    n = nt, sd = .27))
  d$y <- rep(intercept, each = n) + rep(slope, each = n) * d$Intercept +
    rnorm(nrow(d), sd = .5)
  m <- sdmTMB::sdmTMB(y ~ Intercept,
    time = "year", time_varying = ~ Intercept, time_varying_type = "ar1",
    data = d, spatial = "off", spatiotemporal = "off",
    control = sdmTMB::sdmTMBcontrol(backend = "rtmb", newton_loops = 2))
  expect_true(m$sd_report$pdHess)
  labels <- c("time_varying(Intercept)", "time_varying(`Intercept`)")
  expect_identical(names(parametric_terms(m)), c("Intercept", labels))
  e_intercept <- sdmtmb_tvc_expect_covariance(m, "(Intercept)")
  e_slope <- sdmtmb_tvc_expect_covariance(m, "Intercept")
  expect_gt(max(abs(e_intercept$.partial - e_slope$.partial)), .1)
  expect_setequal(unique(parametric_effects(m)$.term), labels)
  plots <- assemble(m)
  expect_length(plots, 2L)
  expect_setequal(names(plots), labels)
  for (label in labels) {
    expect_named(draw(m, terms = label, wrap = FALSE), label)
    expect_silent(ggplot2::ggplot_build(plots[[label]]))
  }
  nd <- data.frame(year = e_intercept$.value, Intercept = 0)
  zero <- predict(m, newdata = nd, se_fit = TRUE,
    re_form = NA, re_form_iid = NA)
  nd$Intercept <- 1
  one <- predict(m, newdata = nd, re_form = NA, re_form_iid = NA)
  expect_equal(e_intercept$.partial, zero$est, tolerance = 1e-7)
  expect_equal(e_intercept$.se, zero$est_se, tolerance = 1e-6)
  expect_equal(e_slope$.partial, one$est - zero$est, tolerance = 1e-7)
})
