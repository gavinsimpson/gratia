# gam.vcomp is a reference for estimates and intervals it already provides.
# Expanded rows and fixed-only fits also have independent numerical checks.
variance_component_data <- function() {
  set.seed(42)
  d <- data.frame(x = runif(240), z = runif(240),
    group = factor(rep(seq_len(12), each = 20)))
  d$y <- sin(6 * d$x) + d$z^2 +
    rnorm(12, sd = 0.8)[d$group] + rnorm(240, sd = 0.3)
  d
}

expect_variance_component_reference <- function(model, rescale = TRUE,
    compare_intervals = TRUE, compare_expanded = TRUE) {
  got <- variance_comp(model, rescale = rescale, coverage = 0.9)
  invisible(capture.output(ref <- mgcv::gam.vcomp(model,
    rescale = rescale, conf.lev = 0.9)))
  expect_s3_class(got, "variance_comp")
  expect_equal(got$.variance, got$.std_dev^2)
  if (is.list(ref)) {
    if (!is.null(ref$all) && compare_expanded) {
      expect_equal(head(got$.std_dev, length(ref$all)), unname(ref$all))
    }
    ref <- ref$vc
  }
  if (!is.null(ref) && compare_intervals) {
    if (is.matrix(ref)) {
      rows <- match(rownames(ref), got$.component)
      expect_false(anyNA(rows))
      expect_equal(got$.std_dev[rows], unname(ref[, "std.dev"]))
      expect_equal(got$.lower_ci[rows], unname(ref[, "lower"]))
      expect_equal(got$.upper_ci[rows], unname(ref[, "upper"]))
    } else {
      rows <- match(names(ref), got$.component)
      expect_false(anyNA(rows))
      expect_equal(got$.std_dev[rows], as.numeric(ref))
      expect_true(all(is.na(got$.lower_ci)))
      expect_true(all(is.na(got$.upper_ci)))
    }
  }
  invisible(got)
}

test_that("variance_comp agrees with gam.vcomp for ordinary GAM fits", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  models <- list(
    gcv = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d),
    reml = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d, method = "REML"),
    ml = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d, method = "ML"),
    known_scale = gam(y ~ s(x, k = 8), data = d, method = "REML", scale = 1),
    tensor = gam(y ~ te(x, z, k = c(4, 4)), data = d, method = "REML"),
    selection = gam(y ~ s(x, k = 8) + s(z, k = 8), data = d,
      method = "REML", select = TRUE),
    unpenalized = gam(y ~ s(x, k = 5, fx = TRUE) + s(z, k = 8),
      data = d, method = "REML")
  )
  for (m in models) {
    for (rescale in c(FALSE, TRUE)) {
      expect_variance_component_reference(m, rescale)
    }
  }
})

test_that("shared smoothing parameters are returned for every penalty", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  for (method in c("GCV.Cp", "REML")) {
    m <- gam(y ~ s(x, k = 8, id = 1) + s(z, k = 8, id = 1),
      data = d, method = method)
    for (rescale in c(FALSE, TRUE)) {
      got <- expect_variance_component_reference(m, rescale)
      expect_equal(head(got$.component, 2), c("s(x)", "s(z)"))
      expect_equal(got$.variance[1], got$.variance[2])
      expect_equal(got$.lower_ci[1], got$.lower_ci[2])
      expect_equal(got$.upper_ci[1], got$.upper_ci[2])
    }
  }
})

test_that("fixed smoothing parameters retain scale uncertainty", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  for (method in c("GCV.Cp", "REML")) {
    for (sp in list(c(1, 2), c(1, -1))) {
      m <- gam(y ~ s(x, k = 8) + s(z, k = 8), data = d,
        method = method, sp = sp)
      for (rescale in c(FALSE, TRUE)) {
        got <- expect_variance_component_reference(m, rescale)
        phi <- m$reml.scale %||% m$sig2
        scaling <- if (rescale) vapply(m$smooth,
          function(sm) sm$S.scale, numeric(1)) else c(1, 1)
        expect_equal(got$.variance[1:2], unname(phi * scaling / m$full.sp))
        expect_equal(head(got$.component, 2), c("s(x)", "s(z)"))
        if (method == "REML") {
          expect_true(all(is.finite(got$.lower_ci)))
          expect_true(all(got$.lower_ci < got$.std_dev))
          expect_true(all(got$.upper_ci > got$.std_dev))
          if (all(sp > 0)) {
            width <- qnorm(0.95) * sqrt(1 / m$outer.info$hess[1, 1]) / 2
            expect_equal(log(got$.upper_ci / got$.std_dev), rep(width, 3))
          }
        }
      }
    }
  }
  m <- gam(y ~ s(x, k = 8) + s(z, k = 8), data = d,
    method = "REML", sp = c(1, 2), scale = 1)
  got <- variance_comp(m, rescale = FALSE)
  expect_equal(got$.variance, c(1, 0.5))
  expect_true(all(is.na(got$.lower_ci)))
})

test_that("parametric penalties include estimated, fixed and mapped parameters", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  for (method in c("GCV.Cp", "REML")) {
    for (fixed in c(FALSE, TRUE)) {
      pp <- list(group = list(diag(11), sp = if (fixed) 2 else -1))
      for (smooth in c(FALSE, TRUE)) {
        f <- if (smooth) y ~ group + s(x, k = 8) else y ~ group
        m <- gam(f, data = d, method = method, paraPen = pp)
        for (rescale in c(FALSE, TRUE)) {
          got <- expect_variance_component_reference(m, rescale)
          phi <- m$reml.scale %||% m$sig2
          expect_equal(got$.component[1], "group")
          expect_equal(got$.variance[1],
            unname(phi / (m$full.sp %||% m$sp)[1]))
        }
      }
    }
  }
  # One estimated parameter controls two penalties through different powers.
  m <- gam(y ~ group, data = d, method = "REML",
    paraPen = list(group = list(diag(11), diag(seq_len(11)),
      L = matrix(c(1, 2), 2, 1))))
  got <- expect_variance_component_reference(m, compare_intervals = FALSE)
  expect_equal(got$.component, c("group1", "group2", "scale"))
  phi <- m$reml.scale
  expect_equal(got$.variance, c(unname(phi / m$sp),
    unname(phi / m$sp^2), phi))
  J <- rbind(c(-0.5, 0.5), c(-1, 0.5), c(0, 0.5))
  se <- sqrt(diag(J %*% solve(m$outer.info$hess) %*% t(J)))
  expect_equal(log(got$.upper_ci / got$.std_dev), qnorm(0.95) * se)
})

test_that("bam uses complete penalty values and mappings", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  for (discrete in c(FALSE, TRUE)) {
    models <- list(
      ordinary = bam(y ~ s(x, k = 8) + s(z, k = 8), data = d,
        method = "fREML", discrete = discrete),
      shared = bam(y ~ s(x, k = 8, id = 1) + s(z, k = 8, id = 1),
        data = d, method = "fREML", discrete = discrete),
      mixed = bam(y ~ s(x, k = 8) + s(z, k = 8), sp = c(1, -1),
        data = d, method = "fREML", discrete = discrete),
      fixed = bam(y ~ s(x, k = 8) + s(z, k = 8), sp = c(1, 2),
        data = d, method = "fREML", discrete = discrete),
      parametric = bam(y ~ group + s(x, k = 8), data = d,
        method = "fREML", discrete = discrete,
        paraPen = list(group = list(diag(11))))
    )
    for (nm in names(models)) {
      m <- models[[nm]]
      for (rescale in c(FALSE, TRUE)) {
        # bam does not retain paraPen in the same slot as gam. Supply it to
        # the reference so gam.vcomp can identify which penalties to rescale.
        ref_model <- m
        if ("group" %in% names(m$sp)) {
          ref_model$paraPen <- list(sp = -1, full.sp.names = "group")
        }
        got <- variance_comp(m, rescale = rescale, coverage = 0.9)
        # Discrete bam can retain different scaling factors for shared
        # smooths. gam.vcomp instead repeats the first smooth's factor.
        ref <- expect_variance_component_reference(ref_model, rescale,
          compare_expanded = !(nm == "shared" && discrete && rescale))
        expect_equal(got, ref)
        expect_true(all(c("s(x)", "s(z)") %in% got$.component) ||
          "group" %in% got$.component)
        if (nm == "shared") {
          unscaled <- variance_comp(m, rescale = FALSE, coverage = 0.9)
          scaling <- if (rescale) vapply(m$smooth,
            function(sm) sm$S.scale, numeric(1)) else c(1, 1)
          expect_equal(got$.variance[1:2],
            unscaled$.variance[1:2] * scaling)
          expect_equal(got$.lower_ci[1:2],
            unscaled$.lower_ci[1:2] * sqrt(scaling))
          expect_equal(got$.upper_ci[1:2],
            unscaled$.upper_ci[1:2] * sqrt(scaling))
        }
      }
    }
  }
})

test_that("family parameters are excluded from the interval transformation", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  d$count <- rnbinom(nrow(d), mu = exp(sin(6 * d$x)), size = 2)
  for (shared in c(FALSE, TRUE)) {
    f <- if (shared) y ~ s(x, k = 8, id = 1) + s(z, k = 8, id = 1) else
      y ~ s(x, k = 8) + s(z, k = 8)
    f[[2]] <- quote(count)
    m <- gam(f, data = d, family = nb(), method = "REML")
    expect_variance_component_reference(m)
  }
})

test_that("fixed shared parameters are expanded for both GAM fitters", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  f <- y ~ s(x, k = 8, id = 1, sp = 2) + s(z, k = 8, id = 1) +
    s(group, bs = "re")
  for (m in list(gam(f, data = d, method = "REML"),
      bam(f, data = d, method = "fREML"),
      bam(f, data = d, method = "fREML", discrete = TRUE))) {
    got <- variance_comp(m, rescale = FALSE)
    expect_equal(got$.component, c("s(x)", "s(z)", "s(group)", "scale"))
    expect_equal(got$.variance[1:2], rep(tail(got$.variance, 1) / 2, 2))
    expect_equal(got$.lower_ci[1], got$.lower_ci[2])
    expect_equal(got$.upper_ci[1], got$.upper_ci[2])
    expect_true(all(is.finite(got$.lower_ci)))
  }
})

test_that("custom parametric mappings also work with bam and fixed parameters", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  for (fit in list(gam, bam)) {
    for (fixed in c(FALSE, TRUE)) {
      m <- fit(y ~ group, data = d, method = "REML",
        paraPen = list(group = list(diag(11), diag(seq_len(11)),
          L = matrix(c(1, 2), 2, 1), sp = if (fixed) 2 else -1)))
      got <- variance_comp(m, coverage = 0.9)
      phi <- m$reml.scale %||% m$sig2
      sp <- if (fixed) 2 else unname(m$sp)
      expect_equal(got$.variance, c(phi / sp, phi / sp^2, phi))
      J <- if (fixed) matrix(0.5, 3, 1) else
        rbind(c(-0.5, 0.5), c(-1, 0.5), c(0, 0.5))
      se <- sqrt(diag(J %*% solve(m$outer.info$hess) %*% t(J)))
      expect_equal(log(got$.upper_ci / got$.std_dev), qnorm(0.95) * se)
    }
  }
})

test_that("unpenalized models return an empty variance component table", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  for (m in list(m_only_para,
      gam(y ~ s(x, k = 5, fx = TRUE), data = d, method = "REML"))) {
    got <- variance_comp(m)
    expect_equal(nrow(got), 0L)
    expect_named(got, c(".component", ".variance", ".std_dev",
      ".lower_ci", ".upper_ci"))
  }
})

test_that("variance_comp validates coverage and rescaling", {
  withr::local_preserve_seed()
  d <- variance_component_data()
  m <- m_1_smooth
  for (coverage in list(0, 1, -1, NA_real_, Inf, numeric(), c(0.9, 0.95), "a")) {
    expect_error(variance_comp(m, coverage = coverage), "between zero and one")
  }
  for (rescale in list(NA, NULL, c(TRUE, FALSE), 1)) {
    expect_error(variance_comp(m, rescale = rescale), "TRUE or FALSE")
  }
})
