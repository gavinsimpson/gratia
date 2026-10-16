skip_if_not_installed("scam", minimum_version = "1.2-22")

# Fit every constructor, including explicit numeric-by bases and the bases
# with fixed-zero coefficients. Small fixed penalties make these checks cheap.
scam_constructor_models <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    s <- mgcv::s
    d <- withr::with_seed(1047, {
      d <- data.frame(x = runif(160, 0.05, 0.95),
        z = runif(160, 0.05, 0.95), w = runif(160, 0.5, 1.5))
      d$error <- rnorm(nrow(d), sd = 0.04)
      d
    })
    constructors <- ls(asNamespace("scam"), pattern =
      "^smooth.construct.*smooth.spec$")
    bases <- sub(".smooth.spec$", "", sub("^smooth.construct.", "", constructors))
    models <- lapply(bases, function(bs) {
      dd <- d
      decreasing <- grepl("^(mpd|mdc|dpo|tedm|tedec|tesmd|tismd)", bs)
      concave <- grepl("^(cv|micv|mdcv|tecvcv|temicv|tedecv)", bs)
      fx <- if (concave) sqrt(dd$x) else exp(dd$x)
      if (decreasing) fx <- -fx
      dd$y <- 2 + fx + 0.3 * exp(dd$z) + dd$error
      bivar <- startsWith(bs, "te") || startsWith(bs, "ti")
      form <- if (bivar) {
        y ~ s(x, z, bs = bs, k = c(5, 5))
      } else if (endsWith(bs, "By")) {
        y ~ s(x, by = w, bs = bs, k = 7)
      } else if (bs %in% c("lmpi", "lipl")) {
        y ~ s(x, bs = bs, k = 12, xt = list(xc = 0.5))
      } else y ~ s(x, bs = bs, k = 7)
      # Boundary coefficients can cause trial-step warnings in scam's fitter;
      # comparisons concern the fitted function returned by that fitter.
      suppressWarnings(scam::scam(form, data = dd,
        sp = rep(0.1, if (bivar) 2L else 1L)))
    })
    names(models) <- bases
    cache <<- list(models = models, data = d)
    cache
  }
})

test_that("every SCAM constructor has a type and evaluates the fitted function", {
  fixtures <- scam_constructor_models()
  expect_length(fixtures$models, 41L)
  for (name in names(fixtures$models)) {
    m <- fixtures$models[[name]]
    sm <- m$smooth[[1L]]
    idx <- smooth_coef_indices(sm)
    expect_type(smooth_type(sm), "character")
    expect_true(all(is.finite(m$coefficients.t)), info = name)
    for (n in c(1L, 2L, 9L)) {
      nd <- fixtures$data[seq_len(n), ]
      # Native lipl drops matrix dimensions for a single prediction row.
      ref <- if (n == 1L) nd[c(1L, 1L), , drop = FALSE] else nd
      X <- predict(m, ref, type = "lpmatrix")[seq_len(n), idx, drop = FALSE]
      dimnames(X) <- NULL
      expected <- drop(X %*% m$coefficients.t[idx])
      for (frequentist in c(FALSE, TRUE)) {
        V <- if (frequentist) m$Ve.t else m$Vp.t
        expected_se <- sqrt(pmax(0, rowSums((X %*% V[idx, idx, drop = FALSE]) * X)))
        actual <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE,
          frequentist = frequentist)
        expect_equal(actual$.estimate, expected, tolerance = 1e-8, info = name)
        expect_equal(actual$.se, expected_se, tolerance = 1e-8, info = name)
        direct <- spline_values(sm, nd, m, unconditional = FALSE,
          overall_uncertainty = FALSE, frequentist = frequentist)$data[[1L]]
        expect_equal(direct$.estimate, expected, tolerance = 1e-8, info = name)
        expect_equal(direct$.se, expected_se, tolerance = 1e-8, info = name)
      }
      reversed <- smooth_estimates(m, data = nd[n:1L, ],
        overall_uncertainty = FALSE)
      expect_equal(reversed$.estimate, rev(expected), tolerance = 1e-8, info = name)
    }
  }
})

test_that("SCAM plots reproduce native functions with an explicit surface centre", {
  fixtures <- scam_constructor_models()
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({grDevices::dev.off(); unlink(file)}, add = TRUE)
  for (name in names(fixtures$models)) {
    m <- fixtures$models[[name]]
    sm <- m$smooth[[1L]]
    native <- scam::plot.scam(m, n = 20, n2 = 10, se = 1, rug = FALSE,
      too.far = 0)[[1L]]
    nd <- if (sm$dim == 2L) expand.grid(x = native$x, z = native$y) else {
      data.frame(x = native$x)
    }
    if (sm$by != "NA") nd[[sm$by]] <- 1
    original <- m
    p <- draw(m, data = nd, centre = "plot", dist = 0, rug = FALSE,
      parametric = FALSE, wrap = FALSE)
    expect_length(p, 1L)
    expect_s3_class(p[[1L]], "ggplot")
    expect_equal(p[[1L]]$data$.estimate, as.numeric(native$fit),
      tolerance = 1e-7, info = name)
    expect_identical(m, original)
    idx <- smooth_coef_indices(sm)
    X <- predict(m, nd, type = "lpmatrix")[, idx, drop = FALSE]
    dimnames(X) <- NULL
    if (sm$dim == 2L) X <- sweep(X, 2L, colMeans(X), FUN = "-")
    se <- sqrt(pmax(0, rowSums((X %*% m$Vp.t[idx, idx, drop = FALSE]) * X)))
    expect_equal(p[[1L]]$data$.se, se, tolerance = 1e-8, info = name)
    uncentred <- assemble(m, data = nd, dist = 0, rug = FALSE,
      parametric = FALSE)
    expected <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE)
    expect_equal(uncentred[[1L]]$data$.estimate, expected$.estimate,
      tolerance = 1e-8, info = name)
    expect_equal(uncentred[[1L]]$data$.se, expected$.se,
      tolerance = 1e-8, info = name)
  }
})

test_that("endpoint curves and constrained surfaces reach ggplot's renderer", {
  fixtures <- scam_constructor_models()
  for (name in c("mifo", "ipo", "lipl", "mpiBy", "tedmi", "tesmi1", "tismi")) {
    plots <- draw(fixtures$models[[name]], n = 20, n_2d = 10,
      rug = FALSE, parametric = FALSE, wrap = FALSE, dist = 0)
    expect_length(plots, 1L)
    expect_silent(ggplot2::ggplot_build(plots[[1L]]))
    expect_s3_class(ggplot2::ggplotGrob(plots[[1L]]), "gtable")
  }
})

test_that("SCAM cyclic margins and interactions keep their fitted matrices", {
  fixtures <- scam_constructor_models()
  d <- fixtures$data
  d$y <- 2 + exp(d$x) + sin(2 * pi * d$z) + d$error
  s <- mgcv::s
  ti <- mgcv::ti
  for (bs in c("tesmi1", "tesmi2", "tesmd1", "tesmd2", "tescx", "tescv",
      "tismi", "tismd")) {
    m <- suppressWarnings(scam::scam(
      y ~ s(x, z, bs = bs, k = c(5, 5), xt = "cc"), data = d,
      sp = c(0.1, 0.1)))
    nd <- d[seq_len(9), ]
    idx <- smooth_coef_indices(m$smooth[[1L]])
    X <- predict(m, nd, type = "lpmatrix")[, idx, drop = FALSE]
    dimnames(X) <- NULL
    actual <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE)
    expect_equal(actual$.estimate, drop(X %*% m$coefficients.t[idx]),
      tolerance = 1e-8, info = bs)
    expect_equal(actual$.se,
      sqrt(pmax(0, rowSums((X %*% m$Vp.t[idx, idx, drop = FALSE]) * X))),
      tolerance = 1e-8, info = bs)
  }
  for (bs in c("tismi", "tismd")) {
    m <- suppressWarnings(scam::scam(y ~ ti(x, k = 5) + ti(z, k = 5) +
      s(x, z, bs = bs, k = c(5, 5)), data = d, sp = rep(0.1, 4)))
    nd <- d[1:7, ]
    idx <- smooth_coef_indices(m$smooth[[3L]])
    X <- predict(m, nd, type = "lpmatrix")[, idx, drop = FALSE]
    dimnames(X) <- NULL
    actual <- smooth_estimates(m, select = 3L, data = nd,
      overall_uncertainty = FALSE)
    expect_equal(actual$.estimate, drop(X %*% m$coefficients.t[idx]),
      tolerance = 1e-8, info = bs)
  }
})

test_that("SCAM matrix-valued numeric-by terms evaluate linear functionals", {
  s <- mgcv::s
  d <- withr::with_seed(20, {
    X <- matrix(runif(480), 120, 4)
    L <- matrix(runif(480), 120, 4)
    list(X = X, L = L, y = rowSums(L * exp(X)) + rnorm(120, sd = 0.05))
  })
  m <- scam::scam(y ~ s(X, by = L, bs = "mpiBy", k = 6), data = d, sp = 0.1)
  nd <- data.frame(y = d$y[1:5])
  nd$X <- d$X[1:5, ]
  nd$L <- d$L[1:5, ]
  idx <- smooth_coef_indices(m$smooth[[1L]])
  X <- predict(m, nd, type = "lpmatrix")[, idx, drop = FALSE]
  dimnames(X) <- NULL
  actual <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE)
  expect_equal(actual$.estimate, drop(X %*% m$coefficients.t[idx]), tolerance = 1e-8)
  expect_equal(actual$.se,
    sqrt(pmax(0, rowSums((X %*% m$Vp.t[idx, idx, drop = FALSE]) * X))),
    tolerance = 1e-8)
})

test_that("centred SCAM surface bands use the same linear transformation", {
  m <- scam_constructor_models()$models$tedmi
  nd <- expand.grid(x = seq(0.1, 0.9, length.out = 8),
    z = seq(0.1, 0.9, length.out = 8))
  idx <- smooth_coef_indices(m$smooth[[1L]])
  X <- predict(m, nd, type = "lpmatrix")[, idx, drop = FALSE]
  X <- unname(sweep(X, 2L, colMeans(X), FUN = "-"))
  estimate <- drop(X %*% m$coefficients.t[idx])
  for (overall in c(FALSE, TRUE)) {
    G <- matrix(0, nrow(X), ncol(m$Vp.t))
    G[, idx] <- X
    if (overall) G[, 1L] <- 1
    se <- sqrt(pmax(0, rowSums((G %*% m$Vp.t) * G)))
    expected <- withr::with_seed(19,
      simultaneous_intervals(estimate, se, G, m$Vp.t,
        level = 0.95, n_sim = 200, n_cores = 1))
    p <- draw(m, data = nd, centre = "plot", interval = "simultaneous",
      overall_uncertainty = overall, n_sim = 200, seed = 19,
      parametric = FALSE, rug = FALSE, dist = 0, wrap = FALSE)[[1L]]
    expect_equal(p$data$.estimate, estimate, tolerance = 1e-8)
    expect_equal(p$data$.se, se, tolerance = 1e-8)
    expect_equal(p$data$.lower_ci, expected$lower, tolerance = 1e-8)
    expect_equal(p$data$.upper_ci, expected$upper, tolerance = 1e-8)
  }
})

test_that("SCAM intervals describe gradients of the function being displayed", {
  fixtures <- scam_constructor_models()
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({grDevices::dev.off(); unlink(file)}, add = TRUE)
  for (name in c("mpiBy", "ipo", "dpo", "tedmi", "tesmi1", "tismi")) {
    m <- fixtures$models[[name]]
    sm <- m$smooth[[1L]]
    idx <- smooth_coef_indices(sm)
    # Perturb transformed coefficients directly. Native plotting reads the
    # unrestricted coefficients, so update both stored parameterisations.
    base <- m
    base$coefficients.t[idx] <- 1
    base$coefficients[idx] <- ifelse(base$p.ident[idx], 0, 1)
    native <- scam::plot.scam(base, n = 20, n2 = 10, se = FALSE,
      rug = FALSE, too.far = 0)[[1L]]
    J <- vapply(idx, function(j) {
      perturbed <- base
      perturbed$coefficients.t[j] <- 2
      perturbed$coefficients[j] <- if (base$p.ident[j]) log(2) else 2
      result <- scam::plot.scam(perturbed, n = 20, n2 = 10, se = FALSE,
        rug = FALSE, too.far = 0)[[1L]]
      as.numeric(result$fit - native$fit)
    }, numeric(length(native$fit)))
    nd <- if (sm$dim == 2L) expand.grid(x = native$x, z = native$y) else {
      data.frame(x = native$x)
    }
    if (sm$by != "NA") nd[[sm$by]] <- 1
    p <- draw(m, data = nd, centre = "plot", dist = 0, rug = FALSE,
      parametric = FALSE, wrap = FALSE)[[1L]]
    expected <- sqrt(pmax(0, rowSums((J %*% m$Vp.t[idx, idx, drop = FALSE]) * J)))
    expect_equal(p$data$.se, expected, tolerance = 1e-7, info = name)
  }
})

test_that("all-missing SCAM smooth grids retain missing simultaneous intervals", {
  fixtures <- scam_constructor_models()
  nd <- data.frame(x = rep(NA_real_, 3), z = rep(NA_real_, 3), w = rep(1, 3))
  for (name in names(fixtures$models)) {
    result <- smooth_estimates(fixtures$models[[name]], data = nd,
      interval = "simultaneous", n_sim = 50, seed = 7,
      overall_uncertainty = FALSE)
    expect_equal(nrow(result), nrow(nd), info = name)
    expect_true(all(is.na(result$.estimate)), info = name)
    expect_true(all(is.na(result$.se)), info = name)
    expect_true(all(is.na(result$.lower_ci)), info = name)
    expect_true(all(is.na(result$.upper_ci)), info = name)
  }
})

test_that("saved SCAM estimates with previous type labels still render", {
  labels <- c(mpi = "Mono inc P spline", mpd = "Mono dec P spline",
    cx = "Convex P spline", cv = "Concave P spline",
    mdcx = "Mono dec conv P spline", mdcv = "Mono dec conc P spline",
    micx = "Mono inc conv P spline", micv = "Mono inc conc P spline",
    miso = "Mono inc 0 start P spline", mifo = "Mono inc 0 end P spline",
    po = "Positive P spline", ipo = "Positive increasing P spline",
    dpo = "Positive decreasing P spline", cpop = "Positive cyclic P spline",
    lmpi = "Locally increasing P spline", lipl = "Increasing then level P spline")
  models <- scam_constructor_models()$models
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  for (name in names(labels)) {
    current <- smooth_estimates(models[[name]], n = 20,
      overall_uncertainty = FALSE)
    current_type <- unique(current$.type)
    # Older saved tables have the previous type strings and no caption metadata.
    legacy <- current
    legacy$.type <- rep(labels[[name]], nrow(legacy))
    attr(legacy, "scam_smooth_info") <- NULL
    saveRDS(legacy, path)
    restored <- readRDS(path)
    plotted <- draw(restored)
    expect_s3_class(plotted, "patchwork")
    expect_equal(plotted[[1L]]$data$.estimate, current$.estimate, info = name)
    expect_equal(plotted[[1L]]$data$.se, current$.se, info = name)
    expect_identical(unique(current$.type), current_type)
    expect_identical(unique(restored$.type), labels[[name]])
    expect_silent(ggplot2::ggplot_build(plotted[[1L]]))
  }
})
