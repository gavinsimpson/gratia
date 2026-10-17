skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_basis_cases <- local({
  s <- mgcv::s
  d <- withr::with_seed(832, {
    x <- data.frame(x = runif(100, 0.1, 0.9), z = runif(100, 0.1, 0.9),
      w = runif(100, 0.5, 1.5))
    x$y <- exp(x$x) + x$z + rnorm(100, sd = 0.08)
    x
  })
  bases <- c("po", "dpo", "miso", "mifo", "lmpi", "lipl", "mpiBy", "tesmi1", "tismi")
  mixed <- c("tedi", "teid")
  mixed <- mixed[vapply(paste0("smooth.construct.", mixed, ".smooth.spec"),
    exists, logical(1), envir = asNamespace("scam"), inherits = FALSE)]
  bases <- c(bases, mixed)
  models <- lapply(bases, function(bs) {
    form <- if (bs %in% c("tesmi1", "tismi", "tedi", "teid")) {
      stats::as.formula(sprintf("y ~ s(x, z, bs = '%s', k = c(4, 4))", bs))
    } else if (bs %in% c("lmpi", "lipl")) {
      stats::as.formula(sprintf("y ~ s(x, bs = '%s', k = 10, xt = list(xc = 0.5))", bs))
    } else if (bs == "mpiBy") y ~ s(x, by = w, bs = "mpiBy", k = 6) else
      stats::as.formula(sprintf("y ~ s(x, bs = '%s', k = 6)", bs))
    suppressWarnings(scam::scam(form, data = d, not.exp = TRUE))
  })
  names(models) <- bases
  list(models = models, data = d)
})

test_that("SCAM tensors with one decreasing direction retain their fitted columns", {
  s <- mgcv::s
  d <- scam_basis_cases$data
  bases <- intersect(c("tedi", "teid"), names(scam_basis_cases$models))
  if (!length(bases)) skip("The tedi and teid tensor bases require SCAM 1.2-23")
  for (bs in bases) {
    spec <- s(x, z, bs = bs, k = c(5, 5))
    sm <- mgcv::smoothCon(spec, data = d)[[1L]]
    # These bases fix the first coefficient at zero. The remaining native
    # prediction columns already include the tensor and centring transforms.
    expected <- mgcv::PredictMat(sm, d)[, -1L, drop = FALSE]
    expect_equal(scam_smooth_matrix(sm, mgcv::PredictMat(sm, d)), sm$X,
      tolerance = 1e-10, info = bs)
    b <- basis(spec, data = d, at = d)
    expect_equal(matrix(b$.value, nrow = nrow(d), byrow = TRUE),
      unname(expected), tolerance = 1e-10, info = bs)
    m <- scam_basis_cases$models[[bs]]
    idx <- smooth_coef_indices(m$smooth[[1L]])
    nd <- d[c(1L, 4L, 8L), ]
    native <- predict(m, newdata = nd, type = "lpmatrix")[, idx, drop = FALSE]
    actual <- scam_smooth_matrix(m$smooth[[1L]],
      mgcv::PredictMat(m$smooth[[1L]], nd))
    expect_equal(unname(actual), unname(native), tolerance = 1e-10, info = bs)
  }
})

test_that("SCAM basis contributions sum to the fitted function", {
  nd <- scam_basis_cases$data[1:8, ]
  for (model in scam_basis_cases$models) {
    sm <- model$smooth[[1L]]
    idx <- smooth_coef_indices(sm)
    X <- lp_matrix(model, data = nd)[, idx, drop = FALSE]
    b <- basis(model, data = nd)
    expect_length(unique(b$.bf), length(idx))
    expect_equal(rowSums(matrix(b$.value, nrow = nrow(nd), byrow = TRUE)),
      as.numeric(X %*% coef(model)[idx]), tolerance = 1e-9)
    # Nonzero chosen weights also check bases whose fitted curve is nearly flat.
    arbitrary <- seq_along(idx) / length(idx)
    b <- tidy_basis(sm, at = nd, coefs = arbitrary)
    expect_equal(rowSums(matrix(b$.value, nrow = nrow(nd), byrow = TRUE)),
      as.numeric(X %*% arbitrary), tolerance = 1e-9)
    raw <- tidy_basis(sm, at = nd)
    expect_length(unique(raw$.bf), length(idx))
    expect_equal(rowSums(matrix(raw$.value, nrow = nrow(nd), byrow = TRUE)),
      rowSums(unname(X)), tolerance = 1e-9)
  }
})

test_that("SCAM adapters preserve native coefficient and covariance slots", {
  model <- scam_basis_cases$models$lmpi
  original <- model
  adapted <- scam_gam_adapter(model)
  expect_identical(model, original)
  expect_identical(adapted$coefficients, original$coefficients)
  expect_identical(adapted$coefficients.t, original$coefficients.t)
  expect_identical(adapted$Vp, original$Vp)
  expect_identical(adapted$Vp.t, original$Vp.t)
  expect_identical(adapted$Ve, original$Ve)
  expect_identical(adapted$Ve.t, original$Ve.t)
  expect_s3_class(adapted$pred.formula, "formula")
  expect_identical(scam_gam_adapter(adapted), adapted)
  expect_equal(stats::predict(adapted, scam_basis_cases$data[1:3, ]),
    stats::predict(original, scam_basis_cases$data[1:3, ]))
})

test_that("SCAM smooth registry covers the installed constructors", {
  ns <- asNamespace("scam")
  constructors <- grep("^smooth.construct.*[.]smooth.spec$", ls(ns), value = TRUE)
  expected <- sub("[.]smooth.spec$", ".smooth", sub("^smooth.construct[.]", "", constructors))
  expected[expected == "cpop.smooth"] <- "cpopspline.smooth"
  # Earlier SCAM releases lack the tedi and teid tensor bases.
  expect_true(all(expected %in% scam_smooth_classes()))
  expect_true(all(setdiff(scam_smooth_classes(), expected) %in%
    c("tedi.smooth", "teid.smooth")))
  for (model in scam_basis_cases$models) {
    sm <- model$smooth[[1L]]
    expect_identical(smooth_shape_constraints(model), list())
    expect_error(scam_smooth_matrix(sm, matrix(0, 1, 1)), "align|subscript|non-conformable")
  }
})

test_that("standalone SCAM bases use the constructed coefficient columns", {
  s <- mgcv::s
  d <- scam_basis_cases$data
  for (bs in c("po", "mifo", "tesmi1")) {
    spec <- if (bs == "tesmi1") s(x, z, bs = bs, k = c(5, 5)) else {
      s(x, bs = bs, k = 7)
    }
    sm <- mgcv::smoothCon(spec, data = d)[[1L]]
    for (rows in list(seq_len(nrow(d)), c(1L, 4L, 8L))) {
      nd <- d[rows, ]
      expected <- scam_smooth_matrix(sm, mgcv::PredictMat(sm, nd))
      b <- basis(spec, data = d, at = nd)
      expect_length(unique(b$.bf), ncol(sm$X))
      actual <- matrix(b$.value, nrow = nrow(nd), byrow = TRUE)
      expect_equal(actual, unname(expected), tolerance = 1e-10, info = bs)
    }
  }
})

test_that("fitted SCAM bases retain scale transformation parameters", {
  s <- mgcv::s
  d <- scam_basis_cases$data
  model <- scam::scam(y ~ s(scale(x), bs = "mpi", k = 7), data = d, sp = 0.1)
  nd <- d[1:6, ]
  b <- basis(model, data = nd)
  expected <- smooth_estimates(model, data = nd, overall_uncertainty = FALSE)
  actual <- rowSums(matrix(b$.value, nrow = nrow(nd), byrow = TRUE))
  expect_equal(actual, expected$.estimate, tolerance = 1e-9)
  # Predictions at the same point retain their value on a shorter grid.
  one <- basis(model, data = nd[1L, , drop = FALSE])
  expect_equal(sum(one$.value), expected$.estimate[1L], tolerance = 1e-9)
})
