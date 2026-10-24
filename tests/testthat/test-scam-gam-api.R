skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_api_fixture <- local({
  s <- mgcv::s
  d <- withr::with_seed(927, {
    d <- data.frame(x = runif(160, .05, .95), z = runif(160),
      fac = factor(rep(c("a", "b"), 80)))
    d$y <- exp(d$x) + .4 * d$z + as.numeric(d$fac) + rnorm(160, sd = .15)
    d
  })
  list(data = d,
    model = scam::scam(y ~ fac + z + s(x, bs = "mpi", k = 6), data = d),
    factor = scam::scam(y ~ fac + s(x, by = fac, bs = "mpi", k = 6), data = d),
    bivariate = scam::scam(y ~ s(x, z, bs = "tedmi", k = c(4, 4)), data = d),
    no_intercept = scam::scam(y ~ 0 + s(x, bs = "mpi", k = 6), data = d))
})

test_that("SCAM extractors separate fixed effects and smooth contributions", {
  m <- scam_api_fixture$model
  original <- m
  expect_equal(fixed_effects(m), coef(m)[seq_len(m$nsdf)])
  expect_equal(model_terms(m), c(names(coef(m))[seq_len(m$nsdf)], "s(x)"))
  expect_equal(model_constant(m), unname(coef(m)["(Intercept)"]))
  expect_equal(model_constant(scam_api_fixture$no_intercept), 0)
  expect_equal(model_vars(m), c("fac", "z", "x"))
  expect_equal(parametric_terms(m), c(fac = "fac", z = "z"))
  expect_equal(which_smooths(m, "s(x)"), 1L)
  expect_equal(term_variables(m, "x"), "x")
  expect_equal(sort(term_names(m)), c("fac", "x", "z"))
  expect_equal(n_eta(m), 1L)
  expect_equal(dispersion(m), m$sig2)
  expect_equal(edf(m)$.edf, sum(m$edf[smooth_coef_indices(m$smooth[[1L]])]))
  expect_equal(edf(m, type = "alternative")$.edf,
    sum(m$edf1[smooth_coef_indices(m$smooth[[1L]])]))
  expect_error(edf(m, type = "unconditional"), "unconditional|smoothing", ignore.case = TRUE)
  expect_equal(overview(m)$edf, c(rep(1, m$nsdf), edf(m)$.edf), tolerance = 1e-4)
  expect_identical(m, original)
})

test_that("SCAM predictions and conditional contrasts use joint covariance", {
  m <- scam_api_fixture$factor
  cv <- conditional_values(m, "x", n_vals = 7, scale = "link")
  expect_equal(cv$.fitted, as.numeric(predict(m, cv, type = "link")))
  X <- lp_matrix(m, data = cv)
  expect_s3_class(X, "lp_matrix")
  expect_equal(unclass(X), unclass(predict(m, cv, type = "lpmatrix")),
    ignore_attr = TRUE)
  expect_equal(as.numeric(lp_matrix(m) %*% coef(m)),
    as.numeric(predict(m, type = "link")))
  expect_equal(model.matrix(m), predict(m, type = "lpmatrix"), ignore_attr = TRUE)
  cd <- conditional_differences(m, "fac", "x", n_vals = 7, scale = "link")
  a <- b <- as.data.frame(cd[c("x")])
  a$fac <- cd$.level_1
  b$fac <- cd$.level_2
  J <- predict(m, a, type = "lpmatrix") - predict(m, b, type = "lpmatrix")
  expect_equal(cd$.diff, as.numeric(J %*% coef(m)))
  expect_equal(cd$.se, sqrt(rowSums((J %*% m$Vp.t) * J)), ignore_attr = TRUE)
  dif <- difference_smooths(m, select = "s(x)", n = 7)
  expect_equal(dif$.diff, as.numeric(J %*% coef(m)) - coef(m)["facb"] * -1,
    tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("SCAM derivatives match numerical prediction gradients", {
  m <- scam_api_fixture$model
  nd <- scam_api_fixture$data[seq_len(8), ]
  h <- 1e-5
  a <- b <- nd
  a$x <- a$x + h / 2
  b$x <- b$x - h / 2
  J <- (predict(m, a, type = "lpmatrix") - predict(m, b, type = "lpmatrix")) / h
  d <- derivatives(m, data = nd, type = "central", eps = h)
  expect_equal(d$.derivative, as.numeric(J %*% coef(m)), tolerance = 1e-8,
    ignore_attr = TRUE)
  expect_equal(d$.se, sqrt(rowSums((J %*% m$Vp.t) * J)),
    tolerance = 1e-8, ignore_attr = TRUE)
  response <- response_derivatives(m, "x", data = nd, type = "central",
    eps = h, uncertainty = "delta")
  expect_equal(response$.derivative, d$.derivative, tolerance = 1e-8,
    ignore_attr = TRUE)
  expect_equal(response$.se, d$.se, tolerance = 1e-8, ignore_attr = TRUE)
  expect_s3_class(draw(d), "ggplot")
  surface <- scam_api_fixture$bivariate
  nd <- nd[c("x", "z")]
  nd$z <- .5
  a <- b <- nd
  a$x <- a$x + h / 2
  b$x <- b$x - h / 2
  J <- (predict(surface, a, type = "lpmatrix") -
    predict(surface, b, type = "lpmatrix")) / h
  partial <- partial_derivatives(surface, focal = "x", data = nd,
    type = "central", eps = h)
  expect_equal(partial$.partial_deriv, as.numeric(J %*% coef(surface)),
    tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(partial$.se, sqrt(rowSums((J %*% surface$Vp.t) * J)),
    tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("SCAM smooth difference derivatives use transformed coefficient covariance", {
  m <- scam_api_fixture$factor
  nd <- expand.grid(x = c(.2, .5, .8), z = .4,
    fac = levels(scam_api_fixture$data$fac))
  for (order in 1:2) {
    h <- .001
    upper <- lower <- nd
    upper$x <- upper$x + if (order == 1) h / 2 else h
    lower$x <- lower$x - if (order == 1) h / 2 else h
    X <- if (order == 1) {
      (predict(m, upper, type = "lpmatrix") - predict(m, lower, type = "lpmatrix")) / h
    } else {
      (predict(m, upper, type = "lpmatrix") - 2 * predict(m, nd, type = "lpmatrix") +
        predict(m, lower, type = "lpmatrix")) / h^2
    }
    K <- X[nd$fac == "a", , drop = FALSE] - X[nd$fac == "b", , drop = FALSE]
    a <- difference_smooths(m, select = "s(x)", data = nd, order = order, eps = h)
    expect_equal(a$.diff, as.numeric(K %*% coef(m)), tolerance = 1e-7)
    expect_equal(a$.se, unname(sqrt(rowSums((K %*% m$Vp.t) * K))), tolerance = 1e-7)
    expect_s3_class(difference_smooths(m, select = "s(x)", n = 5,
      order = order, interval = "simultaneous", n_sim = 100, seed = 264), "difference_smooth")
  }
})

test_that("SCAM ALE retains the fitted function and its covariance", {
  m <- scam_api_fixture$model
  a <- ale(m, "x", n_bins = 6, center = FALSE)
  nd <- scam_api_fixture$data[rep(1L, nrow(a)), ]
  nd$x <- a$.value
  X <- predict(m, nd, type = "lpmatrix")
  J <- sweep(X, 2, X[1L, ], "-")
  expect_equal(a$.ale, as.numeric(J %*% coef(m)), tolerance = 1e-9)
  expect_equal(a$.se, sqrt(rowSums((J %*% m$Vp.t) * J)),
    tolerance = 1e-9, ignore_attr = TRUE)
  expect_s3_class(draw(a), "ggplot")
  expect_error(ale(m, "x", unconditional = TRUE), "unconditional|smoothing",
    ignore.case = TRUE)
})

test_that("SCAM simultaneous bands and confint describe transformed-coordinate uncertainty", {
  m <- scam_api_fixture$model
  nd <- scam_api_fixture$data[seq_len(10), ]
  point <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE)
  band <- smooth_estimates(m, data = nd, overall_uncertainty = FALSE,
    interval = "simultaneous", n_sim = 400, seed = 2)
  expect_equal(band$.estimate, point$.estimate)
  expect_equal(band$.se, point$.se)
  expect_true(all(band$.crit >= qnorm(.975)))
  overall <- smooth_estimates(m, data = nd, overall_uncertainty = TRUE,
    interval = "simultaneous", n_sim = 400, seed = 2)
  J <- predict(m, nd, type = "lpmatrix")
  J[, names(coef(m)) %in% c("facb", "z")] <- 0
  expect_equal(overall$.se, sqrt(rowSums((J %*% m$Vp.t) * J)),
    ignore_attr = TRUE)
  ci <- stats::confint(m, data = nd)
  expect_equal(ci$.estimate, point$.estimate)
  expect_equal(ci$.se, point$.se)
  fitted <- fitted_values(m, data = nd, scale = "link", interval = "simultaneous",
    n_sim = 400, seed = 3)
  X <- predict(m, nd, type = "lpmatrix")
  expect_equal(fitted$.fitted, as.numeric(X %*% coef(m)))
  expect_equal(fitted$.se, sqrt(rowSums((X %*% m$Vp.t) * X)), ignore_attr = TRUE)
  expect_true(all(fitted$.lower_ci <= fitted$.fitted))
})

test_that("SCAM residual helpers and parametric effects retain native values", {
  m <- scam_api_fixture$model
  d <- scam_api_fixture$data
  pr <- partial_residuals(m)
  native <- predict(m, type = "terms")[, "s(x)"] +
    as.numeric(residuals(m, type = "working")) * sqrt(weights(m, type = "working"))
  expect_equal(pr[["s(x)"]], as.numeric(native))
  expect_equal(add_fitted(d, m)$.fitted, as.numeric(predict(m, d, type = "response")))
  expect_equal(add_fitted(d, scam_api_fixture$no_intercept,
    type = "terms")$.constant, rep(0, nrow(d)))
  expect_equal(add_residuals(d, m)$.residual, as.numeric(residuals(m)))
  expect_equal(add_partial_residuals(d, m)[["s(x)"]], pr[["s(x)"]])
  pe <- parametric_effects(m, overall_uncertainty = FALSE)
  z <- pe[pe$.term == "z", ]
  expect_equal(z$.partial, z$.value * unname(coef(m)["z"]))
  i <- match("z", names(coef(m)))
  expect_equal(z$.se, abs(z$.value) * sqrt(m$Vp.t[i, i]))
  expect_s3_class(rootogram(m), "rootogram")
})

test_that("SCAM penalty scales and observed concurvity use the correct coordinates", {
  m <- scam_api_fixture$model
  v <- variance_comp(m, rescale = FALSE)
  expect_equal(v$.variance, as.numeric(m$sig2 / m$sp))
  expect_equal(variance_comp(m)$.variance,
    as.numeric(m$sig2 / m$sp) * m$smooth[[1L]]$S.scale)
  expect_true(all(is.na(v$.lower_ci) & is.na(v$.upper_ci)))
  expect_match(attr(v, "parameterisation"), "unrestricted")
  expect_match(attr(penalty(m), "parameterisation"), "unrestricted")
  X <- predict(m, type = "lpmatrix")
  idx <- smooth_coef_indices(m$smooth[[1L]])
  term <- as.numeric(X[, idx] %*% coef(m)[idx])
  other <- X[, -idx, drop = FALSE]
  projection <- qr.fitted(qr(other), term)
  con <- model_concurvity(m, type = "observed")
  expect_equal(con$.concurvity[con$.term == "s(x)"],
    sum(projection^2) / sum(term^2), tolerance = 1e-8)
})

test_that("SCAM model matrices and concurvity omit plateau coefficients fixed to zero", {
  s <- mgcv::s
  d <- scam_api_fixture$data
  m <- scam::scam(y ~ z + s(x, bs = "lipl", k = 12, xt = list(xc = .5)),
    data = d)
  sm <- m$smooth[[1L]]
  idx <- smooth_coef_indices(sm)
  B <- mgcv::PredictMat(sm, d)
  X <- cbind(model.matrix(m$pterms, d), B[, seq_along(idx) + 1L, drop = FALSE])
  expect_equal(model.matrix(m), X, ignore_attr = TRUE)
  expect_equal(ncol(model.matrix(m)), length(coef(m)))
  term <- as.numeric(X[, idx] %*% coef(m)[idx])
  projection <- qr.fitted(qr(X[, -idx, drop = FALSE]), term)
  con <- model_concurvity(m, type = "observed")
  expect_equal(con$.concurvity[con$.term == sm$label],
    sum(projection^2) / sum(term^2), tolerance = 1e-8)
})

test_that("SCAM concurvity does not depend on model-matrix registration", {
  s <- mgcv::s
  d <- scam_api_fixture$data
  m <- scam::scam(y ~ s(z, bs = "mpi", k = 6) +
    s(x, bs = "lipl", k = 12, xt = list(xc = .5)), data = d)
  original <- m
  sm <- m$smooth[[2L]]
  idx <- smooth_coef_indices(sm)
  raw <- mgcv::PredictMat(sm, d)
  X <- cbind(model.matrix(m$pterms, d), mgcv::PredictMat(m$smooth[[1L]], d),
    raw[, seq_along(idx) + 1L, drop = FALSE])
  groups <- c(list(para = seq_len(m$nsdf)), lapply(m$smooth, smooth_coef_indices))
  names(groups)[-1L] <- smooths(m)
  observed <- vapply(groups, function(j) {
    term <- drop(X[, j, drop = FALSE] %*% coef(m)[j])
    projection <- qr.fitted(qr(X[, -j, drop = FALSE]), term)
    sum(projection^2) / sum(term^2)
  }, numeric(1))
  registered <- getS3method("model.matrix", "scam")
  withr::defer(registerS3method("model.matrix", "scam", registered,
    envir = asNamespace("stats")))
  methods <- list(gratia = model.matrix.scam)
  if (exists("model.matrix.scam", envir = asNamespace("scam"), inherits = FALSE)) {
    methods$scam <- getFromNamespace("model.matrix.scam", "scam")
  }
  reference <- NULL
  for (method in methods) {
    registerS3method("model.matrix", "scam", method, envir = asNamespace("stats"))
    overall <- model_concurvity(m)
    pairwise <- model_concurvity(m, pairwise = TRUE)
    for (label in names(groups)) {
      expect_equal(overall$.concurvity[overall$.type == "observed" &
        overall$.term == label], unname(observed[label]), tolerance = 1e-8)
      term <- drop(X[, groups[[label]], drop = FALSE] %*% coef(m)[groups[[label]]])
      for (other in setdiff(names(groups), label)) {
        projection <- qr.fitted(qr(X[, groups[[other]], drop = FALSE]), term)
        expect_equal(pairwise$.concurvity[pairwise$.type == "observed" &
          pairwise$.with == label & pairwise$.term == other],
          sum(projection^2) / sum(term^2), tolerance = 1e-8)
      }
    }
    actual <- list(overall = overall, pairwise = pairwise)
    if (is.null(reference)) reference <- actual else expect_equal(actual, reference)
  }
  expect_identical(m, original)
})

test_that("SCAM matrix extraction retains training offsets and excluded rows", {
  withr::local_options(na.action = "na.exclude")
  s <- mgcv::s
  d <- scam_api_fixture$data
  omitted <- c(3L, 31L)
  d$x[omitted] <- NA_real_
  m <- scam::scam(y ~ offset(z) + s(x, bs = "mpi", k = 6), data = d,
    offset = rep(.3, nrow(d)))
  X <- model.matrix(m)
  expect_equal(nrow(X), nrow(d))
  expect_true(all(is.na(X[omitted, , drop = FALSE])))
  expected <- d$z + .3
  expected[omitted] <- NA_real_
  expect_equal(attr(X, "model.offset"), expected)
  L <- lp_matrix(m)
  expect_equal(L, X, ignore_attr = TRUE)
  expect_equal(attr(L, "model.offset"), expected)
  explicit <- lp_matrix(m, data = m$model)
  expect_equal(nrow(explicit), nrow(m$model))
  expect_equal(attr(explicit, "model.offset"), d$z[-omitted])
  expect_equal(as.numeric(X %*% coef(m)) + attr(X, "model.offset"),
    as.numeric(stats::naresid(m$na.action, m$linear.predictors)))
})

test_that("SCAM pointwise likelihoods evaluate supplied transformed coefficients", {
  m <- scam_api_fixture$model
  beta <- rbind(coef(m), coef(m) + .001, coef(m) - .001)
  ll <- log_lik(m, method = "user", draws = beta)
  eta <- predict(m, type = "lpmatrix") %*% t(beta)
  reference <- t(vapply(seq_len(nrow(beta)), function(j) {
    dnorm(m$y, eta[, j], sqrt(m$sig2), log = TRUE)
  }, numeric(nrow(m$model))))
  expect_equal(ll, reference, ignore_attr = TRUE)
  expect_error(log_lik(m, unconditional = TRUE), "unconditional|smoothing",
    ignore.case = TRUE)
  for (invalid in list(NA, 1, "TRUE", logical(), c(FALSE, TRUE))) {
    expect_error(log_lik(m, unconditional = invalid), "single non-missing logical")
    expect_error(conditional_values(m, "x", unconditional = invalid),
      "single non-missing logical")
  }
  ar <- m
  ar$AR1.rho <- .5
  expect_error(log_lik(ar), "residual-correlated")
})

test_that("SCAM response-scale delta contrasts and derivatives use inverse-link gradients", {
  s <- mgcv::s
  d <- scam_api_fixture$data
  d$count <- withr::with_seed(73, rpois(nrow(d), exp(.5 + 2 * d$x +
    .3 * as.numeric(d$fac))))
  m <- scam::scam(count ~ fac + s(x, bs = "mpi", k = 6), data = d,
    family = poisson())
  nd <- d[seq_len(6), c("x", "fac")]
  a <- b <- nd
  h <- 1e-5
  a$x <- a$x + h / 2
  b$x <- b$x - h / 2
  mu_a <- as.numeric(predict(m, a, type = "response"))
  mu_b <- as.numeric(predict(m, b, type = "response"))
  J <- (predict(m, a, type = "lpmatrix") * mu_a -
    predict(m, b, type = "lpmatrix") * mu_b) / h
  result <- response_derivatives(m, "x", data = nd, type = "central",
    eps = h, uncertainty = "delta")
  expect_equal(result$.derivative, (mu_a - mu_b) / h)
  expect_equal(result$.se, sqrt(rowSums((J %*% m$Vp.t) * J)),
    ignore_attr = TRUE)
  cd <- conditional_differences(m, "fac", "x", n_vals = 5)
  a <- b <- as.data.frame(cd["x"])
  a$fac <- cd$.level_1
  b$fac <- cd$.level_2
  mu_a <- as.numeric(predict(m, a, type = "response"))
  mu_b <- as.numeric(predict(m, b, type = "response"))
  J <- predict(m, a, type = "lpmatrix") * mu_a -
    predict(m, b, type = "lpmatrix") * mu_b
  expect_equal(cd$.diff, mu_a - mu_b)
  expect_equal(cd$.se, sqrt(rowSums((J %*% m$Vp.t) * J)), ignore_attr = TRUE)
  beta <- rbind(coef(m), coef(m) + .001)
  ll <- log_lik(m, method = "user", draws = beta)
  mu <- exp(predict(m, type = "lpmatrix") %*% t(beta))
  expect_equal(ll, t(vapply(seq_len(nrow(beta)), function(i) {
    dpois(m$y, mu[, i], log = TRUE)
  }, numeric(nrow(d)))), ignore_attr = TRUE)
})

test_that("SCAM likelihood and predictive-check external generics dispatch", {
  skip_if_not_installed("loo")
  m <- scam_api_fixture$model
  beta <- post_draws(m, n_draws = 400, seed = 651)
  args <- list(method = "user", draws = beta)
  ll <- log_lik(m, method = "user", draws = beta)
  w <- loo::waic(m, sampling_args = args)
  expect_equal(w$estimates, loo::waic(ll)$estimates)
  l <- loo::loo(m, sampling_args = args, r_eff = 1, cores = 1)
  expect_equal(l$estimates, loo::loo(ll, r_eff = 1, cores = 1)$estimates)
  if (requireNamespace("bayesplot", quietly = TRUE)) {
    expect_s3_class(bayesplot::pp_check(m, n_draws = 5, seed = 52), "ggplot")
  }
})
