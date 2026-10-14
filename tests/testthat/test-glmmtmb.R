# glmmTMB is an optional dependency.
skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.9000")

tmb_data <- function() {
  set.seed(928)
  d <- data.frame(x = runif(180), z = runif(180),
    g = factor(rep(seq_len(18), 10)))
  d$y <- sin(6 * d$x) + cos(6 * d$z) + rnorm(18)[d$g] + rnorm(180, sd = .3)
  d
}

test_that("smooth coefficients reconstruct mixed model predictors in formula order", {
  d <- tmb_data()
  for (f in list(y ~ (1 | g) + s(x, k = 6) + s(z, k = 5),
    y ~ s(x, k = 6) + (1 | g) + s(z, k = 5),
    y ~ s(x, k = 6) + s(z, k = 5) + (1 | g))) {
    m <- glmmTMB::glmmTMB(f, d, REML = TRUE)
    a <- glmmtmb_smooth_model(m)
    # Check against the native full prediction after removing ordinary REs.
    p <- m$fit$parfull
    b <- p[names(p) == "b"]
    blocks <- m$modelInfo$reStruc$condReStruc
    sizes <- vapply(blocks, function(x) x$blockSize * x$blockReps, numeric(1))
    ends <- cumsum(c(0, sizes))
    pos <- which(lengths(m$modelInfo$reTrms$cond$smooth_info) == 0)
    re <- b[seq.int(ends[pos] + 1, ends[pos + 1])][d$g]
    value <- rep(a$coefficients[1], nrow(d))
    for (sm in a$smooth) {
      value <- value + drop(mgcv::PredictMat(sm, d) %*%
        smooth_coefs(m, select = sm$label))
    }
    expect_equal(unname(value + re), unname(predict(m)), tolerance = 1e-7)
    expect_identical(smooths(m), c("s(x)", "s(z)"))
    expect_length(get_smooths_by_id(m, 1), 1)
    # Deliberately corrupt the known-bad legacy indices.
    m$modelInfo$reTrms$cond$smooth_info[[which(lengths(m$modelInfo$reTrms$cond$smooth_info)>0)[1]]]$re$b_ind <- 1:1000
    expect_equal(smooth_coefs(m, "s(x)"), coef(a)[smooth_coef_indices(a$smooth[[1]])])
  }
})

test_that("joint covariance matches native ML and REML prediction intervals", {
  d <- tmb_data()
  for (reml in c(FALSE, TRUE)) {
    m <- glmmTMB::glmmTMB(y ~ s(x, k = 6), d, REML = reml)
    a <- glmmtmb_smooth_model(m, "cond")
    X <- cbind(1, mgcv::PredictMat(a$smooth[[1]], d))
    se <- sqrt(rowSums((X %*% a$Vp) * X))
    expect_equal(unname(se), unname(predict(m, se.fit = TRUE)$se.fit), tolerance = 1e-7)
    e <- smooth_estimates(m, data = d)
    expect_equal(e$.se, se, tolerance = 1e-7)
    expect_false(".component" %in% names(e))
    expect_error(smooth_estimates(m, unconditional = TRUE), "joint covariance")
    expect_error(smooth_estimates(m, frequentist = TRUE), "joint covariance")
  }
})

test_that("smooth plotting supports transformed, bivariate and numeric by smooths", {
  d <- tmb_data()
  for (f in list(y ~ s(log(x + 1), k = 6), y ~ s(x, z, k = 12),
    y ~ z + s(x, by = z, k = 6, m = c(2, 0)), y ~ s(x, bs = "ts", k = 6))) {
    m <- glmmTMB::glmmTMB(f, d, REML = TRUE)
    e <- smooth_estimates(m, n = 8, n_2d = 5)
    expect_true(all(is.finite(e$.estimate)))
    expect_s3_class(draw(m, n = 8, n_2d = 5), "patchwork")
  }
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 6), d, REML = TRUE)
  e <- smooth_estimates(m, data = data.frame(x = c(.2, NA, .8)))
  expect_true(is.na(e$.estimate[2]))
  expect_true(all(is.finite(e$.estimate[c(1, 3)])))
  expect_error(draw(m, residuals = TRUE), "partial residual")
  expect_s3_class(draw(m, parametric = TRUE), "patchwork")
  plain <- glmmTMB::glmmTMB(y ~ x, d)
  expect_length(smooths(plain), 0)
  expect_length(draw(plain, wrap = FALSE), 1)
  expect_error(smooth_estimates(plain), "No smooths")
})

test_that("all linear predictors share mgcv-style smooth selection and labels", {
  d <- tmb_data()
  set.seed(29)
  d$y <- rpois(nrow(d), exp(sin(6*d$x)))
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 5), ziformula = ~s(x, k = 4),
    family = poisson, data = d)
  expect_identical(smooths(m), c("s(x)", "s.1(x)"))
  expect_identical(unique(smooth_estimates(m)$.smooth), smooths(m))
  expect_identical(unique(smooth_estimates(m, select = 2)$.smooth), "s.1(x)")
  expect_identical(unique(smooth_estimates(m, select = c(FALSE, TRUE))$.smooth), "s.1(x)")
  expect_identical(unique(smooth_estimates(m, select = "s.1", partial_match = TRUE)$.smooth), "s.1(x)")
  expect_length(smooth_coefs(m, "s.1(x)"), 3)
  expect_length(draw(m, wrap = FALSE), 2)
  expect_length(draw(m, select = "s.1(x)", wrap = FALSE), 1)
  expect_identical(get_smooths_by_id(m, 2)[[1]]$label, "s.1(x)")
  e <- smooth_estimates(m, select = "s.1(x)", data = d)
  reference <- smooth_estimates(glmmtmb_smooth_model(m, "zi"), data = d)
  expect_equal(e$.estimate, reference$.estimate)
  expect_equal(e$.se, reference$.se)
  a <- glmmtmb_smooth_model(m, "zi")
  X <- cbind(1, mgcv::PredictMat(a$smooth[[1]], d))
  pr <- predict(m, type = "zlink", se.fit = TRUE)
  expect_equal(unname(drop(X %*% coef(a))), unname(pr$fit), tolerance = 1e-7)
  expect_equal(unname(sqrt(rowSums(X * (X %*% a$Vp)))),
    unname(pr$se.fit), tolerance = 1e-7)
  d$y <- sin(6*d$x) + rnorm(nrow(d), sd = exp(d$x))
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 5), dispformula = ~s(x, k = 4), data = d)
  expect_identical(smooths(m), c("s(x)", "s.1(x)"))
  expect_identical(unique(smooth_estimates(m)$.smooth), smooths(m))
  expect_length(draw(m, wrap = FALSE), 2)
  e <- smooth_estimates(m, select = "s.1(x)", data = d)
  reference <- smooth_estimates(glmmtmb_smooth_model(m, "disp"), data = d)
  expect_equal(e$.estimate, reference$.estimate)
  expect_equal(e$.se, reference$.se)
  g <- mgcv::gam(list(y ~ s(x, k = 5), ~s(x, k = 4)), data = d, family = mgcv::gaulss())
  expect_identical(smooths(m), smooths(g))
  a <- glmmtmb_smooth_model(m, "disp")
  X <- cbind(1, mgcv::PredictMat(a$smooth[[1]], d))
  pr <- predict(m, type = "disp", se.fit = TRUE)
  expect_equal(unname(drop(X %*% coef(a))), unname(log(pr$fit)), tolerance = 1e-7)
  expect_equal(unname(sqrt(rowSums(X * (X %*% a$Vp)))),
    unname(pr$se.fit / pr$fit), tolerance = 1e-7)
  # Predictors without smooths still determine mgcv-style numbering.
  m <- glmmTMB::glmmTMB(y ~ 1, dispformula = ~s(x, k = 4), data = d)
  expect_identical(smooths(m), "s.1(x)")
  expect_length(draw(m, wrap = FALSE), 1)
})

test_that("native residuals are preserved for observed and replaced responses", {
  d <- tmb_data()
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 6) + (1 | g), d, REML = TRUE)
  for (type in c("response", "pearson", "deviance")) {
    expect_equal(glmmtmb_residuals(m, type, d$y), unname(residuals(m, type)))
  }
  expect_equal(glmmtmb_residuals(m, "response", d$y + 1),
    unname(residuals(m, "response")) + 1)
  expect_identical(unname(model.response(model.frame(m))), d$y)
  expect_s3_class(appraise(m, n_simulate = 5, seed = 42), "patchwork")
  expect_s3_class(worm_plot(m, n_simulate = 5, seed = 42), "ggplot")
  expect_s3_class(residuals_linpred_plot(m), "ggplot")
  expect_s3_class(residuals_hist_plot(m), "ggplot")
  expect_s3_class(observed_fitted_plot(m), "ggplot")
  # The grouped-family automatic default must preserve native model defaults.
  for (fun in list(residuals_linpred_plot, residuals_hist_plot)) {
    expect_equal(fun(m)$data$residuals, unname(residuals(m, type = "pearson")))
    expect_equal(fun(m, type = NULL)$data$residuals,
      unname(residuals(m, type = "pearson")))
    for (type in c("response", "pearson", "deviance")) {
      expect_equal(fun(m, type = type)$data$residuals,
        unname(residuals(m, type = type)))
    }
  }
  expect_equal(residuals_linpred_plot(m)$data$eta, unname(predict(m, type = "link")))
  expect_s3_class(qq_plot(m, method = "uniform"), "ggplot")
  expect_s3_class(appraise(m, type = "quantile"), "patchwork")
  set.seed(4); before <- .Random.seed
  a <- qq_plot(m, n_simulate = 5, seed = 42)
  expect_identical(.Random.seed, before)
  expect_identical(a$data, qq_plot(m, n_simulate = 5, seed = 42)$data)
})

test_that("diagnostics align excluded rows and binomial counts", {
  d <- tmb_data(); d$x[3] <- NA
  m <- glmmTMB::glmmTMB(y ~ x, d, na.action = na.exclude)
  p <- observed_fitted_plot(m)
  expect_equal(nrow(p$data), nrow(d))
  expect_true(is.na(p$data$fitted[3]))
  expect_equal(nrow(qq_plot(m, n_simulate = 3)$data), nobs(m))
  d <- tmb_data(); d$n <- rep(c(5, 10), length.out = nrow(d))
  d$y <- rbinom(nrow(d), d$n, plogis(d$x))
  m <- glmmTMB::glmmTMB(cbind(y, n - y) ~ x, d, family = binomial)
  fam <- glmmtmb_family(m)
  y <- fam$rd(NULL, NULL, NULL)
  expect_true(all(y * d$n == round(y * d$n)))
  expect_equal(glmmtmb_residuals(m, "pearson", d$y/d$n), unname(residuals(m, "pearson")))
  expect_equal(observed_fitted_plot(m)$data$observed, d$y/d$n)
  expect_s3_class(appraise(m, n_simulate = 3), "patchwork")
})

test_that("conditional response generators have correct moments and support", {
  set.seed(92)
  n <- 40000L; mu <- rep(2, n); phi <- rep(.7, n)
  for (fam in c("gaussian", "poisson", "nbinom1", "nbinom2", "Gamma", "tweedie")) {
    rd <- glmmtmb_rd(fam, mu, phi, rep(0, n), rep(1, n), 1.5)
    y <- rd(NULL, NULL, NULL)
    variance <- switch(fam, gaussian = .7^2, poisson = 2,
      nbinom1 = 2*1.7, nbinom2 = 2+4/.7, Gamma = 4/.7, tweedie = .7*2^1.5)
    expect_equal(mean(y), 2, tolerance = .03)
    expect_equal(var(y), variance, tolerance = .05)
  }
  for (fam in c("beta", "binomial")) {
    y <- glmmtmb_rd(fam, rep(.4, n), rep(7,n), rep(0,n), rep(10,n))(NULL,NULL,NULL)
    expect_true(all(y >= 0 & y <= 1))
    expect_equal(mean(y), .4, tolerance = .02)
  }
  for (size in list(NULL, .5, 3)) {
    y <- glmmtmb_truncated_rng(rep(.1, n), size)
    p0 <- if (is.null(size)) dpois(0,.1) else dnbinom(0,mu=.1,size=size)
    expect_true(all(y >= 1))
    expect_equal(mean(y), .1/(1-p0), tolerance = .02)
  }
  y <- glmmtmb_rd("poisson", mu, phi, rep(.4,n), rep(1,n))(NULL,NULL,NULL)
  expect_equal(mean(y), 1.2, tolerance = .03)
  expect_true(all(glmmtmb_truncated_rng(rep(1e-12,100)) == 1))
})

test_that("QQ simulation works across supported glmmTMB distributions", {
  set.seed(139)
  d <- data.frame(x = runif(250))
  specs <- list(
    list(f = poisson(), y = rpois(250, exp(d$x))),
    list(f = glmmTMB::nbinom1(), y = rnbinom(250, mu=exp(d$x), size=exp(d$x)/.7)),
    list(f = glmmTMB::nbinom2(), y = rnbinom(250, mu=exp(d$x), size=2)),
    list(f = glmmTMB::beta_family(), y = rbeta(250, 3, 4)),
    list(f = Gamma(link="log"), y = rgamma(250, shape=3, scale=exp(d$x)/3)),
    list(f = glmmTMB::tweedie(), y = rtw(exp(d$x), 1.5, .4)),
    list(f = glmmTMB::truncated_poisson(), y = glmmtmb_truncated_rng(exp(d$x))),
    list(f = glmmTMB::truncated_nbinom1(), y = glmmtmb_truncated_rng(exp(d$x), exp(d$x)/.7)),
    list(f = glmmTMB::truncated_nbinom2(), y = glmmtmb_truncated_rng(exp(d$x), 2))
  )
  for (spec in specs) {
    d$y <- spec$y
    m <- glmmTMB::glmmTMB(y ~ x, data = d, family = spec$f)
    # Truncated nbinom1 has no native Pearson residual implementation.
    type <- if (spec$f$family == "truncated_nbinom1") "response" else "pearson"
    fam <- glmmtmb_family(m)
    expect_length(fam$rd(mu = NULL, wt = NULL, scale = NULL), nrow(d))
    expect_equal(glmmtmb_residuals(m, type, d$y), unname(residuals(m, type)))
    expect_s3_class(appraise(m, type = type, n_simulate = 3, seed = 92), "patchwork")
  }
  d$y <- rpois(250, exp(d$x)); d$y[runif(250)<.3] <- 0
  m <- glmmTMB::glmmTMB(y~x, ziformula=~x, family=glmmTMB::nbinom2(),
    dispformula=~x, data=d)
  expect_s3_class(appraise(m, n_simulate=3), "patchwork")
  d$y <- rgamma(250,3,1); d$y[runif(250)<.3] <- 0
  m <- glmmTMB::glmmTMB(y~x, ziformula=~x, family=glmmTMB::ziGamma(link="log"), data=d)
  expect_s3_class(appraise(m, n_simulate=3), "patchwork")
})

test_that("Gaussian smooths agree with mgcv and covariance failures are explicit", {
  d <- tmb_data()
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 6), d, REML = TRUE)
  g <- mgcv::gam(y ~ s(x, k = 6), data = d, method = "REML")
  expect_equal(smooth_estimates(m, data = d)$.estimate,
    smooth_estimates(g, data = d)$.estimate, tolerance = 1e-4)
  absent <- m; absent$sdr <- NULL
  expect_equal(smooth_estimates(absent, n = 5)$.se, smooth_estimates(m, n = 5)$.se)
  bad <- m; bad$sdr$pdHess <- FALSE
  # Ensure the stored joint precision path is exercised for either fit type.
  bad$sdr$jointPrecision <- TMB::sdreport(m$obj, getJointPrecision = TRUE)$jointPrecision
  expect_error(smooth_estimates(bad), "positive definite")
  bad <- m; bad$modelInfo$reStruc$condReStruc[[1]]$blockSize <- 100
  expect_error(smooth_estimates(bad), "align")
  expect_s3_class(get_smooth(m, "s(x)"), "mgcv.smooth")
})

test_that("glmmTMB plots render consistently", {
  skip_on_cran()
  d <- tmb_data()
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 6) + (1 | g), d, REML = TRUE)
  vdiffr::expect_doppelganger("glmmTMB smooth", draw(m, select = "s(x)"))
  vdiffr::expect_doppelganger("glmmTMB appraise", appraise(m, n_simulate = 20, seed = 928))
})

test_that("fitted Gamma dispersion is converted from sigma to shape", {
  set.seed(290)
  d <- data.frame(x = rep(c(-1,1), each=150))
  d$y <- rgamma(300, shape = exp(1+d$x), scale = 2/exp(1+d$x))
  m <- glmmTMB::glmmTMB(y~1, dispformula=~x, family=Gamma(link="log"), data=d)
  f <- glmmtmb_family(m)
  sims <- replicate(500, f$rd(NULL,NULL,NULL))
  mu <- predict(m,type="conditional")
  variance <- mu^2 * predict(m,type="disp")^2
  for (g in unique(d$x)) {
    y <- as.vector(sims[d$x==g,])
    expect_equal(mean(y), mean(mu[d$x==g]), tolerance=.03)
    expect_equal(var(y), mean(variance[d$x==g]), tolerance=.05)
  }
})

test_that("draw includes all three predictors in one model-wide layout", {
  set.seed(128)
  d <- data.frame(x = runif(600))
  d$y <- rnbinom(600, mu = exp(1 + sin(6 * d$x)),
    size = exp(1 + cos(6 * d$x)))
  d$y[runif(600) < plogis(-1 + 2 * cos(6 * d$x))] <- 0
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 4), ziformula = ~s(x, k = 4),
    dispformula = ~s(x, k = 4), family = glmmTMB::nbinom2(), data = d,
    control = glmmTMB::glmmTMBControl(optimizer = optim,
      optArgs = list(method = "BFGS")))
  expect_identical(smooths(m), c("s(x)", "s.1(x)", "s.2(x)"))
  expect_identical(unique(smooth_estimates(m, n = 5)$.smooth), smooths(m))
  expect_length(draw(m, wrap = FALSE), 3)
  expect_length(draw(m, select = c(1, 3), wrap = FALSE), 2)
  expect_equal(smooth_coefs(m, "s.2(x)") |> unname(),
    smooth_coefs.gam(glmmtmb_smooth_model(m, "disp", covariance = FALSE), "s(x)") |> unname())
  # Mean uncertainty must use only coefficients in the selected predictor,
  # even though the joint covariance retains cross-predictor correlations.
  for (i in seq_along(smooths(m))) {
    e <- smooth_estimates(m, select = i, data = d)
    reference <- smooth_estimates(glmmtmb_smooth_model(m, c("cond", "zi", "disp")[i]), data = d)
    expect_equal(e$.estimate, reference$.estimate)
    expect_equal(e$.se, reference$.se)
  }
})
