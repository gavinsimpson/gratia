gamm_variance_data <- function() {
  set.seed(55)
  n <- 400L
  d <- data.frame(x = runif(n), z = runif(n),
    group = factor(rep(seq_len(20), each = 20)))
  b <- matrix(rnorm(60), 20, 3)
  d$y <- sin(6 * d$x) + cos(6 * d$z) + b[d$group, 1] +
    d$x * b[d$group, 2] + d$z * b[d$group, 3] + rnorm(n, sd = 0.4)
  d
}

expect_gamm_smooth_reference <- function(model) {
  for (rescale in c(FALSE, TRUE)) {
    got <- variance_comp(model, rescale = rescale)
    invisible(capture.output(ref <- mgcv::gam.vcomp(model$gam,
      rescale = rescale)))
    if (is.list(ref)) ref <- ref$vc
    expect_equal(head(got$.component, length(ref)), names(ref))
    expect_equal(head(got$.std_dev, length(ref)), unname(ref))
    expect_equal(got$.variance, got$.std_dev^2)
    expect_s3_class(got, c("variance_comp", "tbl_df", "tbl", "data.frame"))
    expect_named(got, c(".component", ".variance", ".std_dev",
      ".lower_ci", ".upper_ci"))
  }
}

test_that("gamm smooth components agree with the GAM penalty scale", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  d$y <- sin(6 * d$x) + cos(6 * d$z) + rnorm(nrow(d), sd = 0.4)
  for (method in c("ML", "REML")) {
    m <- gamm(y ~ s(x, k = 6) + s(z, k = 6), data = d, method = method)
    expect_gamm_smooth_reference(m)
    reference <- gam(y ~ s(x, k = 6) + s(z, k = 6), data = d, method = method)
    expect_equal(variance_comp(m)$.variance,
      variance_comp(reference)$.variance, tolerance = 1e-3)
    ci <- nlme::intervals(m$lme, level = 0.9, which = "var-cov")
    got <- variance_comp(m, coverage = 0.9, rescale = FALSE)
    # pdIdnot's constrained coefficients and intervals describe variances,
    # despite the sd() labels used by nlme for these custom blocks.
    expect_equal(got$.lower_ci[1:2],
      sqrt(c(ci$reStruct[["g"]][1, "lower"],
        ci$reStruct[["g.0"]][1, "lower"])))
    expect_equal(got$.upper_ci[1:2],
      sqrt(c(ci$reStruct[["g"]][1, "upper"],
        ci$reStruct[["g.0"]][1, "upper"])))
    expect_equal(tail(got$.lower_ci, 1), unname(ci$sigma["lower"]))
    scaled <- variance_comp(m, coverage = 0.9)
    scaling <- c(vapply(m$gam$smooth, function(sm) sm$S.scale, numeric(1)), 1)
    expect_equal(scaled$.lower_ci, got$.lower_ci * sqrt(scaling))
    expect_equal(scaled$.upper_ci, got$.upper_ci * sqrt(scaling))
  }
})

test_that("gamm native random coefficients keep their variances and intervals", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  m <- gamm(y ~ s(x, k = 6) + s(z, k = 6), random = list(group = ~1 + x),
    data = d, method = "REML")
  expect_gamm_smooth_reference(m)
  got <- variance_comp(m)
  expect_equal(got$.component,
    c("s(x)", "s(z)", "group:(Intercept)", "group:x", "scale"))
  covariance <- nlme::pdMatrix(m$lme$modelStruct$reStruct$group) * m$lme$sigma^2
  expect_equal(got$.variance[3:4], unname(diag(covariance)))
  ci <- nlme::intervals(m$lme, which = "var-cov")$reStruct$group
  expect_equal(got$.lower_ci[3:4], unname(ci[1:2, "lower"]))
  expect_equal(got$.upper_ci[3:4], unname(ci[1:2, "upper"]))
  # Estimates must come from lme, rather than the cached GAM scale and sp.
  changed <- m
  changed$gam$sp[] <- 1000
  changed$gam$sig2 <- 1000
  expect_equal(variance_comp(changed), got)
})

test_that("gamm maps factor smooths and tensor penalties back to terms", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  d$fac <- factor(rep(1:2, length.out = nrow(d)))
  models <- list(
    tensor = gamm(y ~ te(x, z, k = c(4, 4)), data = d, method = "REML"),
    t2 = gamm(y ~ t2(x, z, k = c(4, 4)), data = d, method = "REML"),
    factor_by = gamm(y ~ fac + s(z, by = fac, k = 6) + s(x, k = 6),
      data = d, method = "REML"),
    factor_smooth = gamm(y ~ s(z, k = 6) + s(x, group, bs = "fs", k = 5),
      data = d, method = "REML"),
    random_smooth = gamm(y ~ s(x, k = 6) + s(group, bs = "re"),
      data = d, method = "REML"),
    fixed_basis = gamm(y ~ s(z, k = 5, fx = TRUE) + s(x, k = 6),
      data = d, method = "REML")
  )
  for (m in models) expect_gamm_smooth_reference(m)
  tensor <- variance_comp(models$tensor)
  expect_equal(tensor$.component, c("te(x,z)1", "te(x,z)2", "scale"))
  expect_true(all(is.na(tensor$.lower_ci)))
  expect_true(all(is.na(tensor$.upper_ci)))
  fs <- variance_comp(models$factor_smooth)
  expect_equal(fs$.component,
    c("s(z)", "s(x,group)1", "s(x,group)2", "s(x,group)3", "scale"))
})

test_that("gamm handles random-only models and blocked native covariance", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  m <- gamm(y ~ x + z, random = list(group = ~1 + x), data = d,
    method = "REML")
  got <- variance_comp(m)
  expect_equal(got$.component, c("group:(Intercept)", "group:x", "scale"))
  expect_equal(got$.variance[1:2],
    unname(diag(nlme::pdMatrix(m$lme$modelStruct$reStruct$group))) * m$lme$sigma^2)
  blocked <- gamm(y ~ x + s(z, k = 6),
    random = list(group = nlme::pdBlocked(list(nlme::pdIdent(~x - 1),
      nlme::pdSymm(~1 + z)))), data = d, method = "REML")
  got <- variance_comp(blocked)
  expect_equal(got$.component,
    c("s(z)", "group:x", "group:(Intercept)", "group:z", "scale"))
  ci <- nlme::intervals(blocked$lme, which = "var-cov")$reStruct$group
  expect_equal(got$.lower_ci[2:4], unname(ci[1:3, "lower"]))
  expect_equal(got$.upper_ci[2:4], unname(ci[1:3, "upper"]))
  expect_equal(got$.variance[2:4],
    unname(diag(nlme::pdMatrix(blocked$lme$modelStruct$reStruct$group))) *
      blocked$lme$sigma^2)
})

test_that("gamm respects residual weights, correlation and unavailable intervals", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  d$time <- rep(seq_len(20), 20)
  m <- gamm(y ~ s(x, k = 6) + s(z, k = 6), random = list(group = ~1),
    weights = nlme::varFixed(~I(1 + x)),
    correlation = nlme::corAR1(form = ~time | group), data = d, method = "REML")
  expect_gamm_smooth_reference(m)
  got <- variance_comp(m)
  expect_equal(got$.component, c("s(x)", "s(z)", "group:(Intercept)", "scale"))
  expect_equal(tail(got$.variance, 1), m$lme$sigma^2)
  for (unavailable in list(NULL, "Non-positive definite approximate variance-covariance")) {
    changed <- m
    changed$lme$apVar <- unavailable
    result <- variance_comp(changed)
    expect_equal(result$.variance, got$.variance)
    expect_true(all(is.na(result$.lower_ci)))
    expect_true(all(is.na(result$.upper_ci)))
  }
  fixed <- gamm(y ~ s(x, k = 6), random = list(group = ~1), data = d,
    control = nlme::lmeControl(sigma = 0.4), method = "REML")
  expect_false("scale" %in% variance_comp(fixed)$.component)
})

test_that("gamm PQL components retain the fitted working-model scale", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  d$count <- rpois(nrow(d), exp(sin(6 * d$x) + rnorm(20, sd = 0.4)[d$group]))
  m <- gamm(count ~ s(x, k = 6), random = list(group = ~1), data = d,
    family = poisson(), verbosePQL = FALSE)
  expect_gamm_smooth_reference(m)
  got <- variance_comp(m)
  expect_equal(got$.component, c("s(x)", "group:(Intercept)"))
  expect_equal(got$.variance[2],
    unname(nlme::pdMatrix(m$lme$modelStruct$reStruct$group)[1, 1]) * m$lme$sigma^2)
})

test_that("gamm variance components validate the model and arguments", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  m <- gamm(y ~ s(x, k = 6), data = d)
  expect_error(variance_comp(structure(list(), class = "gamm")),
    "valid 'gam' and 'lme'")
  expect_error(variance_comp(m, coverage = 1), "between zero and one")
  expect_error(variance_comp(m, rescale = NA), "TRUE or FALSE")
})

test_that("gamm intervals handle shared native variances and alternative coordinates", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- gamm_variance_data()
  for (natural in c(FALSE, TRUE)) {
    m <- gamm(y ~ s(x, k = 6) + s(z, k = 6),
      random = list(group = ~1 + x), data = d, method = "REML",
      control = nlme::lmeControl(natural = natural))
    got <- variance_comp(m)
    ci <- nlme::intervals(m$lme, which = "var-cov")$reStruct$group
    if (natural) {
      lower <- unname(ci[1:2, "lower"])
      upper <- unname(ci[1:2, "upper"])
    } else {
      lower <- sqrt(unname(ci[c(1, 3), "lower"]))
      upper <- sqrt(unname(ci[c(1, 3), "upper"]))
    }
    expect_equal(got$.lower_ci[3:4], lower)
    expect_equal(got$.upper_ci[3:4], upper)
  }
  m <- gamm(y ~ s(x, k = 6) + s(z, k = 6),
    random = list(group = nlme::pdIdent(~1 + x)), data = d, method = "REML")
  got <- variance_comp(m)
  ci <- nlme::intervals(m$lme, which = "var-cov")$reStruct$group
  expect_equal(got$.variance[3], got$.variance[4])
  expect_equal(got$.lower_ci[3:4], rep(unname(ci[1, "lower"]), 2))
  expect_equal(got$.upper_ci[3:4], rep(unname(ci[1, "upper"]), 2))
})
