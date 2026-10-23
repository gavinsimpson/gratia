gamm4_variance_data <- function() {
  set.seed(71)
  n <- 180L
  d <- data.frame(x = runif(n), z = runif(n),
    group = factor(rep(seq_len(12), each = 15)),
    fac = factor(rep(1:2, length.out = n)))
  d$y <- sin(6 * d$x) + cos(6 * d$z) +
    rnorm(12, sd = 0.5)[d$group] + rnorm(n, sd = 0.3)
  d
}

test_that("gamm4 smooth estimates use the common GAM penalty scale", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  d$y <- sin(6 * d$x) + cos(6 * d$z) + rnorm(nrow(d), sd = 0.3)
  m <- gamm4::gamm4(y ~ s(x, k = 5) + s(z, k = 5), data = d)
  reference <- list(
    gam(y ~ s(x, k = 5) + s(z, k = 5), data = d, method = "REML"),
    bam(y ~ s(x, k = 5) + s(z, k = 5), data = d, method = "fREML"),
    gamm(y ~ s(x, k = 5) + s(z, k = 5), data = d, method = "REML")
  )
  for (rescale in c(FALSE, TRUE)) {
    got <- variance_comp(m, rescale = rescale, interval_method = "none")
    expect_s3_class(got, c("variance_comp", "tbl_df", "tbl", "data.frame"))
    expect_equal(got$.component, c("s(x)", "s(z)", "scale"))
    expect_named(got, c(".component", ".variance", ".std_dev",
      ".lower_ci", ".upper_ci"))
    expect_equal(got$.variance, got$.std_dev^2)
    expect_true(all(is.na(got$.lower_ci)))
    for (ref in reference) {
      expect_equal(got$.variance, variance_comp(ref, rescale = rescale)$.variance,
        tolerance = 1e-3)
    }
  }
  changed <- m
  changed$gam$sp[] <- 1000
  changed$gam$sig2 <- 1000
  expect_equal(variance_comp(changed, interval_method = "none"),
    variance_comp(m, interval_method = "none"))
})

test_that("gamm4 maps every smooth penalty to its fitted block", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  models <- list(
    tensor = gamm4::gamm4(y ~ t2(x, z, k = c(4, 4)),
      random = ~(1 | group), data = d),
    fs = gamm4::gamm4(y ~ s(x, group, bs = "fs", k = 5), data = d),
    factor_by = gamm4::gamm4(y ~ fac + s(x, by = fac, k = 5), data = d),
    random_smooth = gamm4::gamm4(y ~ s(x, k = 5) + s(group, bs = "re"), data = d),
    fixed = gamm4::gamm4(y ~ s(x, k = 5, fx = TRUE) + s(z, k = 5), data = d)
  )
  for (m in models) {
    raw <- lme4::VarCorr(m$mer)
    got <- variance_comp(m, interval_method = "none")
    unscaled <- variance_comp(m, rescale = FALSE, interval_method = "none")
    index <- 0L
    for (sm in m$gam$smooth) {
      if (!length(sm$S)) next
      rows <- index + seq_along(sm$S)
      index <- max(rows)
      expect_equal(got$.component[rows],
        names(m$gam$sp)[seq.int(sm$first.sp, sm$last.sp)])
      values <- vapply(sm$lmer.name, function(nm) unname(raw[[nm]][1, 1]),
        numeric(1))
      expect_equal(unscaled$.variance[rows], unname(values))
      expect_equal(got$.variance[rows], unname(values * sm$S.scale))
    }
  }
  expect_equal(head(variance_comp(models$tensor, interval_method = "none")$.component,
    3), c("t2(x,z)rr", "t2(x,z)nr", "t2(x,z)rn"))
})

test_that("gamm4 native blocks retain separate variance labels", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  d$y <- d$y + d$x * rnorm(12, sd = 0.7)[d$group]
  for (f in list(~(1 + x | group), ~(1 | group) + (0 + x | group))) {
    m <- gamm4::gamm4(y ~ s(z, k = 5), random = f, data = d)
    raw <- as.data.frame(lme4::VarCorr(m$mer))
    raw <- raw[is.na(raw$var2) & raw$grp != "Xr", ]
    got <- variance_comp(m, interval_method = "none")
    expect_equal(got$.variance[-1], raw$vcov)
    expect_equal(got$.component[-1], ifelse(raw$grp == "Residual", "scale",
      paste(raw$grp, raw$var1, sep = ":")))
    expect_equal(variance_comp(m, rescale = FALSE,
      interval_method = "none")$.variance[-1], raw$vcov)
  }
  m <- gamm4::gamm4(y ~ x, random = ~(1 | group), data = d)
  expect_equal(variance_comp(m, interval_method = "none")$.component,
    c("group:(Intercept)", "scale"))
})

test_that("gamm4 profile intervals follow the penalty transformation", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | group), data = d)
  theta <- lme4::getME(m$mer, "theta")
  design <- lme4::getME(m$mer, "Zt")
  ref <- stats::confint(stats::profile(m$mer, which = "theta_", signames = FALSE),
    level = 0.9)
  got <- variance_comp(m, coverage = 0.9)
  rows <- match(c("sd_s(x)|Xr", "sd_(Intercept)|group", "sigma"), rownames(ref))
  scaling <- c(sqrt(m$gam$smooth[[1]]$S.scale), 1, 1)
  expect_true(all(is.finite(got$.lower_ci)))
  expect_equal(got$.lower_ci, unname(ref[rows, 1]) * scaling)
  expect_equal(got$.upper_ci, unname(ref[rows, 2]) * scaling)
  expect_equal(lme4::getME(m$mer, "theta"), theta)
  expect_equal(lme4::getME(m$mer, "Zt"), design)
  raw <- variance_comp(m, rescale = FALSE, coverage = 0.9)
  expect_equal(got$.lower_ci, raw$.lower_ci * scaling)
  expect_equal(got$.upper_ci, raw$.upper_ci * scaling)
})

test_that("gamm4 profiles generalized fits with a fixed scale", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  for (fam in list(poisson(), binomial())) {
    eta <- sin(6 * d$x) + rnorm(12, sd = 0.5)[d$group]
    d$count <- if (fam$family == "poisson") rpois(nrow(d), exp(eta)) else
      rbinom(nrow(d), 1, plogis(eta))
    m <- gamm4::gamm4(count ~ s(x, k = 5), random = ~(1 | group),
      data = d, family = fam)
    got <- variance_comp(m, coverage = 0.9)
    expect_equal(got$.component, c("s(x)", "group:(Intercept)"))
    expect_true(all(is.finite(got$.lower_ci)))
    expect_true(all(is.finite(got$.upper_ci)))
    expect_true(all(got$.lower_ci <= got$.std_dev))
    expect_true(all(got$.upper_ci >= got$.std_dev))
    ref <- stats::confint(stats::profile(m$mer, which = "sd_s(x)|Xr",
      signames = FALSE), level = 0.9)
    expect_equal(got$.lower_ci[1], unname(ref[1, 1]) *
      sqrt(m$gam$smooth[[1]]$S.scale))
  }
})

test_that("gamm4 keeps estimates and other bounds when a profile fails", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | group), data = d)
  # Isolate failure handling at the external profiling boundary.
  testthat::local_mocked_bindings(
    profile = function(fitted, which, ...) {
      if (grepl("s(x)", which, fixed = TRUE)) stop("profile failed")
      structure(list(parameter = which), class = "test_variance_profile")
    },
    confint = function(object, level, ...) {
      matrix(c(0.1, 2), 1, 2, dimnames = list(object$parameter, NULL))
    }, .package = "stats")
  expect_warning(got <- variance_comp(m), "s\\(x\\).*profile failed")
  expect_true(is.na(got$.lower_ci[1]))
  expect_equal(got$.lower_ci[-1], c(0.1, 0.1))
  expect_equal(got$.variance,
    variance_comp(m, interval_method = "none")$.variance)
})

test_that("gamm4 variance components validate arguments", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  m <- gamm4::gamm4(y ~ s(x, k = 5), data = d)
  expect_error(variance_comp(m, coverage = NA), "between zero and one")
  expect_error(variance_comp(m, rescale = 1), "TRUE or FALSE")
  expect_error(variance_comp(m, interval_method = "unknown"), "arg")
  expect_error(variance_comp(m, prof.scale = "varcov"), "parameters and scale")
  expect_error(variance_comp(m, interval_method = "none", maxpts = 10),
    "Profile controls require")
})

test_that("gamm4 profiles separate t2 penalties", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  d$y <- sin(6 * d$x) + cos(6 * d$z) +
    3 * sin(6 * d$x) * cos(6 * d$z) + rnorm(nrow(d), sd = 0.3)
  m <- gamm4::gamm4(y ~ t2(x, z, k = c(4, 4)), data = d)
  got <- variance_comp(m, coverage = 0.9)
  expect_equal(got$.component, c("t2(x,z)rr", "t2(x,z)nr", "t2(x,z)rn", "scale"))
  expect_true(all(is.finite(got$.lower_ci)))
  expect_true(all(is.finite(got$.upper_ci)))
  ref <- stats::confint(stats::profile(m$mer, which = "sd_t2(x,z)|Xr.0",
    signames = FALSE), level = 0.9)
  expect_equal(got$.lower_ci[2], unname(ref[1, 1]) *
    sqrt(m$gam$smooth[[1]]$S.scale[2]))
  expect_equal(got$.upper_ci[2], unname(ref[1, 2]) *
    sqrt(m$gam$smooth[[1]]$S.scale[2]))
})

test_that("gamm4 preserves estimates when GLMM scale profiling is unavailable", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- gamm4_variance_data()
  eta <- sin(6 * d$x) + rnorm(12, sd = 0.3)[d$group]
  d$y <- rgamma(nrow(d), shape = 4, scale = exp(eta) / 4)
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | group),
    family = Gamma(link = "log"), data = d)
  expect_warning(got <- variance_comp(m), "estimated scale")
  expect_equal(got, variance_comp(m, interval_method = "none"))
})
