# Independent oracle using full matrices; unlike the adapter it neither invokes
# the shared helper nor uses predict.gam() SEs to standardize the simulations.
fitted_joint_reference <- function(model, data, seed = 44, level = .95,
                                   n = 300, unconditional = FALSE, ...) {
  X <- predict(model, newdata = data, type = "lpmatrix", ...)
  V <- vcov(model, unconditional = unconditional)
  se <- sqrt(diag(X %*% V %*% t(X)))
  fit <- as.vector(predict(model, newdata = data, type = "link", ...))
  draws <- withr::with_seed(seed, mvnfast::rmvn(n, rep(0, ncol(X)), V, ncores = 1))
  maxima <- apply(abs(sweep(draws %*% t(X), 2, se, `/`)), 1, max)
  crit <- unname(quantile(maxima, level, type = 8))
  list(fit = fit, se = unname(se), lower = fit - crit * se, upper = fit + crit * se)
}

test_that("fitted simultaneous intervals cover arbitrary factor combinations", {
  withr::local_seed(120)
  d <- data.frame(x = runif(100), fac = factor(rep(c("a", "b"), 50)))
  d$y <- sin(5 * d$x) + (d$fac == "b") + rnorm(100, sd = .2)
  model <- mgcv::gam(y ~ fac + s(x, k = 6), data = d, method = "REML")
  nd <- d[c(10, 1), ]
  for (unc in c(FALSE, TRUE)) {
    for (selection in list(list(), list(terms = "s(x)"), list(exclude = "s(x)"))) {
      reference <- do.call(fitted_joint_reference, c(list(model = model,
        data = nd, unconditional = unc), selection))
      fv <- do.call(fitted_values, c(list(object = model, data = nd,
        interval = "simultaneous", n_sim = 300, seed = 44, unconditional = unc), selection))
      expect_equal(fv$.fitted, reference$fit)
      expect_equal(fv$.se, reference$se)
      expect_equal(fv$.lower_ci, unname(reference$lower))
      expect_equal(fv$.upper_ci, unname(reference$upper))
      expect_named(fv, c(".row", names(nd), ".fitted", ".se", ".lower_ci", ".upper_ci"))
    }
  }
  default <- fitted_values(model, data = nd)
  before <- .Random.seed
  expect_identical(default, fitted_values(model, data = nd, interval = "confidence",
    n_sim = NA, n_cores = NA, seed = NA))
  expect_identical(.Random.seed, before)
  p <- predict(model, newdata = nd, se.fit = TRUE)
  expect_equal(default$.lower_ci, as.vector(p$fit - qnorm(.975) * p$se.fit))
  expect_error(fitted_values(model, interval = "bad"), "arg")
  expect_error(fitted_values(model, interval = "simultaneous", seed = NA), "seed")
  expect_error(fitted_values(model, interval = "simultaneous", n_sim = 0), "n_sim")
  expect_error(fitted_values(model, interval = "simultaneous", unconditional = NA), "unconditional")
})

test_that("response intervals preserve offsets and monotone link transformations", {
  withr::local_seed(122)
  d <- data.frame(x = runif(100, 1, 3), exposure = runif(100, 1, 2))
  d$y <- rpois(100, exp(.2 * d$x) * d$exposure)
  model <- mgcv::gam(y ~ s(log(x), k = 5) + offset(log(exposure)),
    family = poisson(), data = d, method = "REML")
  nd <- d[c(1, 25, 80), ]
  reference <- fitted_joint_reference(model, nd)
  link <- fitted_values(model, data = nd, scale = "link", interval = "simultaneous",
    seed = 44, n_sim = 300)
  response <- fitted_values(model, data = nd, interval = "simultaneous", seed = 44, n_sim = 300)
  expect_equal(link$.fitted, reference$fit)
  expect_equal(link$.lower_ci, unname(reference$lower))
  expect_equal(response$.fitted, exp(link$.fitted))
  expect_equal(response$.lower_ci, exp(link$.lower_ci))
  expect_equal(response$.upper_ci, exp(link$.upper_ci))
  expect_identical(response$.se, link$.se)
  expect_identical(link, fitted_values(model, data = nd, scale = "linear predictor",
    interval = "simultaneous", seed = 44, n_sim = 300))

  d$y <- rgamma(100, shape = 100, scale = exp(d$x / 3) / 100)
  inverse <- mgcv::gam(y ~ s(x, k = 5), family = Gamma(), data = d, method = "REML")
  a <- fitted_values(inverse, data = nd, scale = "link", interval = "simultaneous",
    seed = 44, n_sim = 300)
  b <- fitted_values(inverse, data = nd, interval = "simultaneous", seed = 44, n_sim = 300)
  expect_true(all(a$.lower_ci > 0))
  expect_equal(b$.lower_ci, 1 / a$.upper_ci)
  expect_equal(b$.upper_ci, 1 / a$.lower_ci)
  # Inflate covariance to deterministically exercise the pole guard.
  inverse$Vp <- inverse$Vp * 1e6
  expect_error(fitted_values(inverse, data = nd, interval = "simultaneous",
    seed = 44, n_sim = 300), "domain boundary")
  expect_silent(fitted_values(inverse, data = nd, scale = "link",
    interval = "simultaneous", seed = 44, n_sim = 300))
})

test_that("simultaneous fitted values restore missing rows and preserve RNG", {
  withr::local_seed(123)
  d <- data.frame(x = runif(60), y = rnorm(60))
  d$y[c(2, 5)] <- NA
  model <- mgcv::gam(y ~ s(x, k = 5), data = d, na.action = na.exclude)
  before <- .Random.seed
  fv <- fitted_values(model, interval = "simultaneous", seed = 44, n_sim = 300)
  expect_identical(.Random.seed, before)
  expect_identical(fv$.row, seq_len(nrow(d)))
  expect_true(all(is.na(fv$.fitted[c(2, 5)])))
  nd <- d[c(1, 2, 3), ]
  nd$x[3] <- NA
  a <- fitted_values(model, data = nd, interval = "simultaneous", seed = 44, n_sim = 300)
  expect_true(is.finite(a$.fitted[2])) # missing response permits prediction
  expect_true(is.na(a$.fitted[3]))
  b <- fitted_values(model, data = nd, interval = "simultaneous", seed = 44,
    n_sim = 300, na.action = na.omit)
  expect_identical(b$.row, 1:2)
  expect_equal(b$.lower_ci, a$.lower_ci[1:2])
  expect_identical(.Random.seed, before)
  nd$x[] <- NA
  expect_true(all(is.na(fitted_values(model, data = nd,
    interval = "simultaneous", n_sim = 300)$.fitted)))
  expect_equal(nrow(fitted_values(model, data = nd[FALSE, ],
    interval = "simultaneous", n_sim = 300)), 0)
  expect_identical(.Random.seed, before) # dummy layout rows must not consume draws
  rm(".Random.seed", envir = .GlobalEnv)
  expect_silent(fitted_values(model, interval = "simultaneous", seed = 44, n_sim = 300))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("BAM, GAMM and covariance fallback use the same joint calculation", {
  nd <- head(model.frame(m_gam), 3)
  attr(nd, "terms") <- NULL
  for (model in list(m_gamm$gam,
    mgcv::bam(formula(m_gam), data = model.frame(m_gam), discrete = TRUE))) {
    reference <- fitted_joint_reference(model, nd)
    fv <- fitted_values(model, data = nd, interval = "simultaneous", seed = 44, n_sim = 300)
    expect_equal(fv$.fitted, reference$fit)
    expect_equal(fv$.se, reference$se)
    expect_equal(fv$.lower_ci, unname(reference$lower))
  }
  expect_identical(fitted_values(m_gamm, data = nd, interval = "simultaneous", seed = 44, n_sim = 300),
    fitted_values(m_gamm$gam, data = nd, interval = "simultaneous", seed = 44, n_sim = 300))
  model <- m_gam
  model$Vc <- NULL
  expect_warning(actual <- fitted_values(model, data = nd, unconditional = TRUE,
    interval = "simultaneous", seed = 44, n_sim = 300), "Using uncorrected covariance")
  expect_identical(actual, fitted_values(model, data = nd,
    interval = "simultaneous", seed = 44, n_sim = 300))
})

test_that("unsupported simultaneous models keep their existing pointwise path", {
  for (model in list(m_ocat, m_gaulss, m_ziP, m_scam)) {
    expect_error(fitted_values(model, interval = "simultaneous"), "not supported")
    expect_silent(fitted_values(model, interval = "confidence"))
  }
})

test_that("supported extended families use their joint coefficient covariance", {
  withr::local_seed(129)
  d <- data.frame(x = runif(100))
  for (fam in list(mgcv::nb(), mgcv::tw(), mgcv::betar(), mgcv::scat())) {
    d$y <- switch(family_type(fam),
      negative_binomial = rnbinom(100, mu = exp(d$x), size = 3),
      tweedie = rgamma(100, shape = 2, scale = exp(d$x)),
      beta_regression = rbeta(100, shape1 = exp(d$x), shape2 = 2),
      scaled_t = d$x + rt(100, df = 5))
    model <- mgcv::gam(y ~ s(x, k = 5), data = d, family = fam, method = "REML")
    nd <- d[c(1, 40), ]
    reference <- fitted_joint_reference(model, nd)
    a <- fitted_values(model, data = nd, interval = "simultaneous", scale = "link",
      seed = 44, n_sim = 300)
    b <- fitted_values(model, data = nd, interval = "simultaneous", seed = 44, n_sim = 300)
    expect_equal(a$.se, reference$se)
    expect_equal(a$.lower_ci, unname(reference$lower))
    expect_equal(b$.fitted, family(model)$linkinv(a$.fitted))
    expect_equal(b$.lower_ci, family(model)$linkinv(a$.lower_ci))
  }
})

test_that("excluded random effects and model environments share prediction semantics", {
  withr::local_seed(131)
  d <- data.frame(x = runif(100, 1, 2), g = factor(rep(1:10, each = 10)))
  d$y <- log(d$x) + rnorm(10)[d$g] + rnorm(100, sd = .3)
  transform <- function(x) log(x)
  model <- mgcv::gam(y ~ s(transform(x), k = 5) + s(g, bs = "re"),
    data = d, method = "REML")
  nd <- d[c(1, 30), ]
  # Supply an independently evaluated model frame to mgcv: its direct newdata
  # path cannot recover this local transformation in all calling environments.
  evaluated <- nd
  evaluated[["transform(x)"]] <- log(nd$x)
  attr(evaluated, "terms") <- attr(model.frame(model), "terms")
  reference <- fitted_joint_reference(model, evaluated, exclude = "s(g)")
  a <- fitted_values(model, data = nd, exclude = "s(g)", envir = environment(),
    interval = "simultaneous", seed = 44, n_sim = 300)
  expect_equal(a$.fitted, reference$fit)
  expect_equal(a$.se, reference$se)
  expect_equal(a$.lower_ci, unname(reference$lower))
  # Existing na.omit training behaviour keeps only retained observations.
  d$y[3] <- NA
  omitted <- mgcv::gam(y ~ s(x, k = 5), data = d, na.action = na.omit)
  expect_equal(nrow(fitted_values(omitted, interval = "simultaneous",
    seed = 44, n_sim = 100)), nrow(d) - 1L)
})
