test_that("supplied draw identities survive reordering and transformations", {
  m <- mgcv::gam(mpg ~ s(wt, k = 4) + s(hp, k = 4), data = mtcars)
  nd <- mtcars[1:3, ]
  for (chains in c(1L, 4L)) {
    b <- post_draws(m, n_draws = 40, chains = chains, seed = 9)
    meta <- attr(b, "sampling") %||% list(draw_ids = sampling_layout(40, 1),
      chains = NULL, coefficients = NULL)
    meta$draw_ids$.draw <- 101:140
    attr(b, "sampling") <- meta
    indices <- rev(seq_len(nrow(b)))
    reordered <- b[indices, , drop = FALSE]
    meta$draw_ids <- meta$draw_ids[indices, ]
    attr(reordered, "sampling") <- meta
    plain <- reordered
    attr(plain, "sampling") <- NULL

    for (fun in list(fitted_samples, smooth_samples, derivative_samples,
                     posterior_samples)) {
      args <- list(model = m, data = nd, method = "user", seed = 12)
      if (identical(fun, derivative_samples)) args$focal <- "wt"
      actual <- do.call(fun, c(args, list(draws = reordered)))
      expected <- do.call(fun, c(args, list(draws = plain)))
      expected$.draw <- meta$draw_ids$.draw[expected$.draw]
      key <- function(x) paste(x$.draw, x$.row, x[[".term"]] %||% "",
        x[[".parameter"]] %||% "", sep = ":")
      i <- match(key(expected), key(actual))
      expect_false(anyNA(i))
      value <- intersect(c(".fitted", ".response", ".derivative", ".value"), names(actual))
      expect_equal(actual[[value]][i], expected[[value]])
      expect_identical(actual$.draw[i], expected$.draw)
      expect_identical(attr(actual, "sampling"), meta)
      if (chains > 1L) {
        j <- match(actual$.draw, meta$draw_ids$.draw)
        expect_identical(actual$.chain, meta$draw_ids$.chain[j])
        expect_identical(actual$.iteration, meta$draw_ids$.iteration[j])
      }
      if (!identical(fun, posterior_samples)) {
        original <- do.call(fun, c(args, list(draws = b)))
        expect_equal(as_mcmc_draws(actual, "array"), as_mcmc_draws(original, "array"))
      }
    }
    selected <- post_draws(m, method = "user", draws = reordered, index = 2:4)
    expect_identical(attr(selected, "sampling"), meta)
    expect_equal(as_mcmc_draws(selected, "array"),
      as_mcmc_draws(post_draws(m, method = "user", draws = b, index = 2:4), "array"))
  }
})

test_that("GAMM component response draws infer their supplied count and chains", {
  models <- list(m_gamm)
  if (requireNamespace("gamm4", quietly = TRUE)) models <- c(models, list(gamm4_gaussian))
  for (model in models) {
    component <- if (is_gamm4(model)) gamm4_gam(model) else model$gam
    draws <- post_draws(component, n_draws = 12, chains = 3, seed = 9)
    expected <- posterior_samples(component, method = "user", draws = draws, seed = 12)
    actual <- posterior_samples(model, method = "user", draws = draws,
      conditioning = "gam_component", seed = 12)
    expect_equal(actual, expected)
    expect_identical(sort(unique(actual$.draw)), 1:12)
    expect_identical(sort(unique(actual$.chain)), 1:3)
    expect_error(posterior_samples(model, n_draws = 13, method = "user",
      draws = draws, conditioning = "gam_component"), "n_draws.*agree")
  }
})

test_that("distributional response draws retain supplied identities", {
  for (m in list(gfam_model, m_gaulss, m_multinom, m_mvn)) {
    b <- post_draws(m, n_draws = 12, chains = 3, seed = 9)
    meta <- attr(b, "sampling")
    meta$draw_ids$.draw <- 101:112
    indices <- rev(seq_len(nrow(b)))
    b <- b[indices, , drop = FALSE]
    meta$draw_ids <- meta$draw_ids[indices, ]
    attr(b, "sampling") <- meta
    plain <- b
    attr(plain, "sampling") <- NULL
    actual <- posterior_samples(m, method = "user", draws = b, seed = 12)
    expected <- posterior_samples(m, method = "user", draws = plain, seed = 12)
    expected$.draw <- meta$draw_ids$.draw[expected$.draw]
    key <- function(x) paste(x$.row, x$.draw, x[[".parameter"]] %||% "", sep = ":")
    i <- match(key(expected), key(actual))
    expect_false(anyNA(i))
    expect_equal(actual$.response[i], expected$.response)
    expect_identical(actual$.draw[i], expected$.draw)
    expect_identical(attr(actual, "sampling"), meta)
  }
})

test_that("streaming diagnostics reject obsolete sampling controls", {
  expect_error(posterior_predictive_plot(m_gam, n = 4, scalable = TRUE,
    freq = TRUE), "Argument 'freq' has been removed. Use frequentist", fixed = TRUE)
  expect_error(posterior_predictive_plot(m_gam, n = 4, scalable = TRUE,
    ncores = 2), "Argument 'ncores' has been removed", fixed = TRUE)
  m <- lm(mpg ~ wt, data = mtcars)
  expect_error(posterior_predictive_plot(m, n = 4, scalable = TRUE,
    freq = TRUE), "Argument 'freq' has been removed. Use frequentist", fixed = TRUE)
})

test_that("add helpers preserve supplied identities without remapping twice", {
  m <- mgcv::gam(mpg ~ s(wt, k = 4), data = mtcars)
  nd <- mtcars[1:3, ]
  draws <- post_draws(m, n_draws = 12, chains = 3, seed = 9)
  indices <- 12:1
  meta <- attr(draws, "sampling")
  meta$draw_ids$.draw <- 101:112
  meta$draw_ids <- meta$draw_ids[indices, ]
  draws <- draws[indices, , drop = FALSE]
  attr(draws, "sampling") <- meta
  helpers <- list(add_fitted_samples, add_smooth_samples, add_posterior_samples)
  functions <- list(fitted_samples, smooth_samples, posterior_samples)
  for (i in seq_along(helpers)) {
    added <- helpers[[i]](nd, m, method = "user", draws = draws, seed = 12)
    expected <- functions[[i]](m, data = nd, method = "user", draws = draws, seed = 12)
    expect_equal(as_mcmc_draws(added, "array"), as_mcmc_draws(expected, "array"))
  }
})

test_that("the new evaluator rejects dependent GAMM components", {
  m <- mgcv::gamm(mpg ~ s(wt, k = 4), data = mtcars,
    correlation = nlme::corAR1(form = ~ 1), method = "REML")
  expect_error(sampling_target(m$gam), "does not support GAMM components")
  expect_error(post_draws(m$gam, n_draws = 4, method = "mh", mh_backend = "gratia",
    burnin = 1), "does not support GAMM components")
  expect_identical(dim(post_draws(m$gam, n_draws = 4, seed = 9)), c(4L, 4L))
})

test_that("coefficient diagnostics support selection by supplied column names", {
  skip_if_not_installed("posterior")
  m <- mgcv::gam(mpg ~ s(wt, k = 4), data = mtcars)
  b <- post_draws(m, n_draws = 40, chains = 4, seed = 9)
  expected <- names(coef(m))[2:4]
  for (labels in list(names(coef(m)), paste0("coefficient", 1:4))) {
    colnames(b) <- labels
    actual <- post_draws(m, method = "user", draws = b, index = labels[2:4],
      diagnostics = TRUE)
    expect_equal(as.vector(actual), as.vector(b[, 2:4]))
    expect_identical(colnames(actual), expected)
    expect_identical(sampling_diagnostics(actual)$coefficients$variable, expected)
  }
})
