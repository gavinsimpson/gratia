# RNG expectations are assembled from the distribution definitions, independent
# of the grouped dispatcher and its component-parameter extraction.
test_that("grouped response draws use each component's parameters and input scale", {
  families <- list(gaussian(), poisson(), binomial(), Gamma(),
    mgcv::nb(theta = 2.3), mgcv::tw(theta = 1.4), mgcv::betar(theta = 12),
    mgcv::scat(theta = c(5, 1.7)), mgcv::cnorm(theta = 1.9),
    mgcv::clog(theta = 1.3), mgcv::cpois(), mgcv::Tweedie(p = 1.6))
  scales <- c(2.1, 1, 1, 0.3, 1, 0.7, 1, 1, 1, 1, 1, 0.4)
  components <- Map(function(family, scale) list(family = family, scale = scale),
    families, scales)
  # Interleaved rows make accidental contiguous-group dispatch visible.
  index <- rep(seq_along(families), 3L)
  eta <- rep(c(0.3, 0.7, 1.1), each = length(families))
  wt <- rep(c(1, 2, 4), each = length(families))
  context <- list(components = components, index = index)
  generators <- lapply(families, function(f) fix_family_rd(f)$rd)
  expected <- withr::with_seed(713, {
    out <- numeric(length(index))
    for (i in seq_along(families)) {
      rows <- which(index == i)
      mu <- families[[i]]$linkinv(eta[rows])
      w <- wt[rows]
      out[rows] <- switch(i,
        rnorm(length(rows), mu, sqrt(scales[i] / w)),
        rpois(length(rows), mu),
        rbinom(length(rows), w, mu) / w,
        rgamma(length(rows), shape = 1 / scales[i], scale = mu * scales[i]),
        rnbinom(length(rows), size = 2.3, mu = mu),
        mgcv::rTweedie(mu, p = 1.4, phi = scales[i]),
        rbeta(length(rows), 12 * mu, 12 * (1 - mu)),
        rt(length(rows), df = 5) * 1.7 + mu,
        rnorm(length(rows), mu, 1.9 / sqrt(w)),
        rlogis(length(rows), mu, 1.3 / sqrt(w)),
        rpois(length(rows), mu),
        mgcv::rTweedie(mu, p = 1.6, phi = scales[i])
      )
    }
    out
  })
  expect_equal(withr::with_seed(713,
    gfam_response_draw(eta, wt, context, generators)), expected)
  missing <- c(2L, 7L, 19L)
  eta[missing[1]] <- NA_real_
  wt[missing[2]] <- NA_real_
  context$index[missing[3]] <- NA_integer_
  actual <- withr::with_seed(713,
    gfam_response_draw(eta, wt, context, generators))
  expect_true(all(is.na(actual[missing])))
  keep <- setdiff(seq_along(eta), missing)
  context$index <- context$index[keep]
  expected <- withr::with_seed(713,
    gfam_response_draw(eta[keep], wt[keep], context, generators))
  expect_equal(actual[keep], expected)
})

test_that("grouped latent families generate categories and hurdle counts", {
  families <- list(mgcv::ocat(theta = c(0.8, 1.2)),
    mgcv::ziP(theta = c(-0.7, 0.2), b = 0.3))
  index <- rep(1:2, 20)
  eta <- rep(seq(-2, 2, length.out = 20), each = 2)
  context <- list(index = index,
    components = lapply(families, function(f) list(family = f, scale = 1)))
  generators <- lapply(families, function(f) f$rd)
  expected <- withr::with_seed(61, {
    y <- numeric(length(eta))
    rows <- which(index == 1)
    latent <- eta[rows] + qlogis(runif(length(rows)))
    y[rows] <- findInterval(latent, c(-Inf, -1, -0.2, 1, Inf))
    rows <- which(index == 2)
    gamma <- eta[rows]
    lambda <- exp(gamma)
    p <- -expm1(-exp(-0.7 + (0.3 + exp(0.2)) * gamma))
    present <- p > runif(length(rows))
    z <- numeric(length(rows))
    z[present] <- qpois(runif(sum(present), dpois(0, lambda[present]),
      1 - .Machine$double.eps * 10), lambda[present])
    y[rows] <- z
    y
  })
  actual <- withr::with_seed(61,
    gfam_response_draw(eta, rep(1, length(eta)), context, generators))
  expect_equal(actual, expected)
  expect_true(all(actual[index == 1] %in% 1:4))
  expect_true(all(actual[index == 2] >= 0 & actual[index == 2] %% 1 == 0))
})

test_that("grouped simulation validates family-specific weight contracts", {
  context <- list(index = c(1L, 2L), components = list(
    list(family = gaussian(), scale = 2), list(family = binomial(), scale = 1)))
  generators <- lapply(context$components, function(x) fix_family_rd(x$family)$rd)
  expect_error(gfam_response_draw(c(0, 0), c(1, 1.5), context, generators),
    "integer trial counts")
  expect_error(gfam_response_draw(c(0, 0), c(0, 1), context, generators),
    "precision weights must be positive")
  for (wt in list(c(-1, 1), c(Inf, 1))) {
    expect_error(gfam_response_draw(c(0, 0), wt, context, generators),
      "finite and non-negative")
  }
  expect_equal(gfam_response_draw(c(NA, 0), c(1, 0), context, generators),
    c(NA_real_, 0))
  withr::local_seed(21)
  state <- .Random.seed
  expect_equal(gfam_response_draw(c(NA, NA), c(1, 1), context, generators),
    c(NA_real_, NA_real_))
  expect_identical(.Random.seed, state)
})

# The shared integration fixture has groups binomial/Gaussian/Poisson. Its one
# free dispersion is the first packed theta value; do not call gfam_context() in
# this oracle, so wrong component-scale extraction cannot agree with itself.
manual_gfam_eta <- function(model, data) {
  # Native non-discrete design-matrix prediction bypasses gfam response hooks.
  # Compute eta independently of the grouped link-prediction adapter.
  xp <- mgcv::predict.gam(model, newdata = data, type = "lpmatrix",
    newdata.guaranteed = TRUE)
  eta <- as.numeric(xp %*% coef(model))
  offset <- attr(xp, "model.offset")
  if (is.null(offset)) eta else eta + offset
}

manual_gfam_simulation <- function(model, data, index, weights, nsim, seed) {
  eta <- manual_gfam_eta(model, data)
  phi <- exp(model$family$getTheta()[1])
  withr::with_seed(seed, {
    sims <- replicate(nsim, {
      out <- numeric(length(index))
      for (i in 1:3) {
        rows <- which(index == i)
        out[rows] <- switch(i,
          rbinom(length(rows), weights[rows], plogis(eta[rows])) /
            (weights[rows] + (weights[rows] == 0)),
          rnorm(length(rows), eta[rows], sqrt(phi / weights[rows])),
          rpois(length(rows), exp(eta[rows])))
      }
      out
    }, simplify = FALSE)
    as.data.frame(sims, col.names = paste0("sim_", seq_len(nsim)))
  })
}

test_that("grouped gam and bam simulations match independent component draws", {
  for (model in list(gfam_model, gfam_bam)) {
    data <- gfam_data[c(24, 3, 5, 9, 1, 24, 2), ]
    index <- data$family_id
    weights <- c(2, 4, 5, 2, 1, 2, 3)
    expected <- manual_gfam_simulation(model, data, index, weights, 4, 37)
    actual <- simulate(model, nsim = 4, seed = 37, data = data,
      weights = weights)
    expect_s3_class(actual, "simulate_gratia")
    expect_s3_class(actual, "data.frame")
    expect_equal(as.matrix(actual), as.matrix(expected), ignore_attr = TRUE)
    expect_named(actual, paste0("sim_", 1:4))
    without_response <- data[, setdiff(names(data), c("y", "family_id")), drop = FALSE]
    explicit <- simulate(model, nsim = 4, seed = 37, data = without_response,
      weights = weights, family_index = index)
    expect_equal(explicit, actual)
    expect_equal(simulate(model, seed = 37, data = data, weights = 2),
      simulate(model, seed = 37, data = data, weights = rep(2, nrow(data))))
    expect_error(simulate(model, data = data, weights = 1:2),
      "one value per prediction row")
    expect_error(simulate(model, data = without_response), "family")
    expect_error(simulate(model, data = data, family_index = rep(1, nrow(data))),
      "agree|conflict")
  }
})

test_that("grouped training and matrix response membership remain aligned", {
  for (model in list(gfam_model, gfam_bam)) {
    expected <- manual_gfam_simulation(model, gfam_data, gfam_data$family_id,
      model$prior.weights, 2, 11)
    actual <- simulate(model, nsim = 2, seed = 11)
    expect_equal(as.matrix(actual), as.matrix(expected), ignore_attr = TRUE)
    expected <- manual_gfam_simulation(model, gfam_data, gfam_data$family_id,
      rep(3, nrow(gfam_data)), 2, 11)
    expect_equal(as.matrix(simulate(model, nsim = 2, seed = 11, weights = 3)),
      as.matrix(expected), ignore_attr = TRUE)
  }
  d <- gfam_matrix_data[c(10, 8, 10, 3, 1), ]
  actual <- simulate(gfam_matrix_model, nsim = 2, seed = 6, data = d)
  selector <- d
  response <- names(gfam_matrix_model$model)[1]
  selector[[response]] <- d[[response]][, 2]
  expect_equal(simulate(gfam_matrix_model, nsim = 2, seed = 6, data = selector), actual)
  expect_equal(simulate(gfam_matrix_model, nsim = 2, seed = 6,
    data = d[, setdiff(names(d), response), drop = FALSE],
    family_index = d[[response]][, 2]), actual)
})

test_that("grouped simulation restores subset exclusions and prediction NAs once", {
  model <- gfam_missing_model
  omitted <- as.integer(model$na.action)
  retained <- setdiff(seq_len(165), omitted)
  data <- gfam_missing_data[retained, ]
  expected <- manual_gfam_simulation(model, data, data$family_id,
    model$prior.weights, 2, 48)
  actual <- simulate(model, nsim = 2, seed = 48)
  expect_equal(nrow(actual), 165L)
  expect_true(all(is.na(as.matrix(actual[omitted, ]))))
  expect_equal(as.matrix(actual[retained, ]), as.matrix(expected), ignore_attr = TRUE)
  model_omit <- model
  class(model_omit$na.action) <- "omit"
  expect_equal(as.matrix(simulate(model_omit, nsim = 2, seed = 48)),
    as.matrix(expected), ignore_attr = TRUE)

  d <- gfam_data[1:8, ]
  d$x[2] <- NA_real_
  d$family_id[5] <- NA_integer_
  d$y[4] <- NA_real_ # Simulation does not require an observed response.
  weights <- rep(1, nrow(d)); weights[7] <- NA_real_
  actual <- simulate(gfam_model, nsim = 3, seed = 8, data = d, weights = weights)
  expect_equal(dim(actual), c(8L, 3L))
  expect_true(all(is.na(as.matrix(actual[c(2, 5, 7), ]))))
  keep <- c(1, 3, 4, 6, 8)
  expected <- manual_gfam_simulation(gfam_model, d[keep, ], d$family_id[keep],
    weights[keep], 3, 8)
  expect_equal(as.matrix(actual[keep, ]), as.matrix(expected), ignore_attr = TRUE)
  omit <- simulate(gfam_model, nsim = 3, seed = 8, data = d, weights = weights,
    na.action = na.omit)
  expect_equal(nrow(omit), 6L) # Weight NAs mask; predictor/index NAs obey na.action.
  expect_equal(as.matrix(omit), as.matrix(actual[-c(2, 5), ]), ignore_attr = TRUE)
  expect_error(simulate(gfam_model, data = d, na.action = na.fail), "missing")
  d$x[] <- NA_real_
  withr::local_seed(77)
  state <- .Random.seed
  missing <- simulate(gfam_model, nsim = 2, data = d)
  expect_true(all(is.na(as.matrix(missing))))
  expect_identical(.Random.seed, state)
})

test_that("grouped simulations preserve seeded and unseeded RNG contracts", {
  withr::local_seed(191)
  d <- gfam_data[1:7, ]
  state <- .Random.seed
  a <- simulate(gfam_model, nsim = 2, seed = 92, data = d)
  expect_identical(.Random.seed, state)
  expect_identical(simulate(gfam_model, nsim = 2, seed = 92, data = d), a)
  expect_identical(unname(attr(a, "seed")),
    unname(structure(92, kind = as.list(RNGkind()))))
  before_family <- serialize(gfam_model$family$getfl(), NULL)
  before_theta <- gfam_model$family$getTheta()
  simulate(gfam_model, nsim = 2, data = d)
  expect_false(identical(.Random.seed, state))
  expect_identical(serialize(gfam_model$family$getfl(), NULL), before_family)
  expect_identical(gfam_model$family$getTheta(), before_theta)
  rm(".Random.seed", envir = .GlobalEnv)
  expect_silent(simulate(gfam_model, seed = 3, data = d))
  expect_true(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("estimated component parameters and dispersions are not pooled", {
  model <- gfam_extended_model
  d <- gfam_extended_data
  # Gaussian scale precedes the second NB and Tweedie parameters. In mgcv 1.9-4
  # the native component closures can hold incorrect theta after this scale;
  # the aggregate vector, not component getters, is the numerical oracle.
  theta <- model$family$getTheta()
  expect_length(theta, 5L)
  nd <- d[c(2, 1, 3, 20, 14, 9, 1, 4), ]
  eta <- manual_gfam_eta(model, nd)
  expected <- withr::with_seed(42, {
    y <- numeric(nrow(nd))
    i <- which(nd$family_id == 1)
    y[i] <- rnbinom(length(i), size = exp(theta[1]), mu = exp(eta[i]))
    i <- which(nd$family_id == 2)
    y[i] <- rnorm(length(i), eta[i], sqrt(exp(theta[2])))
    i <- which(nd$family_id == 3)
    y[i] <- rnbinom(length(i), size = exp(theta[3]), mu = exp(eta[i]))
    i <- which(nd$family_id == 4)
    power <- 1.01 + (1.99 - 1.01) * plogis(theta[4])
    y[i] <- mgcv::rTweedie(exp(eta[i]), p = power, phi = exp(theta[5]))
    y
  })
  expect_equal(simulate(model, seed = 42, data = nd)$sim_1, expected)

  model <- gfam_gaussian_model
  d <- gfam_gaussian_data
  theta <- model$family$getTheta()
  expect_length(theta, 2L)
  expect_gt(abs(theta[1] - theta[2]), 0.5)
  eta <- manual_gfam_eta(model, d)
  expected <- withr::with_seed(19, {
    y <- numeric(nrow(d))
    for (j in 1:2) {
      i <- which(d$family_id == j)
      y[i] <- rnorm(length(i), eta[i], sqrt(exp(theta[j]) / model$prior.weights[i]))
    }
    y
  })
  expect_equal(simulate(model, seed = 19)$sim_1, expected)
})

test_that("fitted ordinal grouped models produce categories from latent predictors", {
  model <- gfam_ordinal_model
  d <- gfam_ordinal_data
  nd <- d[c(8, 5, 10, 1, 9), ]
  eta <- manual_gfam_eta(model, nd)
  theta <- model$family$getTheta()
  expected <- withr::with_seed(27, {
    y <- numeric(nrow(nd))
    i <- which(nd$family_id == 1)
    latent <- eta[i] + qlogis(runif(length(i)))
    cuts <- c(-Inf, -1, -1 + cumsum(exp(theta)), Inf)
    y[i] <- findInterval(latent, cuts)
    i <- which(nd$family_id == 2)
    y[i] <- rpois(length(i), exp(eta[i]))
    y
  })
  expect_equal(simulate(model, seed = 27, data = nd)$sim_1, expected)
})

test_that("grouped simulation evaluates transformed predictors and offsets", {
  withr::local_seed(46)
  d <- gfam_data
  d$x <- d$x + 1
  model <- mgcv::gam(cbind(y, family_id) ~ s(log(x), k = 5) + offset(log(exp(off))),
    family = mgcv::gfam(list(binomial(), gaussian(), poisson())),
    weights = weight, data = d, method = "REML")
  nd <- d[c(12, 3, 1, 8), ]
  nd$off <- nd$off + 0.3
  expected <- manual_gfam_simulation(model, nd, nd$family_id, rep(2, nrow(nd)), 2, 81)
  expect_equal(as.matrix(simulate(model, seed = 81, nsim = 2, data = nd, weights = 2)),
    as.matrix(expected), ignore_attr = TRUE)
})

test_that("unsupported grouped components identify the failing family", {
  model <- gfam_model
  family <- model$family
  families <- family$getfl()
  bad <- families[[2]]
  bad$family <- "unsupported response family"
  bad$rd <- NULL
  families[[2]] <- bad
  # Change a newly constructed family, never the fixture's closure environment.
  model$family <- mgcv::gfam(families)
  model$family$putTheta(family$getTheta())
  model$family$putfl(families)
  expect_error(simulate(model, data = gfam_data[2, , drop = FALSE]),
    "component 2 <unsupported response family>")
  expect_error(simulate(m_gam, family_index = 1), "only available for grouped")
})

test_that("grouped adapters respect component links and inverse-Gaussian RNGs", {
  families <- list(gaussian(link = "log"), poisson(link = "sqrt"),
    binomial(link = "cloglog"), inverse.gaussian())
  index <- rep(1:4, 3)
  eta <- rep(c(0.2, 0.5, 1), each = 4)
  weights <- rep(2, length(index))
  context <- list(index = index, components = Map(
    function(f, scale) list(family = f, scale = scale), families, c(0.3, 1, 1, 0.5)))
  generators <- lapply(families, function(f) mgcv::fix.family.rd(f)$rd)
  expected <- withr::with_seed(11, {
    out <- numeric(length(index))
    i <- which(index == 1); out[i] <- rnorm(length(i), exp(eta[i]), sqrt(0.3 / 2))
    i <- which(index == 2); out[i] <- rpois(length(i), eta[i]^2)
    i <- which(index == 3)
    out[i] <- rbinom(length(i), 2, -expm1(-exp(eta[i]))) / 2
    i <- which(index == 4)
    out[i] <- mgcv::fix.family.rd(inverse.gaussian())$rd(1 / sqrt(eta[i]),
      weights[i], scale = 0.5)
    out
  })
  actual <- withr::with_seed(11,
    gfam_response_draw(eta, weights, context, generators))
  expect_equal(actual, expected)
  expect_true(all(actual[index == 4] > 0))
})

test_that("grouped simulation preserves single, empty and all-missing predictions", {
  d <- gfam_data[2, , drop = FALSE]
  expect_equal(dim(simulate(gfam_model, data = d)), c(1L, 1L))
  empty <- simulate(gfam_model, nsim = 2, data = d[FALSE, , drop = FALSE])
  expect_equal(dim(empty), c(0L, 2L))
  missing <- d[c(1, 1), ]
  missing$family_id <- NA_integer_
  withr::local_seed(14)
  state <- .Random.seed
  actual <- simulate(gfam_model, nsim = 2, data = missing)
  expect_equal(dim(actual), c(2L, 2L))
  expect_true(all(is.na(as.matrix(actual))))
  expect_identical(.Random.seed, state)
  for (selector in list(0, 4, 1.5, Inf, "1", c(1, 2))) {
    expect_error(simulate(gfam_model, data = d, family_index = selector),
      "integer family-list position")
  }
})


test_that("grouped custom RNGs must return one scalar response per row", {
  context <- list(index = c(1L, 1L, 1L), components = list(
    list(family = gaussian(), scale = 1)))
  for (value in list(1, 1:2, 1:4, matrix(1:3, ncol = 1), c("a", "b", "c"),
      list(1, 2, 3))) {
    generator <- function(mu, wt, scale) value
    expect_error(gfam_response_draw(1:3, rep(1, 3), context, list(generator)),
      "component 1 <gaussian> must generate one numeric response")
  }
  generator <- function(mu, wt, scale) mu > 1
  expect_equal(gfam_response_draw(1:3, rep(1, 3), context, list(generator)),
    c(0, 1, 1))
})
