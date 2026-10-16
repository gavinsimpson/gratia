skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_sampling_models <- local({
  s <- mgcv::s
  d <- withr::with_seed(718, {
    d <- data.frame(x = runif(160, 0.05, 0.95), z = runif(160, 0.05, 0.95))
    d$y <- 2 + exp(d$x) + 0.2 * d$z + rnorm(160, sd = 0.15)
    d
  })
  bases <- c("mpi", "miso", "mifo", "ipo", "dpo", "lipl", "tesmi1", "tismi", "tedmi")
  fits <- lapply(bases, function(bs) {
    f <- if (bs == "lipl") {
      y ~ s(x, bs = "lipl", k = 12, xt = list(xc = 0.5))
    } else if (startsWith(bs, "te") || startsWith(bs, "ti")) {
      as.formula(sprintf("y ~ s(x, z, bs = '%s', k = c(5, 5))", bs))
    } else as.formula(sprintf("y ~ s(x, bs = '%s', k = 6)", bs))
    scam::scam(f, data = d)
  })
  names(fits) <- bases
  fits$not_exp <- scam::scam(y ~ s(x, bs = "mpi", k = 6), data = d,
    not.exp = TRUE, control = list(b.notexp = 2, threshold.notexp = 10))
  list(models = fits, data = d)
})

test_that("SCAM Gaussian draws transform the unrestricted fitting approximation", {
  for (model in scam_sampling_models$models[c("mpi", "not_exp")]) {
    for (frequentist in c(FALSE, TRUE)) {
      raw <- post_draws(model, n_draws = 24, seed = 413, frequentist = frequentist,
        parametrized = FALSE, chains = 2)
      constrained <- post_draws(model, n_draws = 24, seed = 413,
        frequentist = frequentist, chains = 2)
      expected <- raw
      j <- model$p.ident
      expected[, j] <- if (isTRUE(model$not.exp)) {
        getFromNamespace("notExp", "scam")(raw[, j],
          b = model$control$b.notexp, threshold = model$control$threshold.notexp)
      } else exp(raw[, j])
      expect_equal(as.numeric(constrained), as.numeric(expected))
      expect_true(all(constrained[, j] >= 0))
      expect_identical(attr(constrained, "sampling")$draw_ids,
        attr(raw, "sampling")$draw_ids)
      expect_identical(constrained, post_draws(model, n_draws = 24, seed = 413,
        frequentist = frequentist, chains = 2))
      expect_equal(as.numeric(generate_draws(model, n_draws = 24, seed = 413,
        frequentist = frequentist, chains = 2)), as.numeric(constrained))
    }
    nd <- data.frame(x = seq(0.05, 0.95, length.out = 60))
    draws <- post_draws(model, n_draws = 20, seed = 42)
    values <- smooth_samples(model, data = nd, method = "user", draws = draws)
    for (i in unique(values$.draw)) {
      expect_true(all(diff(values$.value[values$.draw == i]) >= -1e-12))
    }
    selected <- gaussian_draws(model, n_draws = 2, seed = 1,
      index = names(coef(model))[2:3])
    expect_identical(dim(selected), c(2L, 2L))
  }
})

test_that("SCAM user draws explicitly validate transformed coefficient order", {
  model <- scam_sampling_models$models$mpi
  draws <- post_draws(model, n_draws = 6, chains = 2, seed = 317)
  expect_identical(user_draws(model, draws = draws), draws)
  expect_equal(user_draws(model, draws = draws, index = 2:3),
    structure(draws[, 2:3, drop = FALSE], sampling = attr(draws, "sampling")))
  expect_error(user_draws(model, draws = draws[, -1, drop = FALSE]), "names and order")
  expect_error(user_draws(model, draws = draws[, rev(seq_len(ncol(draws))), drop = FALSE]),
    "names and order")
  raw <- post_draws(model, n_draws = 3, parametrized = FALSE, seed = 1)
  expect_error(user_draws(model, draws = raw), "unrestricted fitting coordinates")
  bad <- draws; bad[, which(model$p.ident)[1L]] <- -1
  expect_error(user_draws(model, draws = bad), "negative coefficients")
  bad <- draws; bad[1L] <- Inf
  expect_error(user_draws(model, draws = bad), "finite numeric matrix")
  expect_error(post_draws(model, n_draws = 3, method = "mh"), "not implemented for SCAM")
})

test_that("SCAM smooth and fitted draws use the same mapped coefficient columns", {
  nd <- scam_sampling_models$data[1:9, ]
  for (model in scam_sampling_models$models) {
    draws <- matrix(rep(coef(model), 3), nrow = 3, byrow = TRUE)
    draws[2, ] <- draws[2, ] * 0.9
    draws[3, ] <- draws[3, ] * 1.1
    sm <- model$smooth[[1L]]
    idx <- smooth_coef_indices(sm)
    X <- scam_smooth_matrix(sm, mgcv::PredictMat(sm, nd))
    expected <- X %*% t(draws[, idx, drop = FALSE])
    values <- smooth_samples(model, data = nd, method = "user", draws = draws)
    for (i in 1:3) expect_equal(values$.value[values$.draw == i],
      as.numeric(expected[, i]), tolerance = 1e-12)
    fitted <- fitted_samples(model, data = nd, method = "user", draws = draws,
      scale = "linear_predictor")
    XX <- predict_model(model, newdata = nd, type = "lpmatrix")
    for (i in 1:3) expect_equal(fitted$.fitted[fitted$.draw == i],
      as.numeric(XX %*% draws[i, ]), tolerance = 1e-12)
  }
})

test_that("SCAM covariance sampling handles zero directions and rejects invalid matrices", {
  for (method in c("mvnfast", "mgcv")) {
    withr::local_seed(14)
    value <- scam_gaussian_matrix(30, c(1, 2, 3), diag(c(1, 0, 2)), method, 1L)
    expect_identical(dim(value), c(30L, 3L))
    expect_equal(value[, 2], rep(2, 30))
    zero <- scam_gaussian_matrix(3, c(1, 2), matrix(0, 2, 2), method, 1L)
    expect_equal(zero, matrix(c(1, 2), 3, 2, byrow = TRUE))
    expect_true(all(is.finite(scam_gaussian_matrix(3, c(1, 2),
      diag(c(1, -1e-15)), method, 1L))))
    expect_error(scam_gaussian_matrix(3, c(1, 2), diag(c(1, -0.1)), method, 1L),
      "materially negative")
    expect_error(scam_gaussian_matrix(3, c(1, 2), matrix(c(1, 1, 0, 1), 2), method, 1L),
      "not symmetric")
  }
  model <- scam_sampling_models$models$mpi
  model$Vp[,] <- 0
  expect_equal(as.numeric(post_draws(model, n_draws = 2, seed = 1)),
    as.numeric(matrix(rep(coef(model), each = 2), nrow = 2)))
})

test_that("SCAM Gaussian draws report an unusable exponential approximation", {
  model <- scam_sampling_models$models$mpi
  model$Vp <- diag(rep(1e8, length(coef(model))))
  expect_error(post_draws(model, n_draws = 40, seed = 192),
    "transformation overflowed")
  expect_true(all(is.finite(post_draws(model, n_draws = 40, seed = 192,
    parametrized = FALSE))))
  model <- scam_sampling_models$models$not_exp
  model$Vp <- diag(rep(1e8, length(coef(model))))
  expect_true(all(is.finite(post_draws(model, n_draws = 40, seed = 192))))
})

test_that("SCAM fitting-coordinate draws do not depend on vcov method registration", {
  model <- scam_sampling_models$models$mpi
  model$Vp[,] <- 0
  model$Ve[,] <- 0
  old <- getS3method("vcov", "scam")
  withr::defer(registerS3method("vcov", "scam", old, envir = asNamespace("stats")))
  registerS3method("vcov", "scam", getFromNamespace("vcov.scam", "scam"),
    envir = asNamespace("stats"))
  for (frequentist in c(FALSE, TRUE)) {
    raw <- post_draws(model, n_draws = 3, seed = 13, parametrized = FALSE,
      frequentist = frequentist)
    expect_equal(as.numeric(raw), rep(as.numeric(model$coefficients), each = 3))
    transformed <- post_draws(model, n_draws = 3, seed = 13,
      frequentist = frequentist)
    expect_equal(as.numeric(transformed), rep(as.numeric(coef(model)), each = 3))
  }
})

test_that("SCAM sampling parameterisation and covariance choices require logical flags", {
  model <- scam_sampling_models$models$mpi
  for (value in list(NA, 1, "TRUE", NULL, c(TRUE, FALSE))) {
    expect_error(gaussian_draws(model, n_draws = 2, parametrized = value),
      "`parametrized` must be a single non-missing logical value", fixed = TRUE)
    expect_error(gaussian_draws(model, n_draws = 2, frequentist = value),
      "`frequentist` must be a single non-missing logical value", fixed = TRUE)
    expect_error(post_draws(model, n_draws = 2, parametrized = value),
      "`parametrized` must be a single non-missing logical value", fixed = TRUE)
  }
})
