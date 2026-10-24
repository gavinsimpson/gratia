test_that("obsolete sampling arguments fail before matching or evaluation", {
  m <- sampling_interface_model
  functions <- list(
    posterior_samples,
    fitted_samples,
    smooth_samples,
    derivative_samples,
    predicted_samples,
    post_draws,
    generate_draws,
    gaussian_draws,
    mh_draws,
    user_draws
  )
  old <- c(
    n = "n_draws",
    n_sim = "n_draws",
    n_vals = "n_grid",
    freq = "frequentist",
    newdata = "data",
    ncores = "n_cores",
    object = "model"
  )
  for (fun in functions) {
    for (arg in names(old)) {
      args <- c(list(m), setNames(list(10), arg))
      expected <- if (identical(fun, derivative_samples) && arg == "n") {
        "n_grid"
      } else {
        old[[arg]]
      }
      expect_error(
        do.call(fun, args),
        paste0("Argument '", arg, "' has been removed. Use ", expected),
        fixed = TRUE
      )
    }
    expect_error(fun(m, 10), "Sampling controls must be named", fixed = TRUE)
    expect_error(fun(m, n_d = 10), "not supported|Supply 'n_draws'")
  }
  for (fun in list(
    add_fitted_samples,
    add_smooth_samples,
    add_posterior_samples,
    add_predicted_samples
  )) {
    expect_error(
      fun(mtcars, m, n = 10),
      "Argument 'n' has been removed",
      fixed = TRUE
    )
    expect_error(
      fun(mtcars, m, 10),
      "Sampling controls must be named",
      fixed = TRUE
    )
  }
  expect_error(
    derivative_samples(object = stop("must not evaluate"), n_sim = 10),
    "has been removed"
  )
})

test_that("draw counts, grid resolution and model names are consistent", {
  m <- sampling_interface_model
  for (fun in list(
    fitted_samples,
    posterior_samples,
    smooth_samples,
    predicted_samples,
    post_draws,
    generate_draws,
    gaussian_draws,
    mh_draws
  )) {
    expect_error(fun(m), "Supply 'n_draws'", fixed = TRUE)
    expect_error(fun(m, n_draws = 0), "positive integer")
  }
  for (fun in list(smooth_samples, derivative_samples)) {
    args <- list(model = m, n_draws = 12, chains = 3, n_grid = 7, seed = 9)
    if (identical(fun, derivative_samples)) {
      args$focal <- "wt"
    }
    out <- do.call(fun, args)
    expect_equal(nrow(out), 12 * 7)
    expect_identical(sort(unique(out$.draw)), 1:12)
    expect_identical(sort(unique(out$.iteration)), 1:4)
    expect_error(
      do.call(fun, replace(args, "n_grid", list(0))),
      "n_grid.*positive integer"
    )
    args$data <- data.frame(wt = c(2, 3, 4))
    expect_equal(nrow(do.call(fun, args)), 12 * 3)
  }
  for (cls in c("gam", "scam")) {
    signatures <- lapply(
      c(
        "posterior_samples",
        "fitted_samples",
        "smooth_samples",
        "derivative_samples"
      ),
      function(fun) names(formals(getS3method(fun, cls)))
    )
    shared <- c(
      "model",
      "data",
      "n_draws",
      "chains",
      "seed",
      "method",
      "n_cores",
      "n_threads",
      "burnin",
      "thin",
      "t_df",
      "rw_scale",
      "frequentist",
      "unconditional",
      "draws",
      "diagnostics",
      "mh_backend",
      "mvn_method",
      "block_size",
      "dense_limit"
    )
    expect_true(all(vapply(
      signatures,
      function(x) all(shared %in% x),
      logical(1)
    )))
  }
})

test_that("supplied draws determine counts and retain coefficient diagnostics", {
  skip_if_not_installed("posterior")
  m <- sampling_interface_model
  b <- post_draws(m, n_draws = 40, chains = 4, seed = 9, diagnostics = TRUE)
  functions <- list(
    fitted_samples,
    posterior_samples,
    smooth_samples,
    derivative_samples,
    post_draws,
    generate_draws,
    user_draws
  )
  for (fun in functions) {
    args <- list(model = m, draws = b)
    if (!identical(fun, user_draws)) {
      args$method <- "user"
    }
    if (identical(fun, derivative_samples)) {
      args$focal <- "wt"
    }
    out <- do.call(fun, args)
    expect_identical(sampling_diagnostics(out), sampling_diagnostics(b))
    expect_identical(dim(as_mcmc_draws(out, "array"))[1:2], c(10L, 4L))
    expect_error(do.call(fun, c(args, list(n_draws = 39))), "n_draws.*agree")
    expect_error(do.call(fun, c(args, list(chains = 2))), "chains.*agree")
  }
  plain <- unclass(b)
  attr(plain, "sampling") <- NULL
  expect_error(
    fitted_samples(m, method = "user", draws = plain, chains = 4),
    "chain mapping"
  )
})

test_that("explicit inactive sampler controls and unsupported capabilities fail", {
  m <- sampling_interface_model
  for (fun in list(
    posterior_samples,
    fitted_samples,
    smooth_samples,
    derivative_samples,
    post_draws,
    generate_draws
  )) {
    expect_error(fun(m, n_draws = 8, burnin = 0), "burnin.*not used")
    expect_error(fun(m, n_draws = 8, n_cores = 2), "Use n_threads")
    expect_error(
      fun(m, n_draws = 8, method = "mh", unconditional = FALSE),
      "unconditional.*not used"
    )
    expect_error(
      fun(m, n_draws = 8, method = "mh", n_threads = 1),
      "n_threads.*not used"
    )
    expect_error(
      fun(m, n_draws = 8, method = "mh", block_size = 10),
      "require mh_backend"
    )
  }
  expect_error(
    fitted_samples(m, n_draws = 8, buring = 0),
    "buring.*not supported"
  )
  expect_error(
    predicted_samples(m, n_draws = 8, chains = 2),
    "fitted parameters"
  )
  expect_error(
    predicted_samples(m, n_draws = 8, diagnostics = TRUE),
    "fitted parameters"
  )
  expect_equal(
    length(unique(predicted_samples(m, n_draws = 8, seed = 9)$.draw)),
    8
  )
  lm <- m_lm
  expect_error(posterior_samples(lm, n_draws = 8, chains = 2), "not supported")
  expect_error(
    posterior_samples(lm, n_draws = 8, method = "mh"),
    "only method = 'gaussian'"
  )
})


test_that("direct samplers share seed and chain semantics", {
  m <- sampling_interface_model
  withr::local_seed(521)
  before <- .Random.seed
  for (fun in list(gaussian_draws, mh_draws)) {
    args <- list(model = m, n_draws = 12, chains = 3, seed = 91)
    if (identical(fun, mh_draws)) {
      args$burnin <- 10
    }
    a <- do.call(fun, args)
    expect_identical(as.vector(a), as.vector(do.call(fun, args)))
    expect_identical(.Random.seed, before)
    expect_identical(dim(as_mcmc_draws(a, "array"))[1:2], c(4L, 3L))
  }
  expect_error(
    fitted_samples(m, n_draws = 8, mvn_method = "mgcv", n_threads = 1),
    "requires mvn_method"
  )
  b <- post_draws(m, n_draws = 12, chains = 3, seed = 91)
  attr(b, "sampling")$draw_ids$.iteration[2] <- 1L
  expect_error(
    fitted_samples(m, method = "user", draws = b),
    "Invalid chain mapping"
  )
})


test_that("GAMM forwarding preserves errors for explicit inactive controls", {
  fake <- structure(
    list(gam = sampling_interface_model),
    class = "gamm"
  )
  expect_error(
    fitted_samples(fake, n_draws = 8, method = "mh", frequentist = FALSE),
    "frequentist.*not used"
  )
})


test_that("the old direct Gaussian core control names its replacement", {
  m <- sampling_interface_model
  expect_error(gaussian_draws(m, n_draws = 4, n_cores = 2),
    "Argument 'n_cores' has been removed. Use n_threads", fixed = TRUE)
})
