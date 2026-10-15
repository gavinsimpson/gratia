# Standalone research checks; run outside the sandbox from the repository root.
library("gratia")
source("notes/posterior-chains/smoothing-coverage-functions.R")
library("testthat")
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")

test_that("mixture CDF quantiles agree with known normal distributions", {
  means <- matrix(rep(c(-2, 0, 3), each = 16L), 16L)
  variances <- matrix(rep(c(.25, 1, 4), each = 16L), 16L)
  q <- coverage_mixture_quantiles(means, variances)
  expect_equal(q, outer(c(-2, 0, 3), rep(1, 3)) + outer(c(.5, 1, 2), qnorm(c(.025, .5, .975))), tolerance = 1e-8)
  q <- coverage_mixture_quantiles(matrix(rep(c(-1, 1), each = 8), 16L), matrix(.25, 16L, 1L))
  expect_equal(q[1L, 2L], 0, tolerance = 1e-8)
  expect_equal(q[1L, 1L], -q[1L, 3L], tolerance = 1e-8)
  expect_equal(.5 * pnorm(q[1L, ], -1, .5) + .5 * pnorm(q[1L, ], 1, .5),
    c(.025, .5, .975), tolerance = 1e-8)
  expect_equal(coverage_mixture_quantiles(matrix(2, 16, 1), matrix(0, 16, 1)), matrix(2, 1, 3))
  expect_error(coverage_mixture_quantiles(matrix(0, 2, 1), matrix(-1, 2, 1)), "all")
})

test_that("the simulated truths are in the fitted spline spaces", {
  cases <- coverage_cases()
  cases <- rbind(transform(cases, engine = "gam", method = "REML"),
    transform(cases, engine = "bam", method = "fREML"))
  errors <- numeric(nrow(cases))
  for (i in seq_len(nrow(cases))) {
    spec <- cases[i, , drop = FALSE]
    fixture <- coverage_data(spec, 1L)
    # Only the fitted basis is used here, regardless of smoothing optimisation.
    fit <- suppressWarnings(coverage_fit(spec, fixture, spec$engine, spec$method))
    X <- predict(fit, newdata = fixture$data, type = "lpmatrix")
    eta <- fixture$definition$intercept + fixture$definition$signal(fixture$data$x, fixture$data$z)
    beta <- qr.solve(X, eta)
    expect_equal(fit$family$linkinv(fixture$truth[, "link"]), fixture$truth[, "response_mean"])
    projection <- coverage_projection(fit, fixture)
    reconstructed <- matrix(drop(projection$A %*% beta) + projection$offset, ncol = 3L)
    errors[i] <- max(abs(reconstructed - fixture$truth[, 1:3]))
    expect_true(errors[i] < 1e-5, info = spec$case)
  }
  write.csv(data.frame(case = cases$case, engine = cases$engine, method = cases$method, maximum_geometry_error = errors),
    "notes/posterior-chains/smoothing-coverage-geometry.csv", row.names = FALSE)
})

test_that("data and interval results are reproducible without changing the RNG", {
  withr::local_seed(72)
  rng <- .Random.seed
  spec <- coverage_cases()[1L, , drop = FALSE]
  a <- coverage_data(spec, 3L)
  b <- coverage_data(spec, 3L)
  expect_identical(a$data, b$data)
  expect_identical(a$truth, b$truth)
  expect_identical(.Random.seed, rng)
  result <- coverage_replicate(spec, 1L, components = 8L)
  repeated <- coverage_replicate(spec, 1L, components = 8L)
  for (i in seq_along(result$records)) {
    expect_identical(result$records[[i]]$intervals, repeated$records[[i]]$intervals)
    expect_identical(dim(result$records[[i]]$intervals$conditional), c(11L, 3L, 4L))
  }
  expect_identical(.Random.seed, rng)
})

source("notes/posterior-chains/smoothing-coverage-summary.R")
test_that("coverage errors use independent datasets and paired common successes", {
  spec <- coverage_cases()[1L, , drop = FALSE]
  base <- coverage_replicate(spec, 1L, components = 8L)
  directory <- tempfile("coverage-summary-"); output <- file.path(directory, "summary")
  dir.create(directory); on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  protocol <- list(replicates = 4L, cases = spec, configurations = coverage_configurations())
  saveRDS(list(protocol = protocol), file.path(directory, "protocol.rds"))
  datasets <- lapply(seq_len(4L), function(i) {
    result <- base; result$replicate <- i; result$truth[] <- 0
    for (j in seq_along(result$records)) {
      record <- result$records[[j]]; record$replicate <- i
      for (s in c("conditional", "unconditional", "mixture")) {
        interval <- record$intervals[[s]]
        hit <- s != "conditional" || i %% 2L == 0L
        interval[, 1L, ] <- if (hit) -1 else 2
        interval[, 2L, ] <- if (hit) 0 else 2.5
        interval[, 3L, ] <- if (hit) 1 else 3
        record$intervals[[s]] <- interval
      }
      if (i == 4L) {record$intervals$mixture <- NULL; record$status$mixture <- "Deliberate numerical failure."}
      result$records[[j]] <- record
    }
    result
  })
  saveRDS(list(protocol = protocol, results = datasets), file.path(directory, "normal_known-00001.rds"))
  result <- coverage_summarise(directory, output)
  summary <- subset(result$summary, engine == "gam" & method == "REML" &
    quantity == "link" & stratum == "interior")
  conditional <- subset(summary, sampler == "conditional")
  mixture <- subset(summary, sampler == "mixture")
  expect_equal(conditional$coverage, .5)
  expect_equal(conditional$coverage_mcse, sd(c(0, 1, 0, 1)) / 2)
  expect_equal(mixture$returned, 3L)
  expect_equal(mixture$coverage, 1)
  expect_equal(mixture$returned_and_covered, .75)
  paired <- subset(result$paired, engine == "gam" & method == "REML" &
    quantity == "link" & stratum == "interior" & comparison == "mixture - conditional")
  expect_equal(paired$common_returned, 3L)
  expect_equal(paired$difference, 2 / 3)
  expect_equal(paired$difference_mcse, sd(c(1, 0, 1)) / sqrt(3))
  expect_equal(nrow(subset(result$statuses, status != "OK")), 5L)
  datasets[[4L]]$replicate <- 3L
  saveRDS(list(protocol = protocol, results = datasets), file.path(directory, "normal_known-00001.rds"))
  expect_error(coverage_summarise(directory, output), "anyDuplicated")
  datasets[[4L]]$replicate <- 4L
  saveRDS(list(protocol = protocol, results = datasets[-4L]), file.path(directory, "normal_known-00001.rds"))
  expect_error(coverage_summarise(directory, output), "setequal")
  datasets[[1L]]$records[[1L]]$actual_method <- "REML"
  saveRDS(list(protocol = protocol, results = datasets), file.path(directory, "normal_known-00001.rds"))
  expect_error(coverage_summarise(directory, output), "actual_method")
})

test_that("projected Gaussian intervals preserve covariance and offsets", {
  covariance <- matrix(c(4, 1, 1, 1), 2L)
  projection <- list(A = matrix(c(1, 2, 2, -1), 2L, byrow = TRUE), offset = c(.2, 1))
  result <- coverage_normal(c(.5, -.5), covariance, projection)
  expected <- outer(c(-.3, 2.5), rep(1, 3)) +
    outer(sqrt(c(12, 13)), qnorm(c(.025, .5, .975)))
  expect_equal(unname(result), expected)
  expect_error(coverage_normal(c(0, 0), matrix(c(1, 2, 2, 1), 2), projection), "positive semidefinite")
  expect_error(coverage_normal(c(0, 0), matrix(c(1, 0, 1, 1), 2), projection), "asymmetric")
})
