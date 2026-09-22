test_that("link contrasts retain joint covariance, ordering and offsets", {
  f <- conditional_difference_fixture()
  m <- f$model
  cd <- conditional_differences(m, "a", c("x", "b"), n_vals = 4,
    scale = "link", unconditional = TRUE)
  expect_s3_class(cd, "conditional_differences")
  expect_equal(nrow(cd), 3 * 4 * 2)
  expect_identical(as.character(cd$.level_1[c(1, 9, 17)]), c("B", "B", "A"))
  ep <- conditional_difference_endpoints(cd, m)
  X <- predict(m, ep$a, type = "lpmatrix") - predict(m, ep$b, type = "lpmatrix")
  V <- vcov(m, unconditional = TRUE)
  se <- unname(sqrt(rowSums((X %*% V) * X)))
  expect_equal(cd$.diff, as.numeric(predict(m, ep$a) - predict(m, ep$b)))
  expect_equal(cd$.se, se)
  expect_equal(cd$.lower_ci, cd$.diff - qnorm(0.975) * se)
  response <- conditional_differences(m, "a", c("x", "b"), n_vals = 4,
    unconditional = TRUE)
  expect_equal(response$.diff, cd$.diff)
  expect_equal(response$.se, cd$.se)
  expect_equal(conditional_differences(m, "a", c("x", "b"), n_vals = 4,
    scale = "linear_predictor")$.diff, cd$.diff)
})

test_that("response contrasts transform endpoints and use the joint gradient", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  cd <- conditional_differences(m, "a", list(x = c(0.2, 0.8), exposure = 2))
  ep <- conditional_difference_endpoints(cd, m)
  mu1 <- as.numeric(predict(m, ep$a, type = "response"))
  mu2 <- as.numeric(predict(m, ep$b, type = "response"))
  J <- predict(m, ep$a, type = "lpmatrix") * mu1 -
    predict(m, ep$b, type = "lpmatrix") * mu2
  expect_equal(cd$.diff, mu1 - mu2)
  expect_equal(cd$.se, unname(sqrt(rowSums((J %*% vcov(m)) * J))))
  half <- conditional_differences(m, "a", list(x = c(0.2, 0.8), exposure = 1))
  expect_equal(cd$.diff, 2 * half$.diff)
  expect_equal(cd$.se, 2 * half$.se)
})

test_that("simulation pairs shared draws before summarising on either scale", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  betas <- generate_draws(m, n = 101, seed = 7, mvn_method = "mgcv")
  for (scale in c("link", "response")) {
    cd <- conditional_differences(m, "a", list(x = c(0.2, 0.8), exposure = 2),
      scale = scale, uncertainty = "simulation", method = "user", draws = betas,
      ci_level = 0.8)
    ep <- conditional_difference_endpoints(cd, m)
    e1 <- predict(m, ep$a, type = "lpmatrix") %*% t(betas) + log(2)
    e2 <- predict(m, ep$b, type = "lpmatrix") %*% t(betas) + log(2)
    ds <- if (scale == "response") exp(e1) - exp(e2) else e1 - e2
    expect_equal(cd$.se, apply(ds, 1, sd), ignore_attr = TRUE)
    expect_equal(cd$.lower_ci, apply(ds, 1, quantile, probs = 0.1), ignore_attr = TRUE)
    expect_equal(cd$.upper_ci, apply(ds, 1, quantile, probs = 0.9), ignore_attr = TRUE)
    delta <- conditional_differences(m, "a", list(x = c(0.2, 0.8), exposure = 2),
      scale = scale)
    expect_equal(cd$.diff, delta$.diff)
  }
})

test_that("simulation is reproducible and approximates analytic link intervals", {
  f <- conditional_difference_fixture()
  m <- f$model
  withr::local_seed(21)
  rng <- .Random.seed
  sim <- conditional_differences(m, "a", list(x = 0.5), scale = "link",
    uncertainty = "simulation", n_sim = 10000, seed = 42, mvn_method = "mgcv")
  expect_identical(.Random.seed, rng)
  expect_identical(sim, conditional_differences(m, "a", list(x = 0.5),
    scale = "link", uncertainty = "simulation", n_sim = 10000,
    seed = 42, mvn_method = "mgcv"))
  delta <- conditional_differences(m, "a", list(x = 0.5), scale = "link")
  expect_equal(sim$.se / delta$.se, rep(1, 3), tolerance = 0.04)
  expect_equal((sim$.upper_ci - sim$.diff) / delta$.se,
    rep(qnorm(0.975), 3), tolerance = 0.08)
})

test_that("joint factors, level subsets and conditioning strata are distinct", {
  f <- conditional_difference_fixture()
  m <- f$model
  cd <- conditional_differences(m, c("a", "b"), "x", n_vals = 3)
  expect_equal(nrow(cd), choose(6, 2) * 3)
  expect_true(all(c("a_1", "a_2", "b_1", "b_2") %in% names(cd)))
  ep <- conditional_difference_endpoints(cd, m)
  expect_equal(cd$.diff, as.numeric(predict(m, ep$a) - predict(m, ep$b)))
  restricted <- conditional_differences(m, "a",
    list(a = c("C", "B"), "x", b = "high"), n_vals = 3)
  expect_equal(nrow(restricted), 3)
  expect_true(all(restricted$.level_1 == "B" & restricted$.level_2 == "C"))
  expect_true(all(restricted$b == "high"))
  expect_equal(length(unique(restricted$x)), 3)
  subset_data <- droplevels(subset(f$data, a != "A"))
  from_data <- conditional_differences(m, "a", "x", data = subset_data, n_vals = 3)
  expect_equal(nrow(from_data), 3)
  expect_true(all(from_data$.level_1 == "B" & from_data$.level_2 == "C"))
  # Explicit condition values take precedence over ranges in supplied data.
  supplied <- f$data[sample.int(nrow(f$data)), ]
  expect_equal(conditional_differences(m, "a", list(x = c(0.2, 0.8)),
    data = supplied), conditional_differences(m, "a", list(x = c(0.2, 0.8))))
})

test_that("incomplete factor grids compare only shared strata", {
  f <- conditional_difference_fixture()
  d <- subset(f$data, !(a == "C" & b == "high"))
  m <- gam(y ~ a + b + s(x, k = 5), data = d, method = "REML")
  cd <- conditional_differences(m, "a", c("x", "b"), n_vals = 3, complete = FALSE)
  expect_equal(nrow(cd), (3 + 1) * 3)
  expect_false(any(cd$b == "high" & (cd$.level_1 == "C" | cd$.level_2 == "C")))
  joint <- conditional_differences(m, c("a", "b"), "x", n_vals = 3, complete = FALSE)
  expect_equal(nrow(joint), choose(5, 2) * 3)
  # Supplied data cannot make unobserved training combinations count as observed.
  expect_equal(cd, conditional_differences(m, "a", c("x", "b"),
    data = f$data, n_vals = 3, complete = FALSE))
  nested <- subset(d, (a == "B" & b == "low") | (a == "A" & b == "high"))
  nested$a <- droplevels(nested$a)
  mn <- gam(y ~ a + s(x, by = b, k = 5), data = nested, method = "REML")
  expect_error(conditional_differences(mn, "a", c("x", "b"), complete = FALSE),
    "No comparison groups share")
})

test_that("exclusions are honoured by both uncertainty methods", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  excluded <- "s(x):B"
  betas <- generate_draws(m, n = 20, seed = 3)
  cd <- conditional_differences(m, "a", list(x = 0.4), exclude = excluded,
    uncertainty = "simulation", method = "user", draws = betas)
  ep <- conditional_difference_endpoints(cd, m)
  expect_equal(cd$.diff, as.numeric(predict(m, ep$a, type = "response", exclude = excluded) -
    predict(m, ep$b, type = "response", exclude = excluded)))
  a <- fitted_samples(m, data = ep$a, method = "user", draws = betas, exclude = excluded)
  b <- fitted_samples(m, data = ep$b, method = "user", draws = betas, exclude = excluded)
  ds <- matrix(a$.fitted - b$.fitted, nrow = nrow(cd))
  expect_equal(cd$.se, apply(ds, 1, sd))
  expect_identical(attr(cd, "exclude"), excluded)
  delta <- conditional_differences(m, "a", list(x = 0.4), exclude = excluded)
  expect_equal(delta$.diff, cd$.diff)
})

test_that("bam, MH samples, and plotting are supported", {
  f <- conditional_difference_fixture()
  m <- bam(y ~ a + s(x, by = a, k = 5), data = f$data)
  cd <- conditional_differences(m, "a", "x", n_vals = 3)
  expect_s3_class(cd, "conditional_differences")
  mh <- conditional_differences(f$model, "a", list(x = 0.5),
    uncertainty = "simulation", method = "mh", n_sim = 10, burnin = 10, seed = 8)
  expect_true(all(is.finite(mh$.se)))
  for (by in list("a", c("a", "b"))) {
    cd <- conditional_differences(f$model, by, c("x", "b")[!c("x", "b") %in% by],
      n_vals = 3)
    expect_silent(built <- ggplot2::ggplot_build(draw(cd)))
    expect_equal(nrow(built$layout$layout), length(unique(cd$.contrast)))
    expect_identical(attr(cd[1:2, ], "by"), by)
    expect_identical(attr(cd[1:2, ], "channels"), attr(cd, "channels"))
  }
  discrete <- conditional_differences(f$model, "a", "b")
  expect_silent(ggplot2::ggplot_build(draw(discrete)))
})

test_that("invalid inputs fail explicitly", {
  f <- conditional_difference_fixture()
  m <- f$model
  for (by in list(NULL, "x", "unknown", c("a", "a"), NA_character_)) {
    expect_error(conditional_differences(m, by, "x"), "must name one or more distinct factors")
  }
  expect_error(conditional_differences(m, "a"), "must be supplied")
  expect_error(conditional_differences(m, "a", "a"), "must include a covariate other than")
  expect_error(conditional_differences(m, "a", list("x", a = "A")), "At least two")
  expect_error(conditional_differences(m, "a", c("x", "x")), "must not be repeated")
  expect_error(conditional_differences(m, "a", "x", ci_level = 1), "ci_level")
  expect_error(conditional_differences(m, "a", "x", n_vals = 0), "n_vals")
  expect_error(conditional_differences(m, "a", "x", complete = NA), "complete")
  expect_error(conditional_differences(m, "a", "x", terms = "a"), "cannot be supplied")
  expect_error(conditional_differences(m, "a", "x", uncertainty = "simulation", n_sim = 1), "n_sim")
  expect_error(conditional_differences(m, "a", "x", uncertainty = "simulation",
    method = "user", draws = matrix(1, 2, 2)), "draws")
  expect_error(conditional_differences(m, "a", "x", uncertainty = "simulation",
    method = "inla"), "not yet implemented")
  unsupported <- gam(list(y ~ a + x, ~ 1), data = f$data, family = gaulss())
  expect_error(conditional_differences(unsupported, "a", "x"), "not supported")
})

test_that("random smooths and random intercepts can be explicitly excluded", {
  withr::local_seed(54)
  d <- data.frame(x = runif(150), a = factor(rep(c("A", "B"), 75)),
    subject = factor(rep(1:5, each = 30)), word = factor(rep(1:3, 50)))
  d$y <- 1 + sin(d$x * 3) + as.integer(d$a) + rnorm(150, sd = 0.3)
  m <- gam(y ~ a + s(x, by = a, k = 5) + s(x, subject, bs = "fs", k = 4) +
    s(word, bs = "re"), data = d, method = "REML")
  exclude <- c("s(x,subject)", "s(word)")
  cd <- conditional_differences(m, "a", "x", n_vals = 4, exclude = exclude)
  ep <- conditional_difference_endpoints(cd, m)
  expect_equal(cd$.diff, as.numeric(predict(m, ep$a, exclude = exclude) -
    predict(m, ep$b, exclude = exclude)))
  # These additive random effects cancel when conditioned on the same levels.
  included <- conditional_differences(m, "a", "x", n_vals = 4)
  expect_equal(cd$.diff, included$.diff)
  expect_equal(cd$.se, included$.se)
})

test_that("shared intercept uncertainty cancels only on the link scale", {
  f <- conditional_difference_fixture(poisson())
  m <- f$model
  betas <- matrix(rep(coef(m), each = 5), nrow = 5)
  betas[, 1] <- betas[, 1] + seq(-0.4, 0.4, length.out = 5)
  link <- conditional_differences(m, "a", list(x = 0.5), scale = "link",
    uncertainty = "simulation", method = "user", draws = betas)
  response <- conditional_differences(m, "a", list(x = 0.5), scale = "response",
    uncertainty = "simulation", method = "user", draws = betas)
  expect_equal(link$.se, rep(0, nrow(link)), tolerance = 1e-14)
  expect_true(all(response$.se > 0))
})

test_that("explicit seeds also preserve an absent RNG state", {
  f <- conditional_difference_fixture()
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  conditional_differences(f$model, "a", list(x = 0.5),
    uncertainty = "simulation", n_sim = 10, seed = 1)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
