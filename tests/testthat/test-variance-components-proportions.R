# Native link predictions supply each total independently of the allocation code.
reference_term_proportion_draws <- function(model, n_draws, seed,
    groups = NULL, unconditional = FALSE) {
  model <- model_used_rows(model)
  values <- predict_model(model, type = "terms")
  if (is.null(groups)) groups <- list(colnames(values))
  total_labels <- c("total", if (length(groups) > 1L) paste0("total.",
    seq_len(length(groups) - 1L)))
  labels <- c(colnames(values), total_labels)
  answer <- stats::setNames(lapply(groups, function(i) {
    matrix(NA_real_, n_draws, length(labels), dimnames = list(NULL, labels))
  }), total_labels)
  betas <- gaussian_draws(model, n_draws = n_draws, seed = seed,
    unconditional = unconditional, mvn_method = "mgcv")
  for (i in seq_len(n_draws)) {
    draw <- model
    draw$coefficients <- betas[i, ]
    values <- predict_model(draw, type = "terms")
    eta <- as.matrix(predict_model(draw, type = "link"))
    for (k in seq_along(groups)) {
      offset <- if (is.list(draw$offset)) draw$offset[k][[1L]] else draw$offset
      total <- eta[, k] - (offset %||% 0)
      columns <- match(groups[[k]], colnames(values))
      answer[[k]][i, columns] <- as.numeric(cov(values[, columns, drop = FALSE],
        total)) / var(total)
      answer[[k]][i, ncol(values) + k] <- 1
    }
  }
  answer
}

test_that("proportions allocate covariance and use the same posterior draws", {
  withr::local_preserve_seed()
  for (m in variance_total_models()) {
    got <- variance_comp(m, type = "terms", n_draws = 137, seed = 83,
      coverage = 0.8)
    C <- attr(got, "covariance")
    terms <- seq_len(nrow(C))
    expect_equal(got$.proportion[terms], unname(rowSums(C) / sum(C)))
    expect_equal(sum(got$.proportion[terms]), 1)
    expect_equal(tail(got$.proportion, 1), 1)
    expect_gt(max(abs(got$.proportion[terms] - got$.variance[terms] / sum(C))), 0.01)
    expected <- reference_term_proportion_draws(m, 137, 83)
    expect_equal(attr(got, "proportion_draws"), expected)
    expect_equal(rowSums(expected$total[, terms, drop = FALSE]), rep(1, 137))
    bounds <- apply(expected$total, 2L, quantile, probs = c(0.1, 0.9))
    expect_equal(got$.proportion_lower_ci, unname(bounds[1L, ]))
    expect_equal(got$.proportion_upper_ci, unname(bounds[2L, ]))
    point <- variance_comp(m, type = "terms", n_draws = 0)
    expect_equal(point$.proportion, got$.proportion)
    expect_true(all(is.na(point$.proportion_lower_ci)))
    expect_true(all(is.na(point$.proportion_upper_ci)))
    expect_null(attr(point, "proportion_draws"))
    narrower <- variance_comp(m, type = "terms", n_draws = 137, seed = 83,
      coverage = 0.5)
    expect_equal(attr(narrower, "proportion_draws"), expected)
    expect_true(all(narrower$.proportion_lower_ci >= got$.proportion_lower_ci))
    expect_true(all(narrower$.proportion_upper_ci <= got$.proportion_upper_ci))
  }
})

test_that("signed allocations and their intervals retain values outside zero to one", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$z <- d$x + runif(nrow(d), -0.05, 0.05)
  d$y <- 2 * d$x - d$z + rnorm(nrow(d), sd = 0.05)
  m <- gam(y ~ x + z, data = d)
  got <- variance_comp(m, type = "terms", n_draws = 50, seed = 83)
  expect_gt(got$.proportion[1], 1)
  expect_lt(got$.proportion[2], 0)
  expect_gt(got$.proportion_lower_ci[1], 1)
  expect_lt(got$.proportion_upper_ci[2], 0)
  expect_equal(sum(got$.proportion[1:2]), 1)
  expect_equal(attr(got, "proportion_draws"), reference_term_proportion_draws(m, 50, 83))
  # A small positive variance must not be treated as zero by a fixed tolerance.
  small <- m
  small$coefficients <- m$coefficients * 1e-100
  small$Vp <- m$Vp * 1e-200
  scaled <- variance_comp(small, type = "terms", n_draws = 50, seed = 83)
  expect_equal(scaled$.proportion, got$.proportion)
  expect_equal(attr(scaled, "proportion_draws"), attr(got, "proportion_draws"))
})

test_that("zero total variances have undefined proportions and defined draws set intervals", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  single <- gam(y ~ x, data = d)
  one <- variance_comp(single, type = "terms", n_draws = 20, seed = 83)
  expect_equal(one$.proportion, c(1, 1))
  expect_equal(one$.proportion_lower_ci, c(1, 1))
  expect_equal(one$.proportion_upper_ci, c(1, 1))
  expect_equal(unname(attr(one, "proportion_draws")$total), matrix(1, 20, 2))
  m <- gam(y ~ 1 + offset(off), data = d)
  expect_silent(got <- variance_comp(m, type = "terms", n_draws = 20, seed = 83))
  expect_true(all(is.na(got$.proportion)))
  expect_true(all(is.na(got$.proportion_lower_ci)))
  expect_true(all(is.na(got$.proportion_upper_ci)))
  expect_true(all(is.na(attr(got, "proportion_draws")$total)))
  d$z <- d$x
  m <- gam(y ~ x + z, data = d)
  m$coefficients[] <- c(0, 1, -1)
  point <- variance_comp(m, type = "terms", n_draws = 0)
  expect_gt(point$.variance[1], 0)
  expect_equal(point$.variance[3], 0)
  expect_true(all(is.na(point$.proportion)))
  testthat::local_mocked_bindings(gaussian_draws = function(...) {
    rbind(c(0, 1, -1), c(0, 2, -1), c(0, 3, -1))
  })
  expect_silent(got <- variance_comp(m, type = "terms", n_draws = 3,
    coverage = 0.8))
  draws <- attr(got, "proportion_draws")$total
  expect_true(all(is.na(draws[1, ])))
  expect_equal(unname(draws[2:3, ]), rbind(c(2, -1, 1), c(1.5, -0.5, 1)))
  expect_equal(got$.proportion_lower_ci, c(1.55, -0.95, 1))
  expect_equal(got$.proportion_upper_ci, c(1.95, -0.55, 1))
})

test_that("each predictor has separate allocations for its own and shared terms", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- gam(list(y ~ x, ~z, 1 + 2 ~ 0 + s(w, k = 6)), data = d,
    family = gaulss())
  got <- variance_comp(m, type = "terms", n_draws = 50, seed = 83,
    coverage = 0.8)
  groups <- list(c("x", "s.2(w)"), c("z.1", "s.2(w)"))
  expected <- reference_term_proportion_draws(m, 50, 83, groups = groups)
  expect_equal(attr(got, "proportion_draws"), expected)
  expect_named(got, c(".component", ".variance", ".std_dev", ".lower_ci", ".upper_ci",
    ".proportion", ".proportion_lower_ci", ".proportion_upper_ci",
    ".proportion.1", ".proportion_lower_ci.1", ".proportion_upper_ci.1"))
  values <- predict(m, type = "terms")
  for (k in 1:2) {
    suffix <- if (k == 1L) "" else ".1"
    i <- match(groups[[k]], got$.component)
    C <- cov(values[, groups[[k]], drop = FALSE])
    proportions <- got[[paste0(".proportion", suffix)]]
    expect_equal(proportions[i], unname(rowSums(C) / sum(C)))
    expect_equal(sum(proportions[i]), 1)
    own <- c(i, ncol(values) + k)
    expect_true(all(is.na(proportions[setdiff(seq_len(nrow(got)), own)])))
    expect_equal(proportions[ncol(values) + k], 1)
    bounds <- vapply(seq_len(ncol(expected[[k]])), function(j) {
      x <- expected[[k]][, j]
      if (all(is.na(x))) return(c(NA_real_, NA_real_))
      quantile(x, c(0.1, 0.9), names = FALSE)
    }, numeric(2))
    expect_equal(got[[paste0(".proportion_lower_ci", suffix)]], bounds[1, ],
      ignore_attr = TRUE)
    expect_equal(got[[paste0(".proportion_upper_ci", suffix)]], bounds[2, ],
      ignore_attr = TRUE)
    expect_equal(rowSums(expected[[k]][, i, drop = FALSE]), rep(1, 50))
  }
  m <- gam(list(y ~ x + z, ~1), data = d, family = gaulss())
  got <- variance_comp(m, type = "terms", n_draws = 20, seed = 83)
  expect_true(all(is.na(got$.proportion.1)))
  expect_true(all(is.na(got$.proportion_lower_ci.1)))
  expect_true(all(is.na(attr(got, "proportion_draws")$total.1)))
})

test_that("mixed-model proportions retain joint dependence with fitted random effects", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  models <- variance_mixed_fixture()$models
  for (m in models) {
    g <- model_used_rows(m$gam)
    random <- if (inherits(m, "gamm")) gamm_fitted_random_terms(m, g) else
      gamm4_fitted_random_terms(m, g)
    fitted <- cbind(predict_model(g, type = "terms"), random)
    expected <- withr::with_seed(83, {
      X <- predict_model(g, type = "lpmatrix")
      betas <- gaussian_draws(g, n_draws = 37, mvn_method = "mgcv")
      delta <- t(sweep(betas, 2L, coef(g), `-`))
      extra <- variance_terms_random_draws(m, g, X, delta)
      columns <- variance_terms_columns(g)
      t(vapply(seq_len(37), function(i) {
        draw <- fitted
        for (j in seq_along(columns)) {
          index <- columns[[j]]
          draw[, j] <- draw[, j] + as.numeric(X[, index, drop = FALSE] %*%
            delta[index, i])
        }
        for (j in seq_along(extra$designs)) {
          draw[, length(columns) + j] <- draw[, length(columns) + j] +
            as.numeric(extra$designs[[j]] %*% extra$perturbations[[j]][, i])
        }
        C <- cov(draw)
        c(rowSums(C) / sum(C), total = 1)
      }, numeric(ncol(fitted) + 1L)))
    })
    got <- variance_comp(m, type = "terms", n_draws = 37, seed = 83,
      coverage = 0.8)
    expect_equal(attr(got, "proportion_draws")$total, expected, ignore_attr = TRUE)
    expect_equal(rowSums(expected[, -ncol(expected), drop = FALSE]), rep(1, 37))
    bounds <- apply(expected, 2L, quantile, probs = c(0.1, 0.9))
    expect_equal(got$.proportion_lower_ci, unname(bounds[1, ]))
    expect_equal(got$.proportion_upper_ci, unname(bounds[2, ]))
    expect_equal(got$.proportion[-nrow(got)],
      unname(rowSums(attr(got, "covariance")) / sum(attr(got, "covariance"))))
  }
})

test_that("SCAM allocations use the jointly transformed coefficients", {
  skip_if_not_installed("scam")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- variance_scam_model
  got <- variance_comp(m, type = "terms", n_draws = 30, seed = 83)
  X <- predict_model(m, type = "lpmatrix")
  betas <- gaussian_draws(m, n_draws = 30, seed = 83, mvn_method = "mgcv")
  columns <- variance_terms_columns(m)
  expected <- t(vapply(seq_len(30), function(i) {
    values <- vapply(columns, function(j) drop(X[, j, drop = FALSE] %*% betas[i, j]),
      numeric(nrow(X)))
    C <- cov(values)
    c(rowSums(C) / sum(C), 1)
  }, numeric(3)))
  expect_equal(attr(got, "proportion_draws")$total, expected, ignore_attr = TRUE)
  expect_equal(sum(got$.proportion[1:2]), 1)
})
