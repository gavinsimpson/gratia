expect_term_variances <- function(model, values) {
  got <- variance_comp(model, type = "terms", n_draws = 0)
  expected <- cov(values)
  dimnames(expected) <- list(make.unique(colnames(values)), make.unique(colnames(values)))
  expect_s3_class(got, c("variance_comp", "tbl_df", "tbl", "data.frame"))
  suffixes <- c("", if (nrow(got) - ncol(values) > 1L) {
    paste0(".", seq_len(nrow(got) - ncol(values) - 1L))
  })
  expect_named(got, c(".component", ".variance", ".std_dev",
    ".lower_ci", ".upper_ci", unlist(lapply(suffixes, function(suffix) {
      paste0(c(".proportion", ".proportion_lower_ci", ".proportion_upper_ci"), suffix)
    }))))
  terms <- seq_len(ncol(values))
  expect_equal(got$.component[terms], rownames(expected))
  expect_equal(got$.variance[terms], unname(diag(expected)))
  expect_equal(got$.std_dev[terms], sqrt(unname(diag(expected))))
  g <- if (inherits(model, "gam") || inherits(model, "scam")) model else model$gam
  if (!is.list(g$pterms)) {
    expect_equal(tail(got$.component, 1L), "total")
    expect_equal(tail(got$.variance, 1L), var(rowSums(values)))
    expect_equal(tail(got$.variance, 1L), sum(expected))
    if (sum(expected) > 0) {
      expect_equal(got$.proportion[terms], unname(rowSums(expected) / sum(expected)))
      expect_equal(sum(got$.proportion[terms]), 1)
    }
  }
  expect_true(all(is.na(got$.lower_ci)))
  expect_true(all(is.na(got$.upper_ci)))
  expect_equal(attr(got, "covariance"), expected)
  invisible(got)
}

test_that("term variances include complete parametric and smooth terms", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  for (fit in list(gam, bam)) {
    m <- fit(y ~ poly(z, 2) + fac * x + s(w, k = 6) + offset(off),
      data = d, method = "REML")
    values <- predict(m, type = "terms")
    got <- expect_term_variances(m, values)
    expect_equal(got$.component, c("poly(z, 2)", "fac", "x", "fac:x", "s(w)", "total"))
    covariance <- attr(got, "covariance")
    expect_equal(sum(covariance), var(as.numeric(predict(m, type = "link")) - d$off))
    expect_equal(got, variance_comp(m, type = "terms", n_draws = 0, rescale = FALSE,
      coverage = 0.9))
  }
  m <- gam(y ~ x + z, data = d)
  got <- expect_term_variances(m, predict(m, type = "terms"))
  expect_equal(got$.variance[1], unname(coef(m)["x"]^2 * var(d$x)))
  expect_gt(abs(tail(got$.variance, 1L) - sum(head(got$.variance, -1L))), 0.01)
  expect_equal(variance_comp(m), variance_comp(m, type = "penalty"))
})

test_that("term averaging uses fitting observations rather than likelihood weights", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$weight <- seq(1, 4, length.out = nrow(d))
  d$x[c(4, 30)] <- NA_real_
  for (fit in list(gam, bam)) {
    for (discrete in if (identical(fit, bam)) c(FALSE, TRUE) else FALSE) {
      args <- list(formula = y ~ x + s(w, k = 6), data = d,
        weights = d$weight, subset = seq_len(nrow(d)) > 15,
        na.action = na.exclude, method = "REML")
      if (identical(fit, bam)) {
        args$method <- "fREML"
        args$discrete <- discrete
      }
      m <- do.call(fit, args)
      values <- na.omit(predict(m, type = "terms"))
      expect_term_variances(m, values)
    }
  }
})

test_that("mixed-model term variances include fitted extra random coefficients", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  fixture <- variance_mixed_fixture(missing = TRUE)
  d <- fixture$data
  models <- fixture$models
  for (m in models) {
    used <- d[match(rownames(m$gam$model), rownames(d)), ]
    values <- predict(m$gam, newdata = used, type = "terms")
    if (inherits(m, "gamm")) {
      re <- nlme::ranef(m$lme, level = ncol(m$lme$groups))
      group <- as.character(m$lme$groups$group)
      group <- group[match(rownames(used), rownames(m$lme$groups))]
    } else {
      re <- lme4::ranef(m$mer)$group
      group <- as.character(used$group)
    }
    values <- cbind(values, "group:(Intercept)" = re[group, "(Intercept)"],
      "group:z" = used$z * re[group, "z"])
    got <- expect_term_variances(m, values)
    if (inherits(m, "gamm")) {
      eta <- m$lme$fitted[, ncol(m$lme$fitted)]
    } else {
      eta <- as.numeric(predict(m$mer, type = "link"))
      eta <- eta[!is.na(eta)]
    }
    expect_equal(sum(attr(got, "covariance")), var(as.numeric(eta)))
  }
})

test_that("mixed-model term covariances exclude varying formula offsets", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  models <- list(
    gamm(y ~ x + z + fac + s(w, k = 6) + offset(off),
      random = list(group = ~1), data = d, verbosePQL = FALSE),
    gamm4::gamm4(y ~ x + z + fac + s(w, k = 6) + offset(off),
      random = ~(1 | group), data = d)
  )
  for (m in models) {
    got <- variance_comp(m, type = "terms", n_draws = 0)
    eta <- if (inherits(m, "gamm")) {
      as.numeric(m$lme$fitted[, ncol(m$lme$fitted)])
    } else as.numeric(predict(m$mer)) - d$off
    expect_equal(sum(attr(got, "covariance")), var(eta))
    expect_false(any(grepl("offset", got$.component)))
  }
})

test_that("multi-penalty smooths contribute one whole term", {
  skip_if_not_installed("gamm4")
  models <- list(
    su_m_bivar_te, m_fs, su_gamm_bivar_te, m_fs_gamm,
    gamm4_surface, m_fs_gamm4
  )
  for (m in models) {
    g <- if (inherits(m, "gam")) m else m$gam
    values <- predict(g, type = "terms")
    if (inherits(m, "gamm4")) {
      re <- lme4::ranef(m$mer)$group
      if (!is.null(re)) {
        group <- as.character(model.frame(m$mer)$group)
        values <- cbind(values, "group:(Intercept)" = re[group, "(Intercept)"])
      }
    }
    got <- expect_term_variances(m, values)
    expect_equal(nrow(got), ncol(values) + 1L)
  }
})

test_that("PQL and generalized mixed contributions are on the link scale", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$count <- rpois(nrow(d), exp(sin(6 * d$w) + 0.2 * d$x +
    rnorm(12, sd = 0.5)[d$group]))
  models <- list(
    gamm(count ~ x + s(w, k = 6), random = list(group = ~1),
      family = poisson(), data = d, verbosePQL = FALSE),
    gamm4::gamm4(count ~ x + s(w, k = 6), random = ~(1 | group),
      family = poisson(), data = d)
  )
  for (m in models) {
    if (inherits(m, "gamm")) {
      re <- nlme::ranef(m$lme, level = ncol(m$lme$groups))
      group <- as.character(m$lme$groups$group)
      eta <- as.numeric(m$lme$fitted[, ncol(m$lme$fitted)])
    } else {
      re <- lme4::ranef(m$mer)$group
      group <- as.character(d$group)
      eta <- as.numeric(predict(m$mer, type = "link"))
    }
    values <- cbind(predict(m$gam, type = "terms"),
      "group:(Intercept)" = re[group, "(Intercept)"])
    got <- expect_term_variances(m, values)
    expect_equal(sum(attr(got, "covariance")), var(eta))
  }
})

test_that("nested and separate native blocks retain all contributions", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$nested <- factor(rep(1:2, each = 10, length.out = nrow(d)))
  d$y <- d$y + rep(rnorm(24, sd = 0.5), each = 10)
  m <- gamm(y ~ x + s(w, k = 6), random = list(group = ~1, nested = ~1),
    data = d, method = "REML")
  got <- variance_comp(m, type = "terms", n_draws = 0)
  expect_equal(got$.component,
    c("x", "s(w)", "group:(Intercept)", "nested:(Intercept)", "total"))
  expect_equal(sum(attr(got, "covariance")),
    var(as.numeric(m$lme$fitted[, ncol(m$lme$fitted)])))
  m <- gamm4::gamm4(y ~ x + s(w, k = 6), random = ~(1 | group) + (0 + z | group),
    data = d)
  got <- variance_comp(m, type = "terms", n_draws = 0)
  expect_equal(length(got$.component), 5L)
  expect_equal(sum(attr(got, "covariance")), var(as.numeric(predict(m$mer))))
})

test_that("term variances handle empty and distributional models", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- variance_intercept_model
  got <- variance_comp(m, type = "terms", n_draws = 0)
  expect_equal(got$.component, "total")
  expect_equal(got$.variance, 0)
  expect_equal(dim(attr(got, "covariance")), c(0L, 0L))
  m <- gam(list(y ~ s(x, k = 6), ~z), data = d, family = gaulss())
  expect_term_variances(m, predict(m, type = "terms"))
  expect_error(variance_comp(m, type = "unknown"), "arg")
  expect_error(variance_comp(m, type = "terms", n_draws = 0, newdata = d), "Additional arguments")
})

test_that("term mode avoids penalty intervals and retains SCAM term estimates", {
  skip_if_not_installed("gamm4")
  skip_if_not_installed("scam")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- gamm4_gaussian
  testthat::local_mocked_bindings(gamm4_variance_intervals = function(...) {
    stop("profiling must not run")
  })
  expect_silent(variance_comp(m, type = "terms", n_draws = 0))
  m <- variance_scam_model
  got <- expect_term_variances(m, predict(m, type = "terms"))
  expect_null(attr(got, "parameterisation"))
})
