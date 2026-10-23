expect_variance_totals_omitted <- function(model) {
  for (n_draws in c(0, 20)) {
    full <- variance_comp(model, type = "terms", n_draws = n_draws, seed = 83)
    individual <- variance_comp(model, type = "terms", n_draws = n_draws,
      seed = 83, include_total = FALSE)
    labels <- rownames(attr(full, "covariance"))
    keep <- match(labels, full$.component)
    expect_s3_class(individual, "variance_comp")
    expect_identical(individual$.component, labels %||% character())
    for (column in names(full)) {
      expect_identical(individual[[column]], full[[column]][keep])
    }
    for (attribute in c("covariance", "term_groups", "coverage")) {
      expect_identical(attr(individual, attribute), attr(full, attribute))
    }
    if (n_draws > 0) {
      expect_identical(attr(individual, "variance_draws"),
        attr(full, "variance_draws")[, keep, drop = FALSE])
      expected <- lapply(attr(full, "proportion_draws"), function(x) {
        x[, keep, drop = FALSE]
      })
      expect_identical(attr(individual, "proportion_draws"), expected)
      if (length(labels)) {
        expect_silent(ggplot2::ggplot_build(draw(individual)))
        expect_silent(ggplot2::ggplot_build(draw(individual, type = "proportion")))
      }
    } else {
      expect_null(attr(individual, "variance_draws"))
      expect_null(attr(individual, "proportion_draws"))
    }
  }
}

test_that("total rows can be omitted without changing individual term results", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$total <- d$x
  for (fit in list(gam, bam)) {
    model <- fit(y ~ total + z + s(w, k = 6), data = d, method = "REML")
    expect_variance_totals_omitted(model)
    expect_identical(variance_comp(model, include_total = FALSE), variance_comp(model))
    individual <- variance_comp(model, type = "terms", n_draws = 0,
      include_total = FALSE)
    expect_true("total.1" %in% individual$.component)
    expect_equal(sum(individual$.proportion), 1)
    for (bad in list(NA, NULL, logical(), c(TRUE, FALSE), 1, "false")) {
      expect_error(variance_comp(model, type = "terms", include_total = bad),
        "'include_total' must be TRUE or FALSE", fixed = TRUE)
    }
  }
})

test_that("omitting totals handles several predictors and intercept-only models", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  expect_variance_totals_omitted(gam(y ~ 1, data = d))
  model <- gam(list(y ~ x, ~1, 1 + 2 ~ 0 + s(w, k = 6)), data = d,
    family = gaulss())
  expect_variance_totals_omitted(model)
  individual <- variance_comp(model, type = "terms", n_draws = 0,
    include_total = FALSE)
  expect_equal(sum(individual$.proportion, na.rm = TRUE), 1)
  expect_equal(sum(individual$.proportion.1, na.rm = TRUE), 1)
})

test_that("mixed models can omit totals while retaining fitted random effects", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  models <- list(
    gamm(y ~ x + z + s(w, k = 6), random = list(group = ~1),
      data = d, method = "REML"),
    gamm4::gamm4(y ~ x + z + s(w, k = 6), random = ~(1 | group), data = d)
  )
  for (model in models) expect_variance_totals_omitted(model)
})

test_that("SCAM term results can omit totals", {
  skip_if_not_installed("scam")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$y <- 2 * d$x + exp(2 * d$w) + rnorm(nrow(d), sd = 0.1)
  model <- scam::scam(y ~ x + s(w, bs = "mpi", k = 6), data = d)
  expect_variance_totals_omitted(model)
})
