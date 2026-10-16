model_aliases <- c(
  additive = "eg1", bivariate_surface = "eg2", varying_coefficient = "eg3",
  factor_by = "eg4", additive_factor = "eg5", additive_group_effects = "eg6",
  additive_correlated = "eg7", gu_wahba_f2 = "gwf2", luo_wahba_f6 = "lwf6"
)

test_that("descriptive model aliases preserve defaults and legacy distributions", {
  for (alias in names(model_aliases)) {
    original <- model_aliases[[alias]]
    expect_identical(data_sim(alias, n = 23, seed = 42),
      data_sim(original, n = 23, seed = 42), info = alias)
    for (seed in c(0, 42)) for (dist in c("normal", "poisson", "binary", "negbin",
        "gamma", "tweedie", "ocat", "ordered categorical")) {
      expect_identical(
        data_sim(alias, n = 24, dist = dist, scale = 0.3, theta = 5,
          power = 1.7, n_cat = 3, cuts = c(-1, 1), seed = seed),
        data_sim(original, n = 24, dist = dist, scale = 0.3, theta = 5,
          power = 1.7, n_cat = 3, cuts = c(-1, 1), seed = seed),
        info = paste(alias, dist, seed)
      )
    }
  }
})

test_that("descriptive aliases preserve family responses and all truth metadata", {
  families <- list(stats::poisson(), mgcv::gaulss(), mgcv::mvn(d = 2),
    mgcv::gfam(list(stats::gaussian(), stats::poisson())))
  for (alias in names(model_aliases)) {
    original <- model_aliases[[alias]]
    for (family in families) {
      expect_identical(data_sim(alias, n = 24, family = family, seed = 3),
        data_sim(original, n = 24, family = family, seed = 3), info = alias)
    }
    expect_identical(
      data_sim(alias, n = 24, family = stats::gaussian(), seed = 3,
        predictors = function(d) d$f / 5 + 1, parameters = list(dispersion = 0.25)),
      data_sim(original, n = 24, family = stats::gaussian(), seed = 3,
        predictors = function(d) d$f / 5 + 1, parameters = list(dispersion = 0.25)),
      info = alias
    )
  }
})

test_that("descriptive aliases consume the same RNG draws without an explicit seed", {
  withr::with_preserve_seed({
    for (alias in names(model_aliases)) {
      set.seed(42)
      expected <- data_sim(model_aliases[[alias]], n = 24)
      expected_seed <- .Random.seed
      set.seed(42)
      expect_identical(data_sim(alias, n = 24), expected, info = alias)
      expect_identical(.Random.seed, expected_seed, info = alias)
    }
  })
  expect_error(data_sim("additive_unknown"), "Unknown 'model'")
})
