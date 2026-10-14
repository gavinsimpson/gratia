test_that("ordered residual correlation retains omitted positions for both NA actions", {
  d <- data.frame(x = seq_len(10), y = c(1, 6, 2, 7, 5, 8, 9, 3, 10, 4))
  d$x[3L] <- NA_real_
  for (action in list(na.omit, na.exclude)) {
    model <- lm(y ~ x, data = d, na.action = action)
    prepared <- diagnostic_ordered_residuals(model, type = "response")
    expect_identical(prepared$data$.row, setdiff(seq_len(10), 3L))
    actual <- residual_correlation(model, type = "response", lag_max = 2L)
    expect_identical(actual$n_pairs, c(9L, 7L, 6L))
    expanded <- rep(NA_real_, 10)
    expanded[-3L] <- residuals(model_used_rows(model))
    expected <- ordered_lag_correlations(expanded, rep(1L, 10), 2L)
    expect_equal(actual$correlation, expected$correlation)
  }
})

test_that("full interleaved series identify omitted membership without foreign gaps", {
  d <- data.frame(x = seq_len(10), y = c(1, 6, 2, 7, 5, 8, 9, 3, 10, 4),
    series = rep(c("A", "B"), 5))
  no_missing <- lm(y ~ x, data = d)
  expect_identical(residual_correlation(no_missing, type = "response", lag_max = 2L,
    series = d$series)$n_pairs, c(10L, 8L, 6L))
  d$x[3L] <- NA_real_
  for (action in list(na.omit, na.exclude)) {
    model <- lm(y ~ x, data = d, na.action = action)
    actual <- residual_correlation(model, type = "response", lag_max = 2L,
      series = d$series)
    expect_identical(actual$n_pairs, c(9L, 6L, 5L))
    expanded <- rep(NA_real_, 10)
    expanded[-3L] <- residuals(model_used_rows(model))
    expected <- ordered_lag_correlations(expanded, d$series, 2L)
    expect_equal(actual$correlation, expected$correlation)
    # Retained-only labels cannot identify which series owns an omitted row.
    compact <- residual_correlation(model, type = "response", lag_max = 2L,
      series = d$series[-3L])
    expect_identical(compact$n_pairs, c(9L, 5L, 5L))
  }
})

test_that("BAM omission gaps remain inside native AR.start segments", {
  set.seed(37)
  d <- data.frame(x = runif(12), y = rnorm(12), start = rep(FALSE, 12))
  d$start[c(1L, 7L)] <- TRUE
  d$y[c(3L, 9L)] <- NA_real_
  for (action in list(na.omit, na.exclude)) {
    model <- mgcv::bam(y ~ s(x, k = 4), data = d, rho = .4, AR.start = start,
      na.action = action, discrete = TRUE)
    for (type in c("standardized", "working.standardized", "quantile")) {
      actual <- residual_correlation(model, type = type, lag_max = 2L)
      expect_identical(actual$n_pairs, c(10L, 6L, 4L))
    }
  }
})
