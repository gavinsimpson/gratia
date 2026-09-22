# Regression tests for issue #196: expressions must see each group's data.
slice_by_data <- function() {
  data.frame(
    x = c(0, 4, 10, 5, 12, 20),
    fac = factor(rep(c("A", "B"), each = 3), levels = c("B", "A", "unused")),
    z = c(1, 2, 3, 7, 8, 9)
  )
}

test_that("grouped slices use each group's range and overall typical values", {
  d <- slice_by_data()
  ds <- data_slice(d, x = evenly(x, n = 5), .by = fac)
  expect_equal(ds$x[ds$fac == "A"], seq(0, 10, length.out = 5))
  expect_equal(ds$x[ds$fac == "B"], seq(5, 20, length.out = 5))
  expect_equal(ds$z, rep(typical_values(d)$z, 10))
  expect_identical(levels(ds$fac), levels(d$fac))
  expect_s3_class(ds, "tbl_df")
  expect_false(inherits(ds, "grouped_df"))

  n <- 3L
  ds <- data_slice(d, x = evenly(x, n = n), z = mean(z), .by = fac)
  expect_equal(ds$z, rep(c(2, 8), each = 3))
  expect_equal(nrow(ds), 6L)
  expect_equal(unname(data_slice(d, x = quantile(x, c(0, 1)), .by = fac)$x),
    c(0, 10, 5, 20))
})

test_that("group expressions select once and preserve factor metadata", {
  d <- slice_by_data()
  d$fac <- ordered(d$fac, levels = levels(d$fac))
  ds <- data_slice(d, x = evenly(x, n = 3), fac = evenly(fac), .by = fac)
  expect_equal(nrow(ds), 6L)
  expect_true(is.ordered(ds$fac))
  expect_identical(levels(ds$fac), levels(d$fac))
  ds <- data_slice(d, x = evenly(x, n = 3), fac = level(fac, "B"), .by = fac)
  expect_equal(ds$x, c(5, 12.5, 20))
  expect_true(all(ds$fac == "B"))
  expect_identical(levels(ds$fac), levels(d$fac))
  expect_equal(nrow(data_slice(d, .by = fac)), 2L)
  expect_error(data_slice(d, fac = level(fac, "unused"), .by = fac),
    "No observed groups")
  expect_error(data_slice(d, .by = absent), "absent")
  expect_error(data_slice(d, .by = c(renamed = fac)), "rename")
})

test_that("multiple grouping variables use only observed combinations", {
  d <- slice_by_data()
  d$site <- factor(c("one", "one", "two", "two", "two", "two"))
  ds <- data_slice(d, x = evenly(x, n = 3), .by = c(fac, site))
  expect_equal(nrow(ds), 9L)
  expect_false(any(ds$fac == "B" & ds$site == "one"))
  expect_equal(ds$x[ds$fac == "A" & ds$site == "one"], c(0, 2, 4))
  expect_equal(ds$x[ds$fac == "A" & ds$site == "two"], rep(10, 3))
  expect_equal(ds, data_slice(d, x = evenly(x, n = 3),
    .by = all_of(c("fac", "site"))))
  expect_equal(ds, data_slice(d, x = evenly(x, n = 3), .by = where(is.factor)))
})

test_that("explicit values and bounds remain under caller control", {
  d <- slice_by_data()
  ds <- data_slice(d, x = c(-10, 30), .by = fac)
  expect_equal(ds$x, rep(c(-10, 30), 2))
  ds <- data_slice(d, x = evenly(x, n = 3, lower = -5), .by = fac)
  expect_equal(ds$x, c(-5, 2.5, 10, -5, 7.5, 20))
  ds <- data_slice(d, x = evenly(x, by = 5), .by = fac)
  expect_equal(ds$x, c(0, 5, 10, 5, 10, 15, 20))
})

test_that("observed-only filtering matches within the correct group", {
  d <- slice_by_data()
  ds <- data_slice(d, x = evenly(x, n = 3), .by = fac, .observed_only = TRUE)
  # A's interpolated 5 exists in B, but is not observed in A.
  expect_equal(ds$x[ds$fac == "A"], c(0, 10))
  expect_equal(ds$x[ds$fac == "B"], c(5, 20))
  expect_equal(ds, data_slice(d, x = evenly(x, n = 3), .by = fac,
    .observed_only = "x"))
  expect_equal(nrow(data_slice(d, x = evenly(x, n = 3), .by = fac,
    .observed_only = "fac")), 6L)
})

test_that("NULL grouping retains the original full grid", {
  d <- slice_by_data()
  ds <- data_slice(d, x = evenly(x, n = 3), fac = evenly(fac))
  expect_identical(ds, data_slice(d, x = evenly(x, n = 3),
    fac = evenly(fac), .by = NULL))
  expect_equal(ds$x, rep(c(0, 10, 20), each = 3))
  expect_equal(nrow(ds), 9L)
})

test_that("grouped evaluation errors identify the group", {
  d <- slice_by_data()
  d$x[d$fac == "B"] <- NA_real_
  expect_error(suppressWarnings(data_slice(d, x = evenly(x), .by = fac)),
    "fac = B")
})

test_that("GAM slices recover grouping columns and respect supplied data", {
  d <- data.frame(x = c(seq(0, 10, length.out = 30), seq(5, 20, length.out = 30)),
    fac = factor(rep(c("A", "B"), each = 30)), z = seq_len(60))
  d$y <- sin(d$x) + d$z / 20
  m <- mgcv::gam(y ~ fac + s(x, by = fac, k = 4) + z, data = d)
  ds <- data_slice(m, x = evenly(x, n = 5), .by = fac)
  expect_equal(ds$x[ds$fac == "A"], seq(0, 10, length.out = 5))
  expect_equal(ds$x[ds$fac == "B"], seq(5, 20, length.out = 5))
  expect_equal(ds$z, rep(typical_values(m)$z, 10))
  expect_length(predict(m, newdata = ds), 10L)
  expect_true(all(is.finite(predict(m, newdata = ds))))
  expect_identical(ds, data_slice(m, x = evenly(x, n = 5), .by = where(is.factor)))
  expect_equal(nrow(data_slice(m, .by = fac)), 2L)

  ref <- d
  ref$x <- ref$x + 100
  ds <- data_slice(m, x = evenly(x, n = 3), .by = fac, data = ref)
  expect_equal(ds$x, c(100, 105, 110, 105, 112.5, 120))
  ds <- data_slice(m, x = evenly(x, n = 3), fac = level(fac, "B"), .by = fac)
  expect_equal(ds$x, c(5, 12.5, 20))
  expect_identical(data_slice(m, x = evenly(x)),
    data_slice(m, x = evenly(x), .by = NULL))
})

test_that("multiple slice expressions expand independently within groups", {
  d <- slice_by_data()
  ds <- data_slice(d, x = evenly(x, n = 3), z = range(z), .by = fac)
  expect_equal(nrow(ds), 12L)
  expect_equal(ds$x[ds$fac == "A"], rep(c(0, 5, 10), each = 2))
  expect_equal(ds$z[ds$fac == "A"], rep(c(1, 3), 3))
  expect_equal(ds$z[ds$fac == "B"], rep(c(7, 9), 3))
})

test_that("grouped GAM slices recover raw transformed covariates", {
  d <- data.frame(x = c(seq(1, 10, length.out = 30), seq(5, 20, length.out = 30)),
    fac = factor(rep(c("A", "B"), each = 30)))
  d$y <- sin(log(d$x))
  m <- mgcv::gam(y ~ fac + s(log(x), by = fac, k = 4), data = d)
  ds <- data_slice(m, x = evenly(x, n = 3), .by = fac, envir = environment())
  expect_equal(ds$x, c(1, 5.5, 10, 5, 12.5, 20))
  expect_true(all(is.finite(predict(m, newdata = ds))))
  expect_identical(ds, data_slice(m, x = evenly(x, n = 3), .by = fac, data = d))
  expect_identical(ds, data_slice(structure(list(gam = m), class = "gamm"),
    x = evenly(x, n = 3), .by = fac, envir = environment()))
})

test_that("GAM observed-only filtering is scoped to groups", {
  d <- data.frame(x = c(0, 4, 10, 5, 12, 20),
    fac = factor(rep(c("A", "B"), each = 3)), y = c(1, 2, 3, 5, 7, 8))
  m <- mgcv::gam(y ~ fac + x, data = d)
  ds <- data_slice(m, x = evenly(x, n = 3), .by = fac, .observed_only = TRUE)
  expect_equal(ds$x[ds$fac == "A"], c(0, 10))
  expect_equal(ds$x[ds$fac == "B"], c(5, 20))
  expect_identical(ds, data_slice(m, x = evenly(x, n = 3), .by = fac,
    .observed_only = "x"))
})
