test_that("sets preserve equality and inequality restrictions jointly", {
  region <- shape_confidence_set(
    diag(2), c(0.1, 0.2), c(0.8, 0.9), Ain = diag(2),
    C = matrix(c(1, 1), nrow = 1), d = 1,
    band_type = "simultaneous"
  )
  expect_s3_class(region, "shape_confidence_set")
  expect_identical(region$interpretation, "confidence")
  expect_identical(region$band_type, "simultaneous")
  expect_equal(region$level, 0.95)
  expect_true(in_shape_set(region, c(0.4, 0.6)))
  expect_false(in_shape_set(region, c(0.4, 0.7)))
  expect_false(in_shape_set(region, c(0, 1)))
  # Separate extrema obey the interval but violate the equality.
  expect_false(in_shape_set(region, region$lower))
  expect_false(in_shape_set(region, region$upper))
  candidates <- cbind(valid = c(0.4, 0.6), invalid = c(0.4, 0.7))
  expect_identical(in_shape_set(region, candidates), c(valid = TRUE, invalid = FALSE))
})

test_that("coupled constraints agree with direct calculations", {
  X <- rbind(c(1, 0, 0), c(1, 1, 1), c(1, 2, 4))
  Ain <- rbind(c(0, 1, 0), c(0, 1, 4), c(0, 0, 1))
  region <- shape_confidence_set(X, c(-1, -1, -1), c(2, 3, 8), Ain)
  candidates <- cbind(c(0, 0, 1), c(0, 1, -0.1), c(0, -1, 1), c(3, 0, 0))
  reference <- apply(candidates, 2, function(b) {
    all(X %*% b >= region$lower) && all(X %*% b <= region$upper) &&
      all(Ain %*% b >= 0)
  })
  expect_identical(in_shape_set(region, candidates, tolerance = 0), reference)
  expect_identical(reference, c(TRUE, FALSE, FALSE, FALSE))
})

test_that("empty and redundant matrices define sets without optimisation", {
  region <- shape_confidence_set(matrix(numeric(), 0, 2), numeric(), numeric())
  expect_equal(dim(region$Ain), c(0, 2))
  expect_equal(dim(region$C), c(0, 2))
  expect_true(in_shape_set(region, c(-100, 100)))
  expect_identical(in_shape_set(region, matrix(numeric(), 2, 0)), logical())
  singular <- shape_confidence_set(matrix(c(1, 1, 1, 1), 2), c(0, 0), c(1, 1))
  expect_true(in_shape_set(singular, c(0.4, 0.6)))
  expect_false(in_shape_set(singular, c(1, 1)))
  empty <- shape_confidence_set(diag(2), c(0, 0), c(1, 1),
    C = rbind(c(1, 0), c(1, 0)), d = c(0, 1))
  expect_false(in_shape_set(empty, c(0, 0)))
  expect_false(in_shape_set(empty, c(1, 0)))
})

test_that("infinite limits and boundary tolerances behave as specified", {
  region <- shape_confidence_set(diag(2), c(0, -Inf), c(Inf, Inf),
    Ain = matrix(c(0, 1), 1), C = matrix(c(1, -1), 1), d = 0)
  expect_true(in_shape_set(region, c(0, 0), tolerance = 0))
  expect_true(in_shape_set(region, c(-1e-9, 0), tolerance = 1e-8))
  expect_false(in_shape_set(region, c(-1e-9, 0), tolerance = 0))
  expect_false(in_shape_set(region, c(0, -1e-6), tolerance = 1e-8))
  exact <- shape_confidence_set(diag(1), 1, 1)
  expect_true(in_shape_set(exact, 1 + 1e-9))
  expect_false(in_shape_set(exact, 1 + 1e-9, tolerance = 0))
  expect_error(in_shape_set(exact, .Machine$double.xmax * 2), "finite")
  overflow <- shape_confidence_set(matrix(2, 1, 1), -Inf, Inf)
  expect_error(in_shape_set(overflow, .Machine$double.xmax), "produced non-finite")
})

test_that("set inputs are checked without recycling or dimension dropping", {
  expect_error(shape_confidence_set(1, 0, 1), "X.*matrix")
  expect_error(shape_confidence_set(matrix(NA_real_), 0, 1), "X.*finite")
  expect_error(shape_confidence_set(matrix(numeric(), 0, 0), numeric(), numeric()),
    "at least one column")
  expect_error(shape_confidence_set(diag(2), 0, c(1, 1)), "lower.*length 2")
  expect_error(shape_confidence_set(diag(1), NA_real_, 1), "lower")
  expect_error(shape_confidence_set(diag(1), 0, NaN), "upper")
  expect_error(shape_confidence_set(diag(1), 2, 1), "must not exceed")
  expect_error(shape_confidence_set(diag(1), 0, 1, Ain = matrix(1, 1, 2)), "columns")
  expect_error(shape_confidence_set(diag(1), 0, 1, Ain = matrix(Inf)), "Ain")
  expect_error(shape_confidence_set(diag(1), 0, 1, bin = 1), "bin.*length 0")
  expect_error(shape_confidence_set(diag(1), 0, 1, C = matrix(1), d = Inf), "d")
  expect_error(shape_confidence_set(diag(1), 0, 1, C = matrix(1, 1, 2)), "columns")
  for (level in list(0, 1, -1, Inf, NA_real_, c(0.8, 0.9))) {
    expect_error(shape_confidence_set(diag(1), 0, 1, level = level), "level")
  }
  expect_error(shape_confidence_set(diag(1), 0, 1, interpretation = "other"), "arg")
  expect_error(shape_confidence_set(diag(1), 0, 1, band_type = "other"), "arg")
  region <- shape_confidence_set(diag(2), c(0, 0), c(1, 1))
  expect_error(in_shape_set(list(), c(0, 0)), "object")
  expect_error(in_shape_set(region, 1), "length 2")
  expect_error(in_shape_set(region, matrix(1, 1, 2)), "one row")
  expect_error(in_shape_set(region, c(NA_real_, 0)), "coefficients")
  expect_error(in_shape_set(region, matrix("a", 2, 1)), "coefficients")
  for (tolerance in list(-1, Inf, NA_real_, c(0, 1))) {
    expect_error(in_shape_set(region, c(0, 0), tolerance), "tolerance")
  }
})
