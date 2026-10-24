# Test evenly

test_that("evenly with by works", {
  expect_silent(x <- evenly(1987:1999, by = 2))
  expect_identical(length(x), 7L)
  expect_identical(seq(1987, 1999, by = 2), x)


  expect_silent(x <- evenly(1987:1999, by = 2, lower = 1990))
  expect_identical(length(x), 5L)
  expect_identical(seq(1990, 1999, by = 2), x)


  expect_silent(x <- evenly(1987:1999, by = 2, upper = 1997))
  expect_identical(length(x), 6L)
  expect_identical(seq(1987, 1997, by = 2), x)
})

test_that("seq min max with by works", {
  expect_silent(x <- seq_min_max(1987:1999, by = 2))
  expect_identical(length(x), 7L)
  expect_identical(seq(1987, 1999, by = 2), x)


  expect_silent(x <- seq_min_max(1987:1999, by = 2, lower = 1990))
  expect_identical(length(x), 5L)
  expect_identical(seq(1990, 1999, by = 2), x)


  expect_silent(x <- seq_min_max(1987:1999, by = 2, upper = 1997))
  expect_identical(length(x), 6L)
  expect_identical(seq(1987, 1997, by = 2), x)
})

test_that("ref_level works", {
  f <- factor(sample(letters[1:5], 100, replace = TRUE),
    levels = letters[1:5]
  )
  expect_silent(rl <- ref_level(f))
  expect_identical(factor("a", levels = letters[1:5]), rl)

  expect_error(ref_level(1:10), "'fct' must be a factor")
})

test_that("ref_level works", {
  f <- factor(sample(letters[1:5], 100, replace = TRUE),
    levels = letters[1:5]
  )
  expect_silent(rl <- level(f, "b"))
  expect_identical(factor("b", levels = letters[1:5]), rl)

  expect_error(level(1:10), "'fct' must be a factor")
})

test_that("factor level helpers preserve ordered status, levels and values", {
  for (ordered in c(FALSE, TRUE)) {
    f <- factor(c("middle", "low"), levels = c("high", "middle", "low"),
      ordered = ordered)
    expect_identical(ref_level(f),
      factor("high", levels = levels(f), ordered = ordered))
    expect_identical(level(f, "low"),
      factor("low", levels = levels(f), ordered = ordered))
    expect_identical(level(f, c("low", "high")),
      factor(c("low", "high"), levels = levels(f), ordered = ordered))
    expect_identical(class(ref_level(f)), class(f))
    expect_identical(class(level(f, "low")), class(f))
    expect_identical(levels(ref_level(f)), levels(f))
    expect_identical(levels(level(f, "low")), levels(f))
    expect_error(level(f, "unknown"), "not a valid level of factor")
  }
  expect_error(ref_level(1:3), "'fct' must be a factor", fixed = TRUE)
  expect_error(level(1:3, "a"), "'fct' must be a factor", fixed = TRUE)
})
