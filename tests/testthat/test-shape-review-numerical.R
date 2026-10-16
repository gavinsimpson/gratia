test_that("flat hull segments remain exactly flat after interpolation", {
  for (height in c(0.1, 0.3, 1.3, 1e150)) {
    x <- c(0, 0.1, 0.17, 0.2, 0.3, 0.86, 1)
    y <- rep(2 * height, length(x))
    y[c(1L, length(x))] <- height
    expect_identical(shape_convex_minorant(x, y), rep(height, length(x)))
  }
})

test_that("hull interpolation orders separately transformed endpoints", {
  withr::with_seed(941, {
    x <- sort(c(0, runif(8), 1))
    for (scale in c(1e-150, 1, 1e150)) {
      for (i in seq_len(30)) {
        lower <- scale * rnorm(length(x))
        upper <- lower + scale * rexp(length(x))
        lo <- shape_convex_minorant(x, lower)
        hi <- shape_convex_minorant(x, upper)
        expect_true(all(lo <= hi))
        expect_true(all(is.finite(lo)))
        expect_true(all(is.finite(hi)))
      }
    }
  })
})
