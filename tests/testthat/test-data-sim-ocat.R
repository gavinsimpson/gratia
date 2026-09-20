test_that("ordinal data simulation honours and validates cut points", {
  make <- function(cuts, n_cat = length(cuts) + 1L) {
    data_sim("eg1",
      n = 200, dist = "ocat", seed = 42,
      cuts = cuts, n_cat = n_cat
    )
  }
  a <- make(c(-10, 0, 10))
  b <- make(c(-1, 0, 1))
  expect_equal(a$latent, b$latent)
  expect_false(identical(a$y, b$y))
  for (cuts in list(c(-10, 0, 10), c(-1, 0, 1), 0, c(-2, -1, 0, 1))) {
    got <- make(cuts)
    expected <- as.integer(cut(got$latent,
      breaks = c(-Inf, cuts, Inf),
      labels = FALSE, right = TRUE
    ))
    expect_equal(got$y, expected)
    expect_equal(nrow(got), 200L)
  }
  expect_equal(
    data_sim("eg1", n = 200, dist = "ocat", seed = 42),
    make(c(-1, 0, 5))
  )
  expect_error(make(c(0, 1), n_cat = 4), "Number of cut points")
  for (cuts in list(c(1, 0), c(0, 0), c(NA, 1), c(0, Inf), c("a", "b"))) {
    expect_error(make(cuts), "strictly increasing")
  }
  for (n_cat in list(1, 2.5, NA_real_, Inf, c(2, 3))) {
    expect_error(make(0, n_cat), "single integer")
  }
})
