test_that("grid containment does not imply coverage between nodes", {
  # The exact node band covers the convex truth x^2 at both nodes.
  # Its straight-line interpolant is above that truth in the interior.
  grid <- data.frame(x = c(0, 1))
  band <- shape_intervals(c(0, 1), c(0, 1), c(0, 1), grid,
    constraints = list(x = c("m+", "c+")))
  midpoint_lower <- approx(grid$x, band$.lower_ci, xout = 0.5)$y
  expect_equal(midpoint_lower, 0.5)
  expect_gt(midpoint_lower, 0.5^2)
  expect_identical(attr(band, "interval_info")$domain, "evaluation grid")
})

test_that("intersecting a band preserves its event for feasible coefficients", {
  # Two masses sum to one; X evaluates their linear combination at three
  # locations. The truth is feasible for every region in this experiment.
  truth <- c(0.25, 0.75)
  X <- rbind(c(1, 0), c(0.5, 0.5), c(0, 1))
  values <- c(0.25, 0.5, 0.75)
  boxes <- list(
    list(lower = c(0, 0, 0), upper = c(1, 1, 1), covers = TRUE),
    list(lower = values, upper = values, covers = TRUE),
    list(lower = c(0, 0.6, 0), upper = c(1, 1, 1), covers = FALSE),
    list(lower = c(0, 0, 0), upper = c(1, 1, 0.7), covers = FALSE)
  )
  for (box in boxes) {
    region <- shape_confidence_set(X, box$lower, box$upper,
      Ain = diag(2), C = matrix(c(1, 1), nrow = 1), d = 1,
      band_type = "simultaneous")
    expect_identical(unname(in_shape_set(region, truth, tolerance = 0)),
      box$covers)
  }
})
