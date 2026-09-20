test_that("rtw aligns vector parameters with Poisson events", {
  withr::local_seed(42)
  reference <- function(mu, p, phi) {
    size <- max(length(mu), length(p), length(phi))
    mu <- rep_len(mu, size)
    p <- rep_len(p, size)
    phi <- rep_len(phi, size)
    counts <- rpois(size, mu^(2 - p) / ((2 - p) * phi))
    owner <- rep(seq_len(size), counts)
    events <- rgamma(length(owner),
      shape = ((2 - p) / (p - 1))[owner],
      scale = (phi * (p - 1) * mu^(p - 1))[owner]
    )
    vapply(seq_len(size), function(i) sum(events[owner == i]), numeric(1))
  }
  for (pars in list(
    list(mu = c(0, 2, 3, 0, 1), p = c(1.2, 1.8, 1.3, 1.6, 1.9), phi = 1),
    list(mu = c(0, 1, 2, 3), p = 1.5, phi = c(0.5, 1, 2, 3)),
    list(mu = 2, p = c(1.2, 1.8), phi = 1)
  )) {
    set.seed(42)
    expected <- do.call(reference, pars)
    set.seed(42)
    got <- do.call(rtw, pars)
    expect_equal(got, expected, tolerance = 1e-12)
  }
  expect_equal(rtw(c(0, 0), c(1.2, 1.8), 1), c(0, 0))
  expect_equal(rtw(numeric(), numeric(), numeric()), numeric())
  expect_error(rtw(1:3, c(1.2, 1.8), 1), "common nonzero length")
  expect_error(rtw(numeric(), 1.5, 1), "common nonzero length")
  expect_error(rtw(2, NA_real_, 1), "finite numeric")
  expect_error(rtw(2, 2, 1), "interval")
  # Distributional check independent of the event-level reference.
  p <- rep(c(1.2, 1.8), each = 100000)
  set.seed(123)
  y <- rtw(2, p, 1)
  for (power in c(1.2, 1.8)) {
    values <- y[p == power]
    expect_lt(abs(mean(values) - 2), 6 * sqrt(2^power / length(values)))
  }
})
