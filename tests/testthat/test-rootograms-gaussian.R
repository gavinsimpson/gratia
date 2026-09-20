test_that("rootograms gaussian works", {
  withr::local_seed(42)
  d <- data.frame(x = runif(300), w = rep(c(0.5, 1, 2), 100))
  for (dispersion in c(0.16, 4)) {
    for (link in c("identity", "log")) {
      for (weighted in c(FALSE, TRUE)) {
        d$weight <- if (weighted) d$w else 1
        eta <- 1 + d$x
        mu <- if (link == "log") exp(eta) else eta
        d$y <- mu + rnorm(nrow(d), sd = sqrt(dispersion / d$weight))
        m <- gam(y ~ x,
          family = gaussian(link), weights = weight,
          data = d, method = "REML"
        )
        breaks <- seq(min(d$y) - 1, max(d$y) + 1, length.out = 12)
        got <- rootogram(m, breaks = breaks)
        # Independently integrate the fitted observation densities per bin.
        means <- fitted(m)
        sds <- sqrt(summary(m)$scale / d$weight)
        expected <- vapply(seq_len(length(breaks) - 1L), function(j) {
          integrate(function(q) {
            vapply(q, function(v) sum(dnorm(v, means, sds)), numeric(1))
          }, lower = breaks[j], upper = breaks[j + 1L])$value
        }, numeric(1))
        expect_equal(unname(got$.fitted), expected, tolerance = 1e-7)
        expect_equal(got$.observed, hist(d$y, breaks, plot = FALSE)$counts)
        expect_equal(attr(got, "sigma"), sds)
      }
    }
  }
  # Unweighted model: model.weights() returns NULL.
  m <- gam(y ~ x, data = d, method = "REML")
  expect_equal(attr(rootogram(m), "sigma"), rep(sqrt(m$sig2), nrow(d)))
  # Reject zero precision rather than silently treating it as a valid density.
  d$weight[1] <- 0
  m <- gam(y ~ x, weights = weight, data = d, method = "REML")
  expect_error(rootogram(m), "positive prior weights")
})
