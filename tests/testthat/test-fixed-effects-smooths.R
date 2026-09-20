test_that("fixed_effects() excludes smooth coefficients by index", {
  withr::local_seed(42)
  d <- as.data.frame(matrix(runif(400 * 8), ncol = 8))
  names(d) <- c("x", "z", "u", "v", "w", "q", "r", "linear")
  d$f <- factor(rep(c("a", "b"), 200))
  d$y <- sin(6 * d$x) + d$linear + rnorm(400, sd = 0.3)
  models <- list(
    gam(
      y ~ linear + te(x, z, k = c(4, 4)) +
        ti(u, v, k = c(4, 4)) + t2(w, q, k = c(4, 4)) + s(r, k = 5),
      data = d, method = "REML"
    ),
    gam(y ~ linear + f + s(x, by = f, k = 5), data = d, method = "REML"),
    gam(y ~ 0 + linear + te(x, z, k = c(4, 4)), data = d, method = "REML"),
    gam(y ~ linear, data = d),
    gam(y ~ 0 + s(x, k = 5), data = d, method = "REML")
  )
  for (m in models) {
    # summary.gam's parametric coefficient table is an independent reference.
    table <- summary(m)$p.table
    expected <- setNames(as.numeric(table[, 1]), rownames(table))
    if (!length(expected)) names(expected) <- character()
    expect_equal(fixed_effects(m), expected)
  }
  expect_length(fixed_effects(models[[1]]), 2L)
  expect_length(fixed_effects(models[[5]]), 0L)

  # Extraction must not depend on smooth coefficient labels.
  m <- models[[1]]
  idx <- unlist(lapply(m$smooth, function(sm) {
    seq.int(sm$first.para, sm$last.para)
  }))
  names(m$coefficients)[idx] <- paste0("mgcv::smooth", seq_along(idx))
  expect_equal(fixed_effects(m), fixed_effects(models[[1]]))
})
