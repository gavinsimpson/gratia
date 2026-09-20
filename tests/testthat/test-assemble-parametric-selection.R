test_that("assemble and draw respect parametric term selection", {
  withr::local_seed(42)
  d <- data.frame(x = runif(200), z = runif(200), w = runif(200))
  d$y <- sin(6 * d$x) + d$z - d$w + rnorm(200, sd = 0.3)
  m <- gam(y ~ s(x, k = 6) + z + w, data = d, method = "REML")
  for (term in c("z", "w")) {
    assembled <- assemble(m, data = d, parametric = TRUE, terms = term)
    expect_setequal(names(assembled), c("s(x)", term))
    expect_equal(unique(assembled[[term]]$data$.term), term)
    drawn <- draw(m, data = d, parametric = TRUE, terms = term)
    expect_length(drawn, 2L)
    plot_terms <- unlist(lapply(seq_len(length(drawn)), function(i) {
      unique(drawn[[i]]$data$.term)
    }), use.names = FALSE)
    expect_setequal(plot_terms, c("s(x)", term))
  }
  expect_setequal(
    names(assemble(m, data = d, parametric = TRUE)), c("s(x)", "z", "w")
  )
  expect_setequal(
    names(assemble(m, data = d, parametric = TRUE, terms = c("z", "w"))),
    c("s(x)", "z", "w")
  )
})
