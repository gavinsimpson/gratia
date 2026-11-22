# test the scale functions provided by gratia

test_that("the partial effect fill scale uses the requested palette direction", {
  colours <- c("#B2182B", "#F7F7F7", "#2166AC")
  scale <- scale_fill_partial_effect(limits = c(-1, 1), direction = 1)
  expect_equal(scale$map(c(-1, 0, 1)), colours)
})

test_that("the partial effect fill scale works", {
  skip_on_cran()
  plt <- su_m_bivar_te |>
    smooth_estimates(n_2d = 30) |>
    ggplot(
      aes(
        x = x, y = z
      )
    ) +
    geom_raster(
      aes(
        fill = .estimate
      )
    ) +
    scale_fill_partial_effect()

  expect_doppelganger("partial effect fill scale", plt)
})
