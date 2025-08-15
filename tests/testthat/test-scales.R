# test the scale functions provided by gratia

test_that("the partial effect fill scale works", {
  plt <- su_m_bivar_te |>
    smooth_estimates(n = 30) |>
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
