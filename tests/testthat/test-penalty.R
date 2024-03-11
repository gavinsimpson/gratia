# Test penalty()

# Uses this model, now in setup.R
# dat <- data_sim("eg4", n = 400, seed = 42)
# su_m_su_eg4 <- gam(y ~ s(x0) + s(x1) + s(x2, by = fac),
#   data = dat, method = "REML")

# reference names
pen_nms <- c(".smooth", ".type", ".penalty", ".row", ".col", ".value")

test_that("penalty() works with a simple GAM", {
  expect_silent(p <- penalty(su_m_su_eg4))
  expect_s3_class(p, "penalty_df")
  expect_named(p, pen_nms)
})

test_that("penalty() resclaing works with a simple GAM", {
  expect_silent(p <- penalty(su_m_su_eg4, rescale = TRUE))
  expect_s3_class(p, "penalty_df")
  expect_named(p, pen_nms)
})

test_that("penalty() works with a factor by smooth", {
  expect_silent(p <- penalty(su_m_su_eg4, select = "s(x2):fac2"))
  expect_s3_class(p, "penalty_df")
  expect_named(p, pen_nms)
})

test_that("penalty() rescaling works with a factor by smooth", {
  expect_silent(p <- penalty(su_m_su_eg4,
    select = "s(x2):fac2",
    rescale = TRUE
  ))
  expect_s3_class(p, "penalty_df")
  expect_named(p, pen_nms)
})

test_that("smooth arg of penalty() is properly deprecated", {
  expect_warning(p <- penalty(su_m_su_eg4, smooth = "s(x2)",
    partial_match = TRUE), "deprecated")
})

## draw.penalty
test_that("draw.penalty_df works", {
  skip_on_cran()
  skip_on_ci()

  expect_silent(pen <- penalty(su_m_univar_4))
  plt1 <- draw(pen)
  plt2 <- draw(penalty(su_m_univar_4, "s(x1)"))

  skip_on_ci()
  expect_doppelganger("draw penalty_df with multiple smooths", plt1)
  expect_doppelganger("draw penalty_df with single smooths", plt2)
})

test_that("draw.penalty_df gets labels on plot in correct order issue 95", {
  skip_on_cran()
  skip_on_ci()

  expect_silent(pen <- penalty(su_m_penalty))
  plt <- draw(pen)

  skip_on_ci()
  expect_doppelganger("draw penalty_df issue 95 label order", plt)
})

test_that("draw.penalty_df accepts user-specified continuous_fill", {
  skip_on_cran()
  skip_on_ci()

  expect_silent(pen <- penalty(su_m_univar_4))
  plt1 <- draw(pen,
    continuous_fill = scale_fill_distiller(
      palette = "Spectral",
      type = "div"
    )
  )

  plt2 <- draw(penalty(su_m_univar_4, "s(x1)"),
    continuous_fill = scale_fill_distiller(
      palette = "Spectral",
      type = "div"
    )
  )

  skip_on_ci()
  expect_doppelganger(
    "draw penalty multiple smooths user continuous fill",
    plt1
  )
  expect_doppelganger(
    "draw penalty single smooths user continuous fill",
    plt2
  )
})

test_that("draw.penalty_df works with normalization", {
  skip_on_cran()
  skip_on_ci()

  expect_silent(pen <- penalty(su_m_univar_4))
  plt1 <- draw(pen, normalize = TRUE)
  plt2 <- draw(penalty(su_m_univar_4, "s(x1)"), normalize = TRUE)

  skip_on_ci()
  expect_doppelganger(
    "draw penalty_df with multiple smooths normalized",
    plt1
  )
  expect_doppelganger(
    "draw penalty_df with single smooths normalized",
    plt2
  )
})

test_that("draw.penalty_df gets labels on plot in correct order as matrix", {
  skip_on_cran()

  expect_silent(pen <- penalty(su_m_penalty))
  plt1 <- draw(pen, as_matrix = TRUE)
  plt2 <- draw(pen, as_matrix = FALSE)

  skip_on_ci()
  expect_doppelganger("draw penalty_df as matrix true", plt1)
  expect_doppelganger("draw penalty_df as matrix false", plt2)
})
