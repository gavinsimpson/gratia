# Test parametric_effects() method

lev_nms <- c(".level", ".partial", ".se")
val_nms <- c(".value", ".partial", ".se")
both_nms <- c(".level", ".value", ".partial", ".se")

test_that("parametric_effects works for m_2_fac", {
  expect_message(
    peff <- parametric_effects(m_2_fac, envir = teardown_env(), data = df_2_fac),
    "Interaction terms are not currently supported."
  )
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 5L)
  expect_identical(nrow(peff), 7L)
  expect_named(peff, c("term", "type", lev_nms))
})

test_that("parametric_effects works for m_para_sm", {
  expect_message(
    peff <- parametric_effects(m_para_sm, data = df_2_fac, envir = teardown_env()),
    "Interaction terms are not currently supported."
  )
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 6L)
  expect_identical(nrow(peff), 407L)
  expect_named(peff, c("term", "type", both_nms))
})

test_that("parametric_effects works for m_2_fac select term", {
  expect_silent(peff <- parametric_effects(m_2_fac,
    term = "fac", data = df_2_fac,
    envir = teardown_env()
  ))
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 5L)
  expect_identical(nrow(peff), 3L)
  expect_named(peff, c("term", "type", lev_nms))
})

test_that("parametric_effects works for m_para_sm select term", {
  expect_silent(peff <- parametric_effects(m_para_sm,
    term = "fac",
    data = df_2_fac, envir = teardown_env()
  ))
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 5L)
  expect_identical(nrow(peff), 3L)
  expect_named(peff, c("term", "type", lev_nms))
})

test_that("parametric_effects works with only parametric terms", {
  expect_message(
    peff <- parametric_effects(m_only_para,
      data = df_2_fac,
      envir = teardown_env()
    ),
    "Interaction terms are not currently supported."
  )
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 6L)
  expect_identical(nrow(peff), 1207L)
  expect_named(peff, c("term", "type", both_nms))
})

test_that("parametric_effects works with weird parametric terms", {
  expect_silent(peff <- parametric_effects(m_poly,
    data = df_2_fac,
    envir = teardown_env()
  ))
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 6L)
  expect_identical(nrow(peff), 1207L)
  expect_named(peff, c("term", "type", both_nms))

  expect_silent(peff <- parametric_effects(m_poly,
    transform = TRUE,
    data = df_2_fac, envir = teardown_env()
  ))
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 6L)
  expect_identical(nrow(peff), 1207L)
  expect_named(peff, c("term", "type", both_nms))
})

test_that("issue 212 remains fixed", {
  skip_on_cran()
  # issue #212 is about weird model terms
  data_212 <- quick_eg1 |>
    mutate(fac = factor(rep(c("a", "b"), c(200, 100))))
  m_212 <- gam(y ~ fac + poly(x0, 2, raw = TRUE) +
    poly(x1, 2, raw = TRUE) +
    poly(x2, 2, raw = TRUE) +
    poly(x3, 2, raw = TRUE), data = data_212)
  expect_silent(peff <- parametric_effects(m_212,
    data = data_212,
    envir = teardown_env()
  ))
  expect_s3_class(peff, class = c(
    "parametric_effects", "tbl_df", "tbl",
    "data.frame"
  ))
  expect_identical(ncol(peff), 6L)
  expect_identical(nrow(peff), 1202L)
  expect_named(peff, c("term", "type", both_nms))

  plt <- draw(peff, rug = FALSE)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("testing issue 212", plt)
})
