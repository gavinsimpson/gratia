## Test qq_plot() methods

test_that("appraise() works", {
  plt <- with_seed(
    3476, # seed
    appraise(m_tiny_eg1, method = "simulate",
      ci_col = "red", ci_alpha = 0.3)
  )

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("appraise diagnostic plots", plt)
})

test_that("appraise() method direct yields a message", {
  expect_message(
    plt <- appraise(m_gam, method = "direct"),
    "`method = \"direct\"` is deprecated, use `\"uniform\"`"
  )
})

test_that("appraise() fails if n_bins not numeric or one of character options", {
  msg <- paste(
    "'arg' should be one of",
    paste(dQuote(c("sturges", "scott", "fd")), collapse = ", ")
  )
  expect_error(appraise(m_gam, n_bins = "foo"), msg, fixed = TRUE)
  msg <- paste(
    "'n_bins' should be a number or one of:",
    paste(dQuote(c("sturges", "scott", "fd")), collapse = ", ")
  )
  expect_error(appraise(m_gam, n_bins = TRUE), msg, fixed = TRUE)
})

test_that("residuals_hist_plot fails if non-numeric n_bins doesn't match character options", {
  msg <- paste(
    "'arg' should be one of",
    paste(dQuote(c("sturges", "scott", "fd")), collapse = ", ")
  )
  expect_error(residuals_hist_plot(m_gam, n_bins = "foo"), msg, fixed = TRUE)
  msg <- paste(
    "'n_bins' should be a number or one of:",
    paste(dQuote(c("sturges", "scott", "fd")), collapse = ", ")
  )
  expect_error(residuals_hist_plot(m_gam, n_bins = TRUE), msg, fixed = TRUE)
})

test_that("worm_plot works for a GAM", {
  expect_silent(plt1 <- withr::with_seed(1, worm_plot(m_tiny_eg1)))

  expect_silent(plt2 <- withr::with_seed(1, worm_plot(m_tiny_eg1,
    method = "simulate"
  )))

  expect_silent(plt3 <- withr::with_seed(1, worm_plot(m_tiny_eg1,
    method = "normal"
  )))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm plot gam uniform", plt1)
  expect_doppelganger("worm plot gam simulate", plt2)
  expect_doppelganger("worm plot gam normal", plt3)
})

test_that("worm_plot works for a GLM", {
  expect_silent(plt1 <- withr::with_seed(1, worm_plot(m_glm)))

  expect_silent(plt2 <- withr::with_seed(1, worm_plot(m_glm,
    method = "simulate"
  )))

  expect_silent(plt3 <- withr::with_seed(1, worm_plot(m_glm,
    method = "normal"
  )))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm plot glm uniform", plt1)
  expect_doppelganger("worm plot glm simulate", plt2)
  expect_doppelganger("worm plot glm normal", plt3)
})

test_that("worm_plot works for a LM", {
  expect_silent(plt1 <- withr::with_seed(1, worm_plot(m_lm)))

  expect_silent(plt2 <- withr::with_seed(1, worm_plot(m_lm,
    method = "simulate"
  )))

  expect_silent(plt3 <- withr::with_seed(1, worm_plot(m_lm,
    method = "normal"
  )))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm plot lm uniform", plt1)
  expect_doppelganger("worm plot lm simulate", plt2)
  expect_doppelganger("worm plot lm normal", plt3)
})

test_that("qq_plot works for a GLM", {
  expect_silent(plt1 <- withr::with_seed(1, qq_plot(m_glm)))

  expect_silent(plt2 <- withr::with_seed(1, qq_plot(m_glm,
    method = "simulate"
  )))

  expect_silent(plt3 <- withr::with_seed(1, qq_plot(m_glm,
    method = "normal"
  )))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("qq plot glm uniform", plt1)
  expect_doppelganger("qq plot glm simulate", plt2)
  expect_doppelganger("qq plot glm normal", plt3)
})

test_that("qq_plot works for a LM", {
  expect_silent(plt1 <- withr::with_seed(1, qq_plot(m_lm)))

  expect_silent(plt2 <- withr::with_seed(1, qq_plot(m_lm,
    method = "simulate"
  )))

  expect_silent(plt3 <- withr::with_seed(1, qq_plot(m_lm,
    method = "normal"
  )))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("qq plot lm uniform", plt1)
  expect_doppelganger("qq plot lm simulate", plt2)
  expect_doppelganger("qq plot lm normal", plt3)
})

test_that("appraise works for a LM", {
  expect_silent(plt <- withr::with_seed(1, appraise(m_lm)))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("appraise lm ", plt)
})

test_that("appraise can use the worm plot", {
  expect_silent(plt <- withr::with_seed(1, appraise(m_tiny_eg1,
    use_worm = TRUE,
    method = "simulate"
  )))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("appraise worm plot", plt)
})

test_that("appraise handles mvn models", {
  skip_on_cran()
  expect_silent(
    plt <- withr::with_seed(1, appraise(m_mvn, method = "simulate"))
  )

  expect_doppelganger(
    "appraise for mvn model", plt
  )
})

test_that("appraise handles twlss models", {
  skip_on_cran()
  expect_silent(
    plt <- withr::with_seed(1, appraise(m_twlss, method = "simulate"))
  )

  expect_doppelganger(
    "appraise for twlss model", plt
  )
})
