## Test qq_plot() methods

test_that("appraise() works", {
    plt <- appraise(m_gam)
    expect_doppelganger("appraise diagnostic plots", plt)
})

test_that("appraise() method direct yields a message", {
    expect_message(plt <- appraise(m_gam, method = "direct"),
                   "`method = \"direct\"` is deprecated, use `\"uniform\"`")
    expect_doppelganger("appraise diagnostic plots", plt)
})

test_that("appraise() fails if n_bins not numeric or one of character options",
{
    msg <- paste("'arg' should be one of",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(appraise(m_gam, n_bins = "foo"), msg, fixed = TRUE)
    msg <- paste("'n_bins' should be a number or one of:",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(appraise(m_gam, n_bins = TRUE), msg, fixed = TRUE)
})

test_that("residuals_hist_plot fails if non-numeric n_bins doesn't match character options", {
    msg <- paste("'arg' should be one of",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(residuals_hist_plot(m_gam, n_bins = "foo"), msg, fixed = TRUE)
    msg <- paste("'n_bins' should be a number or one of:",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(residuals_hist_plot(m_gam, n_bins = TRUE), msg, fixed = TRUE)
})

test_that("worm_plot works for a GAM", {
    expect_silent(plt <- withr::with_seed(1, worm_plot(m_gam)))
    expect_doppelganger("worm plot gam uniform", plt)

    expect_silent(plt <- withr::with_seed(1, worm_plot(m_gam,
        method = "simulate")))
    expect_doppelganger("worm plot gam simulate", plt)

    expect_silent(plt <- withr::with_seed(1, worm_plot(m_gam,
        method = "normal")))
    expect_doppelganger("worm plot gam normal", plt)
})

test_that("worm_plot works for a GLM", {
    expect_silent(plt <- withr::with_seed(1, worm_plot(m_glm)))
    expect_doppelganger("worm plot glm uniform", plt)

    expect_silent(plt <- withr::with_seed(1, worm_plot(m_glm,
        method = "simulate")))
    expect_doppelganger("worm plot glm simulate", plt)

    expect_silent(plt <- withr::with_seed(1, worm_plot(m_glm,
        method = "normal")))
    expect_doppelganger("worm plot glm normal", plt)
})

test_that("worm_plot works for a LM", {
    expect_silent(plt <- withr::with_seed(1, worm_plot(m_lm)))
    expect_doppelganger("worm plot lm uniform", plt)

    expect_silent(plt <- withr::with_seed(1, worm_plot(m_lm,
        method = "simulate")))
    expect_doppelganger("worm plot lm simulate", plt)

    expect_silent(plt <- withr::with_seed(1, worm_plot(m_lm,
        method = "normal")))
    expect_doppelganger("worm plot lm normal", plt)
})

test_that("qq_plot works for a GLM", {
    expect_silent(plt <- withr::with_seed(1, qq_plot(m_glm)))
    expect_doppelganger("qq plot glm uniform", plt)

    expect_silent(plt <- withr::with_seed(1, qq_plot(m_glm,
        method = "simulate")))
    expect_doppelganger("qq plot glm simulate", plt)

    expect_silent(plt <- withr::with_seed(1, qq_plot(m_glm,
        method = "normal")))
    expect_doppelganger("qq plot glm normal", plt)
})

test_that("qq_plot works for a LM", {
    expect_silent(plt <- withr::with_seed(1, qq_plot(m_lm)))
    expect_doppelganger("qq plot lm uniform", plt)

    expect_silent(plt <- withr::with_seed(1, qq_plot(m_lm,
        method = "simulate")))
    expect_doppelganger("qq plot lm simulate", plt)

    expect_silent(plt <- withr::with_seed(1, qq_plot(m_lm,
        method = "normal")))
    expect_doppelganger("qq plot lm normal", plt)
})

test_that("appraise works for a LM", {
    expect_silent(plt <- withr::with_seed(1, appraise(m_lm)))
    expect_doppelganger("appraise lm ", plt)
})

test_that("appraise can use the worm plot", {
    expect_silent(plt <- withr::with_seed(1, appraise(m_gam, use_worm = TRUE,
        method = "simulate")))
    expect_doppelganger("appraise worm plot", plt)
})