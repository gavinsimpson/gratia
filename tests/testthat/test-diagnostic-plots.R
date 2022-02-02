## Test qq_plot() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")

test_that("appraise() works", {
    plt <- appraise(m_gam)
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
    set.seed(1)
    expect_silent(plt <- worm_plot(m_gam))
    expect_doppelganger("worm plot gam uniform", plt)

    set.seed(1)
    expect_silent(plt <- worm_plot(m_gam, method = "simulate"))
    expect_doppelganger("worm plot gam simulate", plt)

    set.seed(1)
    expect_silent(plt <- worm_plot(m_gam, method = "normal"))
    expect_doppelganger("worm plot gam normal", plt)
})

test_that("worm_plot works for a GLM", {
    set.seed(1)
    expect_silent(plt <- worm_plot(m_glm))
    expect_doppelganger("worm plot glm uniform", plt)

    set.seed(1)
    expect_silent(plt <- worm_plot(m_glm, method = "simulate"))
    expect_doppelganger("worm plot glm simulate", plt)

    set.seed(1)
    expect_silent(plt <- worm_plot(m_glm, method = "normal"))
    expect_doppelganger("worm plot glm normal", plt)
})

test_that("worm_plot works for a LM", {
    set.seed(1)
    expect_silent(plt <- worm_plot(m_glm))
    expect_doppelganger("worm plot lm uniform", plt)

    set.seed(1)
    expect_silent(plt <- worm_plot(m_glm, method = "simulate"))
    expect_doppelganger("worm plot lm simulate", plt)

    set.seed(1)
    expect_silent(plt <- worm_plot(m_glm, method = "normal"))
    expect_doppelganger("worm plot lm normal", plt)
})

test_that("qq_plot works for a GLM", {
    set.seed(1)
    expect_silent(plt <- qq_plot(m_glm))
    expect_doppelganger("qq plot glm uniform", plt)

    set.seed(1)
    expect_silent(plt <- qq_plot(m_glm, method = "simulate"))
    expect_doppelganger("qq plot glm simulate", plt)

    set.seed(1)
    expect_silent(plt <- qq_plot(m_glm, method = "normal"))
    expect_doppelganger("qq plot glm normal", plt)
})

test_that("qq_plot works for a LM", {
    set.seed(1)
    expect_silent(plt <- qq_plot(m_lm))
    expect_doppelganger("qq plot lm uniform", plt)

    set.seed(1)
    expect_silent(plt <- qq_plot(m_lm, method = "simulate"))
    expect_doppelganger("qq plot lm simulate", plt)

    set.seed(1)
    expect_silent(plt <- qq_plot(m_lm, method = "normal"))
    expect_doppelganger("qq plot lm normal", plt)
})
