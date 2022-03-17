# Test draw.smooth_estimates()
# A lot of the current tests for this are over in test-draw-methods.R

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")

test_that("draw.smooth_estimates works for m_1_smooth", {
    skip_on_cran()
    expect_silent(plt <- draw(smooth_estimates(m_1_smooth, "s(x0)")))
    expect_doppelganger("draw_smooth_estimates m_1_smooth", plt)
})

test_that("draw.smooth_estimates works for m_gam", {
    expect_silent(plt <- draw(smooth_estimates(m_gam, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_gam", plt)
})

test_that("draw.smooth_estimates works for m_gamm", {
    expect_silent(plt <- draw(smooth_estimates(m_gamm, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_gamm", plt)
})

test_that("draw.smooth_estimates works for m_gamm4", {
    skip_on_ci()
    skip_on_cran()
    expect_silent(plt <- draw(smooth_estimates(m_gamm4, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_gamm4", plt)
})

test_that("draw.smooth_estimates works for m_bam", {
    expect_silent(plt <- draw(smooth_estimates(m_bam, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_bam", plt)
})

test_that("draw.smooth_estimates works for m_gaulss", {
    expect_silent(plt <- draw(smooth_estimates(m_gaulss, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_gaulss", plt)
})

test_that("draw.smooth_estimates works for m_scat", {
    expect_silent(plt <- draw(smooth_estimates(m_scat, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_scat", plt)
})

test_that("draw.smooth_estimates works for m_gamgcv", {
    expect_silent(plt <- draw(smooth_estimates(m_gamgcv, "s(x2)")))
    expect_doppelganger("draw.smooth_estimates m_gamgcv", plt)
})

test_that("draw.smooth_estimates works for su_m_bivar", {
    skip_on_os(os = "mac") # trivial diffs in contours
    skip_on_os(os = "win") # trivial diffs in contours
    expect_silent(plt <- draw(smooth_estimates(su_m_bivar, dist = 0.1)))
    expect_doppelganger("draw.smooth_estimates su_m_bivar", plt)
})

test_that("draw.smooth_estimates works for su_m_bivar_te", {
    skip_on_os(os = "win") # trivial diffs in contours
    expect_silent(plt <- draw(smooth_estimates(su_m_bivar_te, dist = 0.1)))
    expect_doppelganger("draw.smooth_estimates su_m_bivar_te", plt)
})

test_that("draw.smooth_estimates works for su_m_bivar_t2", {
    expect_silent(plt <- draw(smooth_estimates(su_m_bivar_t2, dist = 0.1)))
    expect_doppelganger("draw.smooth_estimates su_m_bivar_t2", plt)
})

test_that("draw.smooth_estimates works for su_m_trivar", {
    skip_on_os(os = "win")
    skip_on_os(os = "mac")
    expect_silent(plt <- draw(smooth_estimates(su_m_trivar, dist = 0.1,
                                               n = 25, n_3d = 4)))
    expect_doppelganger("draw.smooth_estimates su_m_trivar", plt)
})

test_that("draw.smooth_estimates works for su_m_trivar_te", {
    skip_on_os(os = "win")
    skip_on_os(os = "mac")
    expect_silent(plt <- draw(smooth_estimates(su_m_trivar_te, dist = 0.1,
                                               n = 25, n_3d = 4)))
    expect_doppelganger("draw.smooth_estimates su_m_trivar_te", plt)
})

test_that("draw.smooth_estimates works for su_m_trivar_t2", {
    skip_on_os(os = "win")
    skip_on_os(os = "mac")
    expect_silent(plt <- draw(smooth_estimates(su_m_trivar_t2, dist = 0.1,
                                               n = 25, n_3d = 4)))
    expect_doppelganger("draw.smooth_estimates su_m_trivar_t2", plt)
})

test_that("draw.smooth_estimates works for su_m_quadvar", {
    skip_on_os(os = "win")
    skip_on_os(os = "mac")
    expect_silent(plt <- draw(smooth_estimates(su_m_quadvar, dist = 0.1,
                                               n = 25, n_3d = 4, n_4d = 3)))
    expect_doppelganger("draw.smooth_estimates su_m_quadvar", plt)
})

test_that("draw.smooth_estimates works for su_m_quadvar_te", {
    skip_on_os(os = "win")
    skip_on_os(os = "mac")
    expect_silent(plt <- draw(smooth_estimates(su_m_quadvar_te, dist = 0.1,
                                               n = 25, n_3d = 4, n_4d = 3)))
    expect_doppelganger("draw.smooth_estimates su_m_quadvar_te", plt)
})

test_that("draw.smooth_estimates works for su_m_quadvar_t2", {
    skip_on_os(os = "win")
    skip_on_os(os = "mac")
    expect_silent(plt <- draw(smooth_estimates(su_m_quadvar_t2, dist = 0.1,
                                               n = 25, n_3d = 4, n_4d = 3)))
    expect_doppelganger("draw.smooth_estimates su_m_quadvar_t2", plt)
})
