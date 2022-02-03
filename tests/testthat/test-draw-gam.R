# Test draw.gam()
# A lot of the current tests for this are over in test-draw-methods.R

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")

test_that("draw.gam works for m_1_smooth", {
    expect_silent(plt <- draw(m_1_smooth))
    expect_doppelganger("draw_gam m_1_smooth", plt)
})

test_that("draw.gam works for m_gam", {
    expect_silent(plt <- draw(m_gam))
    expect_doppelganger("draw_gam m_gam", plt)
})

test_that("draw.gam works for m_gamm", {
    expect_silent(plt <- draw(m_gamm))
    expect_doppelganger("draw_gam m_gamm", plt)
})

test_that("draw.gam works for m_gamm4", {
    expect_silent(plt <- draw(m_gamm4))
    expect_doppelganger("draw_gam m_gamm4", plt)
})

test_that("draw.gam works for m_bam", {
    expect_silent(plt <- draw(m_bam))
    expect_doppelganger("draw_gam m_bam", plt)
})

test_that("draw.gam works for m_gaulss", {
    expect_silent(plt <- draw(m_gaulss))
    expect_doppelganger("draw_gam m_gaulss", plt)
})

test_that("draw.gam works for m_scat", {
    expect_silent(plt <- draw(m_scat))
    expect_doppelganger("draw_gam m_scat", plt)
})

test_that("draw.gam works for m_gamgcv", {
    expect_silent(plt <- draw(m_gamgcv))
    expect_doppelganger("draw_gam m_gamgcv", plt)
})

test_that("draw.gam works for rm1", {
    expect_silent(plt <- draw(rm1))
    expect_doppelganger("draw_gam rm1", plt)
})

test_that("draw.gam works for rm2", {
    expect_silent(plt <- draw(rm2))
    expect_doppelganger("draw_gam rm2", plt)
})

test_that("draw.gam works for dlnm_m", {
    expect_silent(plt <- draw(dlnm_m))
    expect_doppelganger("draw_gam dlnm_m", plt)
})

test_that("draw.gam works for m_ar1", {
    expect_silent(plt <- draw(m_ar1))
    expect_doppelganger("draw_gam m_ar1", plt)
})

test_that("draw.gam works for m_ar1_by", {
    expect_silent(plt <- draw(m_ar1_by))
    expect_doppelganger("draw_gam m_ar1_by", plt)
})

test_that("draw.gam works for m_2_fac", {
    expect_silent(plt <- draw(m_2_fac))
    expect_doppelganger("draw_gam m_2_fac", plt)
})

test_that("draw.gam works for m_para_sm", {
    expect_silent(plt <- draw(m_para_sm))
    expect_doppelganger("draw_gam m_para_sm", plt)
})

test_that("draw.gam works for m_2_fac", {
    expect_message(plt <- draw(m_2_fac, parametric = TRUE),
                   "Interaction terms are not currently supported.")
    expect_doppelganger("draw_gam m_2_fac parametric", plt)
})

test_that("draw.gam works for m_para_sm", {
    expect_message(plt <- draw(m_para_sm, parametric = TRUE),
                   "Interaction terms are not currently supported.")
    expect_doppelganger("draw_gam m_para_sm parametric", plt)
})
