# Test draw.parametric_effects() method

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("draw.parametric_effects works for m_2_fac", {
    expect_message(peff <- parametric_effects(m_2_fac),
                   "Interaction terms are not currently supported.")
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_2_fac", plt)
})

test_that("draw.parametric_effects works for m_para_sm", {
    expect_message(peff <- parametric_effects(m_para_sm),
                   "Interaction terms are not currently supported.")
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_para_sm", plt)
})

test_that("draw.parametric_effects works for m_2_fac select term", {
    expect_silent(peff <- parametric_effects(m_2_fac, term = "fac"))
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_2_fac with term", plt)
})

test_that("draw.parametric_effects works for m_para_sm select term", {
    expect_silent(peff <- parametric_effects(m_para_sm, term = "fac"))
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_para_sm with term", plt)
})
