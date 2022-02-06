# Test parametric_effects() method

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("parametric_effects works for m_2_fac", {
    expect_message(peff <- parametric_effects(m_2_fac),
                   "Interaction terms are not currently supported.")
    expect_s3_class(peff, class = c("parametric_effects", "tbl_df", "tbl",
                                    "data.frame"))
    expect_identical(ncol(peff), 5L)
    expect_identical(nrow(peff), 7L)
    expect_named(peff, c("term", "type", "level", "partial", "se"))
})

test_that("parametric_effects works for m_para_sm", {
    expect_message(peff <- parametric_effects(m_para_sm),
                   "Interaction terms are not currently supported.")
    expect_s3_class(peff, class = c("parametric_effects", "tbl_df", "tbl",
                                    "data.frame"))
    expect_identical(ncol(peff), 6L)
    expect_identical(nrow(peff), 407L)
    expect_named(peff, c("term", "type", "level", "value", "partial", "se"))
})

test_that("parametric_effects works for m_2_fac select term", {
    expect_silent(peff <- parametric_effects(m_2_fac, term = "fac"))
    expect_s3_class(peff, class = c("parametric_effects", "tbl_df", "tbl",
                                    "data.frame"))
    expect_identical(ncol(peff), 5L)
    expect_identical(nrow(peff), 3L)
    expect_named(peff, c("term", "type", "level", "partial", "se"))
})

test_that("parametric_effects works for m_para_sm select term", {
    expect_silent(peff <- parametric_effects(m_para_sm, term = "fac"))
    expect_s3_class(peff, class = c("parametric_effects", "tbl_df", "tbl",
                                    "data.frame"))
    expect_identical(ncol(peff), 5L)
    expect_identical(nrow(peff), 3L)
    expect_named(peff, c("term", "type", "level", "partial", "se"))
})
