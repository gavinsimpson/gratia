# Test overview() & methods

# load packages
library("testthat")
library("gratia")

test_that("overview works for a GAM", {
    expect_silent(ovr <- overview(m_gam))
    expect_s3_class(ovr, "tbl_df")
    expect_s3_class(ovr, "overview")
})

test_that("overview works for a GAMM", {
    expect_silent(ovr <- overview(m_gamm))
    expect_s3_class(ovr, "tbl_df")
    expect_s3_class(ovr, "overview")
})

test_that("overview works for a BAM", {
    expect_silent(ovr <- overview(m_bam))
    expect_s3_class(ovr, "tbl_df")
    expect_s3_class(ovr, "overview")
})

test_that("print() output is as expected for GAM", {
    expect_snapshot({
        print(overview(m_gam))
    })
})

test_that("print() output is as expected for GAMM", {
    expect_snapshot({
        print(overview(m_gamm))
    })
})

test_that("print() output is as expected for BAM", {
    expect_snapshot({
        print(overview(m_bam))
    })
})
