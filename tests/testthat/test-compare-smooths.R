## Test fitted-values()

## load packages
library("testthat")
library("gratia")
library("mgcv")

# uses
# * su_m_quick_eg1, and
# * su_m_quick_eg1_shrink
# from setup.R

test_that("compare_smooths() works for a pair of GAMs", {
    expect_silent(cs <- compare_smooths(su_m_quick_eg1,
                                        su_m_quick_eg1_shrink))

    expect_named(cs, expected = c("model", "smooth", "type", "by", "data"))

    expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))

    expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})

test_that("compare_smooths() errors when passed a single model", {
    expect_error(compare_smooths(su_m_quick_eg1),
                 "Need at least two models to compare smooths")
})

test_that("draw.compare_smooths() can plot a comparison of smooths", {
    expect_silent(cs <- compare_smooths(su_m_quick_eg1,
                                        su_m_quick_eg1_shrink))
    expect_silent(plt <- draw(cs) )
    expect_doppelganger("compare smooths - all smooths", plt)

    expect_silent(plt <- draw(cs, nrow = 2, ncol = 3))
    expect_doppelganger("compare smooths - set nrow ncol", plt)

    expect_silent(plt <- draw(cs) & theme(legend.position = "bottom"))
    expect_doppelganger("compare smooths - bottom legened", plt)
})