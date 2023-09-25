## Test bam() with AR(1) models

# models created in setup.R
#  * m_ar1 simple model with single time series with rho = 0.6
#  * m_ar1_by factor by model with two series, rho = 0.6, n = 400 in each

test_that("draw works for a simple BAM with AR 1", {
    expect_silent(plt <- draw(m_ar1, rug = FALSE))

    skip_on_ci()
    expect_doppelganger("draw bam ar 1 single series", plt)
})

test_that("draw works for a factor by BAM, 2 series, with AR 1", {
    expect_silent(plt <- draw(m_ar1_by, rug = FALSE))

    skip_on_ci()
    expect_doppelganger("draw bam ar 1 factor by", plt)
})
