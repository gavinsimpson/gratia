## Test subsetting methods `[`

test_that("subsetting works for smooth_samples", {
    sm <- smooth_samples(m_gam, seed = 42)
    hsm <- head(sm)
    expect_identical(NROW(hsm), 6L)
    expect_true(! is.null(attr(hsm, "seed")))

    attrs <- attributes(hsm)
    expect_type(attrs, "list")
    expect_named(attrs,
        expected = c("names", "row.names", "class", "seed", "data_names"))
})
