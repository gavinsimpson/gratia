# uses df_pois & m_nb

# response derivatives
test_that("response derivatives works", {
    skip_on_cran()
    N <- 50L
    expect_silent(ds <- data_slice(m_nb, x2 = evenly(x2, n = N),
        data = df_pois, envir = teardown_env()))
    expect_silent(yd <- response_derivatives(m_nb, data = ds,
        type = "central", focal = "x2", seed = 2))
    expect_s3_class(yd, class = "response_derivatives")
    expect_identical(nrow(yd), N)
    expect_snapshot(print(yd))

    expect_silent(yd <- response_derivatives(m_nb, data = ds,
        type = "forward", focal = "x2", seed = 2))
    expect_s3_class(yd, class = "response_derivatives")
    expect_identical(nrow(yd), N)
    expect_snapshot(print(yd))

    expect_silent(yd <- response_derivatives(m_nb, data = ds,
        type = "backward", focal = "x2", seed = 2))
    expect_s3_class(yd, class = "response_derivatives")
    expect_identical(nrow(yd), N)
    expect_snapshot(print(yd))
})
