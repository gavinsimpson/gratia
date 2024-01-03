## Test partial_residuals() and related residuals functions

N <- 1000L # 400L
# df <- data_sim("eg1", n = N, seed = 42)
## fit the model
# m       <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = 'REML')
# m_bam   <-  bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = 'fREML')
# m_gamm  <-  gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)
# m_gamm4 <-  gamm4(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)

test_that("partial_residuals returns a tibble", {
    expect_silent(p_res <- partial_residuals(m_gam))
    expect_s3_class(p_res, class = c("tbl_df", "tbl", "data.frame"),
        exact = TRUE)
    expect_named(p_res, c("s(x0)", "s(x1)", "s(x2)", "s(x3)"))
    expect_identical(nrow(p_res), N)
})

test_that("partial_residuals returns a tibble", {
    expect_silent(p_res <- partial_residuals(m_bam))
    expect_s3_class(p_res, class = c("tbl_df", "tbl", "data.frame"),
        exact = TRUE)
    expect_named(p_res, c("s(x0)", "s(x1)", "s(x2)", "s(x3)"))
    expect_identical(nrow(p_res), N)
})

test_that("partial_residuals returns a tibble", {
    expect_silent(p_res <- partial_residuals(m_gamm))
    expect_s3_class(p_res, class = c("tbl_df", "tbl", "data.frame"),
        exact = TRUE)
    expect_named(p_res, c("s(x0)", "s(x1)", "s(x2)", "s(x3)"))
    expect_identical(nrow(p_res), N)
})

test_that("partial_residuals returns a tibble", {
    expect_silent(p_res <- partial_residuals(m_gamm4))
    expect_s3_class(p_res, class = c("tbl_df", "tbl", "data.frame"),
        exact = TRUE)
    expect_named(p_res, c("s(x0)", "s(x1)", "s(x2)", "s(x3)"))
    expect_identical(nrow(p_res), N)
})

test_that("select works with partial_residuals", {
    expect_silent(p_res <- partial_residuals(m_gam, select = "s(x1)"))
    expect_s3_class(p_res, class = c("tbl_df", "tbl", "data.frame"),
        exact = TRUE)
    expect_named(p_res, "s(x1)")
    expect_identical(nrow(p_res), N)
})

test_that("partial_match selecting works with partial_residuals", {
    expect_silent(p_res <- partial_residuals(m_gam, select = "x1",
        partial_match = TRUE))
    expect_s3_class(p_res, class = c("tbl_df", "tbl", "data.frame"),
        exact = TRUE)
    expect_named(p_res, "s(x1)")
    expect_identical(nrow(p_res), N)
})

test_that("selecting throws an error if no match", {
    err_msg <- "Failed to match any smooths in model `m_gam`.
Try with 'partial_match = TRUE'?"
    expect_error(partial_residuals(m_gam, select = "foo", partial_match = TRUE),
        err_msg)
    expect_error(partial_residuals(m_gam, select = "foo", partial_match = FALSE),
        err_msg)
})
