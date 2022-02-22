## Test basis() and related functions

test_that("model_concurvity() works with a gam", {
    expect_silent(con <- model_concurvity(m_gam))
    expect_s3_class(con, "overall_concurvity")
    expect_s3_class(con, "concurvity")
    expect_named(con, c("type", "term", "concurvity"))
})

test_that("concrvity() works with a gam", {
    expect_silent(con <- concrvity(m_gam))
    expect_s3_class(con, "overall_concurvity")
    expect_s3_class(con, "concurvity")
    expect_named(con, c("type", "term", "concurvity"))
    expect_silent(plt <- draw(con))
    expect_doppelganger("draw.concurvity overall", plt)
})

test_that("model_concurvity() pairwise works with a gam", {
    expect_silent(con <- model_concurvity(m_gam, pairwise = TRUE))
    expect_s3_class(con, "pairwise_concurvity")
    expect_s3_class(con, "concurvity")
    expect_named(con, c("type", "term", "with", "concurvity"))
})

test_that("concrvity() pariwise works with a gam", {
    expect_silent(con <- concrvity(m_gam, pairwise = TRUE))
    expect_s3_class(con, "pairwise_concurvity")
    expect_s3_class(con, "concurvity")
    expect_named(con, c("type", "term", "with", "concurvity"))
    expect_silent(plt <- draw(con))
    expect_doppelganger("draw.concurvity pairwise", plt)
})
