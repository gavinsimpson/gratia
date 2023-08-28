## Test handling `by` variables in smooths/GAMs

test_that("draw() works with continuous by", {
    plt <- draw(su_m_cont_by)

    skip_on_ci()
    expect_doppelganger("continuous by-variable smmoth", plt)
})

test_that("draw() works with continuous by and fixed scales", {
    plt <- draw(su_m_cont_by, scales = "fixed")
    expect_s3_class(plt, "ggplot")

    expect_doppelganger("continuous by-variable smmoth fixed scales", plt)
})

test_that("evaluate_smooth() works with continuous by", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    sm  <- evaluate_smooth(su_m_cont_by, "s(x2)")
    expect_s3_class(sm, "evaluated_1d_smooth")
})

test_that("is_by_smooth() is TRUE with continuous by", {
    expect_true(is_by_smooth(su_m_cont_by[["smooth"]][[1]]))
})

test_that("is_factor_by_smooth() is FALSE with continuous by", {
    expect_false(is_factor_by_smooth(su_m_cont_by[["smooth"]][[1]]))
})

test_that("is_continuous_by_smooth() is TRUE with continuous by", {
    expect_true(is_continuous_by_smooth(su_m_cont_by[["smooth"]][[1]]))
})

test_that("get_by_smooth works", {
    sm <- get_by_smooth(su_m_factor_by, "s(x2)", level = "1")
    expect_s3_class(sm, "mgcv.smooth")
    expect_equal(sm, su_m_factor_by[["smooth"]][[1L]])

    sm <- get_by_smooth(su_m_factor_by_gamm, "s(x2)", level = "1")
    expect_s3_class(sm, "mgcv.smooth")
    expect_equal(sm, su_m_factor_by_gamm[["gam"]][["smooth"]][[1L]])

    expect_error(get_by_smooth(su_m_factor_by, "s(x4)", level = "1"),
                 "The requested smooth 's(x4)' is not a by smooth.",
                 fixed = TRUE)

    expect_error(get_by_smooth(su_m_factor_by, "s(x2)"),
                 "No value provided for argument 'level':", fixed = TRUE)

    expect_error(get_by_smooth(su_m_factor_by, "s(x2)", level = "4"),
                 "Invalid 'level' for smooth 's(x2)'.",
                 fixed = TRUE)
})

test_that("draw.gam works with select and parametric = TRUE", {
    plt <- draw(su_m_factor_by, select = 's(x2):fac1', parametric = TRUE,
        data = df_2_fac, envir = teardown_env())

    skip_on_ci()
    expect_doppelganger("draw.gam-user-select-and-parametric-true",
                                plt)
})
