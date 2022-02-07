## Test basis() and related functions

## load packages
library("testthat")
library("gratia")
library("mgcv")

#set.seed(42)
#data <- gamSim(eg = 1, verbose = FALSE)
## take only some columns
data <- su_eg1[, c("y", "x0", "x1", "x2", "x3")]
## fit the model
#m <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = su_eg1, method = 'REML')

test_that("add_fitted works for a GAM", {
    expect_silent(df <- add_fitted(data, m_gam))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".value"))
})

test_that("add_fitted works for a GAM with type = 'terms'", {
    expect_silent(df <- add_fitted(data, m_gam, type = 'terms'))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".constant",
                       ".s(x0)", ".s(x1)", ".s(x2)", ".s(x3)"))
})

test_that("add_fitted works for a GAM with se.fit = TRUE", {
    expect_silent(df <- add_fitted(data, m_gam, se.fit = TRUE))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".value"))
})

test_that("prefix works for a GAM with type = 'terms'", {
    expect_silent(df <- add_fitted(data, m_gam, type = 'terms', prefix = ".."))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", "..constant",
                       "..s(x0)", "..s(x1)", "..s(x2)", "..s(x3)"))
})

test_that("add_residuals works for a GAM", {
    expect_silent(df <- add_residuals(data, m_gam, type = "pearson"))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".residual"))
})

test_that("add_partial_residuals works for a GAM", {
    expect_silent(df <- add_partial_residuals(data, m_gam))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3",
                       "s(x0)", "s(x1)", "s(x2)", "s(x3)"))
    expect_silent(df <- add_partial_residuals(data, m_gam, select = "s(x2)"))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", "s(x2)"))
})

## test what happens with na.action and NAs in input
miss <- sample(nrow(data), 10)
data[["x0"]][miss] <- NA
m_na <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data, method = 'REML')
m_na_excl <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data, method = 'REML',
                 na.action = na.exclude)

test_that("add_residuals works for a GAM with NA in data", {
    expect_error(add_residuals(data, m_na, value = "..resid.."),
                 "Length of model residuals does not equal number of rows in 'data'",
                 fixed = TRUE)
    expect_silent(df <- add_residuals(data, m_na_excl))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".residual"))
})

test_that("add_partial_residuals works for a GAM", {
    skip_on_cran()
    skip("This needs fixing")
    expect_error(add_partial_residuals(data, m_na),
                 "Length of model residuals not equal to number of rows in 'data'",
                 fixed = TRUE)
    expect_silent(df <- add_partial_residuals(data, m_na_excl))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3",
                       "s(x0)", "s(x1)", "s(x2)", "s(x3)"))
    expect_silent(df <- add_partial_residuals(data, m_na_excl,
                  select = "s(x2)"))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", "s(x2)"))
})


test_that("add_confint works for smooth_estimates", {
    expect_silent(sm <- smooth_estimates(m_gam))
    expect_silent(sm <- add_confint(sm, coverage = 0.89))
    expect_s3_class(sm, c("smooth_estimates", "tbl_df", "tbl", "data.frame"))
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x0",
                       "x1", "x2", "x3", "lower_ci","upper_ci"))
    expect_identical(nrow(sm), 400L)
    expect_identical(ncol(sm), 11L)
})

test_that("add_confint works for smooth_estimates", {
    expect_silent(sm <- smooth_estimates(m_gam, unnest = FALSE))
    expect_error(add_confint(sm),
                 "Did you use `smooth_estimates(..., unnest = FALSE)`?",
                 fixed = TRUE)
})

test_that("add_confint.default fails is no est and se", {
    expect_error(add_confint(typical_values(m_gam)),
                 "'object' does not contain one or both of 'est' or 'se'.")
})

test_that("add_constant works for parametric_effects", {
    expect_message(pe <- parametric_effects(m_para_sm),
                   "Interaction terms are not currently supported.")
    expect_silent(pe <- add_constant(pe, constant = 10))
    expect_error(pe <- add_constant(pe, constant = "a"),
                 "'constant' must be numeric: supplied <a>",
                 fixed = TRUE)
})

test_that("add_constant works for evaluate_parametric_term", {
    expect_silent(pe <- evaluate_parametric_term(m_para_sm, term = "fac"))
    expect_silent(pe <- add_constant(pe, constant = 10))
    expect_error(pe <- add_constant(pe, constant = "a"),
                 "'constant' must be numeric: supplied <a>",
                 fixed = TRUE)
})

test_that("add_constant works for evaluated_smooth", {
    expect_warning(sm <- evaluate_smooth(m_gam, smooth = "s(x1)"))
    expect_silent(sm <- add_constant(sm, constant = 10))
    expect_error(pe <- add_constant(pe, constant = "a"),
                 "'constant' must be numeric: supplied <a>",
                 fixed = TRUE)
})

test_that("add_constant works for evaluated_smooth", {
    expect_silent(sm <- smooth_estimates(m_gam, smooth = "s(x1)"))
    expect_silent(sm <- add_constant(sm, constant = 10))
    expect_error(pe <- add_constant(pe, constant = "a"),
                 "'constant' must be numeric: supplied <a>",
                 fixed = TRUE)
})

test_that("add_constant works for tbl", {
    expect_silent(tbl <- add_constant(su_eg1, constant = 10, column = "y"))
    expect_error(pe <- add_constant(pe, constant = "a"),
                 "'constant' must be numeric: supplied <a>",
                 fixed = TRUE)
})
