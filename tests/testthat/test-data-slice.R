# Test data_slice() methods

# load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("data_slice works for a GAM", {
    expect_silent(ds <- data_slice(su_m_quick_eg1,
                                   x1 = evenly(x1, n = 50),
                                   x2 = evenly(x2, n = 50)))
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("x1", "x2", "x0", "x3"))
    expect_message(data_slice(su_m_quick_eg1,
                              x1 = evenly(x1, n = 50), var2 = "foo"),
                   "Some specified variable\\(s\\) not used in model")
    expect_message(data_slice(su_m_quick_eg1,
                              x1 = evenly(x1, n = 50), var2 = "foo"),
                   "var2")
})

test_that("process_slice_data works when passed a 1-row data frame, tibble, or list", {
    expect_silent(result1 <- process_slice_data(quick_eg1[1, ]))
    expect_silent(result2 <- process_slice_data(as_tibble(quick_eg1[1, ])))
    expect_silent(result3 <- process_slice_data(as.list(quick_eg1[1, ])))
    expect_equal(NROW(result1), 1L)
    expect_equal(NROW(result2), 1L)
    expect_equal(NROW(result3), 1L)
    expect_equal(result1, result2)
    expect_equal(result1, result3)
    expect_equal(result2, result3)
})

test_that("process_slice_data fails when passed a data frame with > 1 rows", {
    expect_error(process_slice_data(quick_eg1),
                 "'data' should have 1 row only. Supplied <200>",
                 fixed = TRUE)
})

test_that("process_slice_data fails when passed a matrix", {
    expect_error(process_slice_data(as.matrix(quick_eg1)),
                 "'data' should be a tibble, data frame, or list. Supplied <matrix>",
                 fixed = TRUE)
})

test_that("process_slice_data fails when passed a list with elements of length > 1", {
    expect_error(process_slice_data(as.list(quick_eg1[1:2, ])),
                 "If 'data' is a list, it should be a list of length-1 vectors",
                 fixed = TRUE)
})

test_that("process_slice_var fails when passed a logical variable", {
    dat2 <- cbind(quick_eg1,
                  foo = sample(c(TRUE,FALSE), nrow(quick_eg1), replace = TRUE))
    expect_error(process_slice_var("foo", dat2),
                 "Variable <foo> must be a factor or numeric vector. Found <character>",
                 fixed = TRUE)
})

test_that("process_slice_var fails when `x` is not character", {
    bar <- 1
    expect_error(process_slice_var(bar, dat2),
                 "Supplied 'x' is not character.",
                 fixed = TRUE)
})

test_that("process_slice_var returns NULL when `x` is NULL", {
    expect_identical(process_slice_var(NULL, dat2), NULL)
})

#set.seed(42)
#dat <- gamSim(4, n = 400, verbose = FALSE)
#mf <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)

test_that("data_slice works for a GAM with factor by", {
    expect_silent(ds <- data_slice(su_m_factor_by,
                                   x2 = evenly(x2),
                                   fac = evenly(fac)))
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("x2", "fac", "x0"))
})

test_that("default data_slice method fails gracefully", {
    expect_error(data_slice(1:10),
                 "Don't know how to create a data slice from <integer>",
                 fixed = TRUE)
})

test_that("value_closest_to_median fails for character vectors", {
    expect_error(value_closest_to_median(LETTERS),
                 "'x' must be a factor or numeric vector. Supplied <character>",
                 fixed = TRUE)
})

test_that("value_closest_to_median fails for logical vectors", {
    expect_error(value_closest_to_median(sample(c(TRUE, FALSE), 50,
                                                replace = TRUE)),
                 "'x' must be a factor or numeric vector. Supplied <logical>",
                 fixed = TRUE)
})

test_that("value_closest_to_median works with a factor", {
    expect_silent( result <- 
      gratia:::value_closest_to_median(su_eg4[["fac"]]) )
    expect_identical(factor(1, levels = c(1,2,3)), result)
})

# typical_values()
test_that("typical_values works with a simple GAM", {
    expect_silent(tv <- typical_values(su_m_quick_eg1))
    expect_s3_class(tv, "tbl_df")
    expect_identical(nrow(tv), 1L)
    expect_identical(ncol(tv), 4L)
})

test_that("typical_values works when including terms", {
    expect_silent(tv <- typical_values(su_m_quick_eg1, vars = c(x0, x2)))
    expect_s3_class(tv, "tbl_df")
    expect_identical(nrow(tv), 1L)
    expect_identical(ncol(tv), 2L)
    expect_identical(names(tv), c("x0","x2"))
})

test_that("typical_values works when excluding terms", {
    expect_silent(tv <- typical_values(su_m_quick_eg1, vars = !c(x0, x2)))
    expect_s3_class(tv, "tbl_df")
    expect_identical(nrow(tv), 1L)
    expect_identical(ncol(tv), 2L)
    expect_identical(names(tv), c("x1","x3"))
})

# factor_combos()
test_that("factor_combos works with a simple GAM", {
    expect_silent(fc <- factor_combos(m_para_sm))
    expect_s3_class(fc, "tbl_df")
    expect_identical(nrow(fc), 12L)
    expect_identical(ncol(fc), 2L)
    expect_named(fc, c("fac", "ff"))
})

test_that("factor_combos works when including terms", {
    expect_silent(fc <- factor_combos(m_para_sm, vars = fac))
    expect_s3_class(fc, "tbl_df")
    expect_identical(nrow(fc), 3L)
    expect_identical(ncol(fc), 1L)
    expect_named(fc, c("fac"))
})

test_that("factor_combos works when excluding terms", {
    expect_silent(fc <- factor_combos(m_para_sm, vars = !fac))
    expect_s3_class(fc, "tbl_df")
    expect_identical(nrow(fc), 4L)
    expect_identical(ncol(fc), 1L)
    expect_named(fc, c("ff"))
})

test_that("factor_combos works when there are no factor terms", {
    expect_message(fc <- factor_combos(m_gam, vars = !fac),
                   "Model contains no factor terms")
    expect_identical(fc, NULL)
})

# data_combos()
test_that("data_combos works with a GAM", {
    expect_silent(dc <- data_combos(m_para_sm))
    expect_s3_class(dc, "tbl_df")
    expect_identical(nrow(dc), 12L)
    expect_identical(ncol(dc), 5L)
    expect_named(dc, c("fac", "ff", "x0", "x1", "x2"))
})

test_that("data_combos works when including terms", {
    expect_silent(dc <- data_combos(m_para_sm, vars = c(fac, x0)))
    expect_s3_class(dc, "tbl_df")
    expect_identical(nrow(dc), 12L)
    expect_identical(ncol(dc), 2L)
    expect_named(dc, c("fac", "x0"))
})

test_that("data_combos works when exluding terms", {
    expect_silent(dc <- data_combos(m_para_sm, vars = !c(fac, x0)))
    expect_s3_class(dc, "tbl_df")
    expect_identical(nrow(dc), 12L)
    expect_identical(ncol(dc), 3L)
    expect_named(dc, c("ff", "x1", "x2"))
})

test_that("data_combos works when there are no factor terms", {
    expect_message(dc <- data_combos(m_gam),
        "Model contains no factor terms")
    expect_identical(nrow(dc), 1L)
    expect_identical(ncol(dc), 4L)
    expect_named(dc, c("x0", "x1", "x2", "x3"))
})

# Test data_slice with models that have an offset(s) - # 189
test_that("data_slice with no args works with models with an offset", {
    expect_silent(ds <- data_slice(m_1_smooth_offset))
    expect_identical(nrow(ds), 1L)
    expect_identical(ncol(ds), 2L)
    expect_identical(ds$off, 2)
})

test_that("data_slice with works with models with an offset", {
    expect_silent(ds <- data_slice(m_1_smooth_offset, off = 1))
    expect_identical(nrow(ds), 1L)
    expect_identical(ncol(ds), 2L)
    expect_identical(ds$off, 1)
})

test_that("data_slice with works with models with an offset", {
    expect_silent(ds <- data_slice(m_1_smooth_offset, x0 = evenly(x0, n = 50),
        off = 1))
    expect_identical(nrow(ds), 50L)
    expect_identical(ncol(ds), 2L)
    expect_identical(ds$off, rep(1, length.out = 50))
})

test_that("typical_values works for a data frame", {
    expect_silent(tv <- typical_values(su_eg2, vars = any_of(c("x", "z"))))
    expect_snapshot(tv, cran = FALSE)
})

test_that("data_slice works for a data frame", {
    expect_silent(ds <- data_slice(su_eg2, x = evenly(x, n = 25)))
    expect_snapshot(ds, cran = FALSE)
    expect_identical(nrow(ds), 25L)

    expect_silent(ds <- data_slice(su_eg2, x = evenly(x, n = 25),
        z = evenly(z, n = 25)))
    expect_snapshot(ds, cran = FALSE)
    expect_identical(nrow(ds), 625L)
})
