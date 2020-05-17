## Test basis() and related functions

## load packages
library("testthat")
library("gratia")

context("test data_sim()")

data(ref_sims, package = "gratia")

test_that("test data_sim() reproduces expected output", {
    new_sims <- create_reference_simulations()
    for (i in names(ref_sims)) {
        expect_identical(new_sims[[!!(i)]], ref_sims[[!!(i)]])
    }
})
