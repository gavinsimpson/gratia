# Test draw.parametric_effects() method

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("draw.parametric_effects works for m_2_fac", {
    expect_message(peff <- parametric_effects(m_2_fac, data = df_2_fac,
    envir = teardown_env()),
                   "Interaction terms are not currently supported.")
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_2_fac", plt)
})

test_that("draw.parametric_effects works for m_para_sm", {
    expect_message(peff <- parametric_effects(m_para_sm, data = df_2_fac,
    envir = teardown_env()),
                   "Interaction terms are not currently supported.")
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_para_sm", plt)
})

test_that("draw.parametric_effects works for m_2_fac select term", {
    expect_silent(peff <- parametric_effects(m_2_fac, term = "fac", data = df_2_fac,
    envir = teardown_env()))
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_2_fac with term", plt)
})

test_that("draw.parametric_effects works for m_para_sm select term", {
    expect_silent(peff <- parametric_effects(m_para_sm, term = "fac", data = df_2_fac,
    envir = teardown_env()))
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_para_sm with term", plt)
})

test_that("draw.parametric_effects works with only parametric terms", {
    expect_message(peff <- parametric_effects(m_only_para, data = df_2_fac,
        envir = teardown_env()),
    "Interaction terms are not currently supported.")
    expect_silent(plt <- draw(peff))
    expect_doppelganger("draw parametric effects m_only_para", plt)
})

test_that("issue 45 parametric effects for lss models remains fixed", {
    skip_on_cran()
    # gratia error reprex
    issue_45_data <- function(n = 500, seed = NULL) {
        if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            runif(1)
        }
        if (is.null(seed)) {
            RNGstate <- get(".Random.seed", envir = .GlobalEnv)
        } else {
            R.seed <- get(".Random.seed", envir = .GlobalEnv)
            set.seed(seed)
            RNGstate <- structure(seed, kind = as.list(RNGkind()))
            on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
        }

        ## simulate some data...
        f0 <- function(x) 2 * sin(pi * x)
        f1 <- function(x) exp(2 * x)
        f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
            (10 * x)^3 * (1 - x)^10
        x0 <- runif(n)
        x1 <- runif(n)
        x2 <- runif(n)
        x3 <- runif(n)
        x4 <- sample(factor(c("a", "b", "c")), size = n, replace = TRUE)

        ## Simulate probability of potential presence...
        eta1 <- f0(x0) + f1(x1) - 3
        p <- binomial()$linkinv(eta1)
        y <- as.numeric(runif(n) < p) ## 1 for presence, 0 for absence

        ## Simulate y given potentially present (not exactly model fitted!)...
        ind <- y > 0
        eta2 <- f2(x2[ind]) / 3
        y[ind] <- rpois(exp(eta2), exp(eta2))
        df <- data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
        df
    }

    # set.seed(5)
    data_45 <- issue_45_data(n = 500, seed = 5)

    # Fit ZIP model...
    b <- gam(list(y ~ s(x2) + x3, ~ s(x0) + x1), family = ziplss(),
        data = data_45)

    expect_silent(plt <- draw(b, rug = FALSE))
    expect_doppelganger("ziplss with numeric para not plotted", plt)

    expect_silent(plt <- draw(b, parametric = TRUE, rug = FALSE,
        data = data_45, envir = teardown_env()))
    expect_doppelganger("ziplss with numeric para plotted", plt)

    # ZIP model with a categorical predictor
    b0 <- gam(list(y ~ s(x2) + x4, ~ s(x0) + x1), family = ziplss(),
        data = data_45)

    expect_silent(plt <- draw(b0, rug = FALSE))
    expect_doppelganger("ziplss with factor para not plotted", plt)

    expect_silent(plt <- draw(b0, parametric = TRUE, rug = FALSE,
        data = data_45, envir = teardown_env()))
    expect_doppelganger("ziplss with factor para plotted", plt)

    # ZIP model with linear and categorical predictor
    b1 <- gam(list(y ~ s(x2) + x3 + x4, ~ s(x0) + x1), family = ziplss(),
        data = data_45)

    expect_silent(plt <- draw(b1, rug = FALSE))
    expect_doppelganger("ziplss with both parametric not plotted", plt)

    expect_silent(plt <- draw(b1, parametric = TRUE, rug = FALSE,
        data = data_45, envir = teardown_env()))
    expect_doppelganger("ziplss with both parametric plotted", plt)
})
