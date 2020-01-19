## Test family and link utilities

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("Testing Family Utility Functions")

val <- 1

test_that("link() works for gaussian() family objects", {
    f <- link(gaussian())
    expect_type(f, "closure")
    expect_identical(f(val), val)
    expect_identical(f, gaussian()$linkfun)
})

test_that("link() works for poisson() family objects", {
    f <- link(poisson())
    expect_type(f, "closure")
    expect_identical(f(val), log(val))
    expect_identical(f, poisson()$linkfun)
})

test_that("link() works for binomial() family objects", {
    f <- link(binomial())
    expect_type(f, "closure")
    expect_identical(f(val), binomial()$linkfun(val))
    expect_identical(f, binomial()$linkfun)
})

test_that("link() works for Gamma() family objects", {
    f <- link(Gamma())
    expect_type(f, "closure")
    expect_identical(f(val), Gamma()$linkfun(val))
    expect_identical(f, Gamma()$linkfun)
})

test_that("link() works for inverse.gaussian() family objects", {
    f <- link(inverse.gaussian())
    expect_type(f, "closure")
    expect_identical(f(val), inverse.gaussian()$linkfun(val))
    expect_identical(f, inverse.gaussian()$linkfun)
})

test_that("link() works for quasi() family objects", {
    f <- link(quasi())
    expect_type(f, "closure")
    expect_identical(f(val), quasi()$linkfun(val))
    expect_identical(f, quasi()$linkfun)
})

test_that("link() works for quasibinomial() family objects", {
    f <- link(quasibinomial())
    expect_type(f, "closure")
    expect_identical(f(val), quasibinomial()$linkfun(val))
    expect_identical(f, quasibinomial()$linkfun)
})

test_that("link() works for quasipoisson() family objects", {
    f <- link(quasipoisson())
    expect_type(f, "closure")
    expect_identical(f(val), quasipoisson()$linkfun(val))
    expect_identical(f, quasipoisson()$linkfun)
})

test_that("link() works for negbin() family objects", {
    theta <- 1.1
    f <- link(negbin(theta = theta))
    expect_type(f, "closure")
    expect_identical(f(val), negbin(theta = theta)$linkfun(val))
    expect_identical(f, negbin(theta = theta)$linkfun)
})

test_that("link() works for nb() family objects", {
    f <- link(nb())
    expect_type(f, "closure")
    expect_identical(f(val), nb()$linkfun(val))
    expect_identical(f, nb()$linkfun)
})

test_that("link() works for Tweedie() family objects", {
    p <- 1.1
    f <- link(Tweedie(p = p))
    expect_type(f, "closure")
    expect_identical(f(val), Tweedie(p = p)$linkfun(val))
    expect_identical(f, Tweedie(p = p)$linkfun)
})

test_that("link() works for tw() family objects", {
    f <- link(tw())
    expect_type(f, "closure")
    expect_identical(f(val), tw()$linkfun(val))
    expect_identical(f, tw()$linkfun)
})

test_that("link() works for scat() family objects", {
    f <- link(scat())
    expect_type(f, "closure")
    expect_identical(f(val), scat()$linkfun(val))
    expect_identical(f, scat()$linkfun)
})

test_that("link() works for betar() family objects", {
    f <- link(betar())
    expect_type(f, "closure")
    expect_identical(f(val), betar()$linkfun(val))
    expect_identical(f, betar()$linkfun)
})

test_that("link() works for ocat() family objects", {
    theta <- 1.1
    f <- link(ocat(theta = theta))
    expect_type(f, "closure")
    expect_identical(f(val), ocat(theta = theta)$linkfun(val))
    expect_identical(f, ocat(theta = theta)$linkfun)
})

test_that("link() works for ziP() family objects", {
    f <- link(ziP())
    expect_type(f, "closure")
    expect_identical(f(val), ziP()$linkfun(val))
    expect_identical(f, ziP()$linkfun)
})

test_that("link() works for cox.ph() family objects", {
    f <- link(cox.ph())
    expect_type(f, "closure")
    expect_identical(f(val), cox.ph()$linkfun(val))
    expect_identical(f, cox.ph()$linkfun)
})

## inv_link
test_that("inv_link() works for gaussian() family objects", {
    f <- inv_link(gaussian())
    expect_type(f, "closure")
    expect_identical(f(val), val)
    expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works for poisson() family objects", {
    f <- inv_link(poisson())
    expect_type(f, "closure")
    expect_identical(f(val), exp(val))
    expect_identical(f, poisson()$linkinv)
})

test_that("inv_link() works for binomial() family objects", {
    f <- inv_link(binomial())
    expect_type(f, "closure")
    expect_identical(f(val), binomial()$linkinv(val))
    expect_identical(f, binomial()$linkinv)
})

test_that("inv_link() works for Gamma() family objects", {
    f <- inv_link(Gamma())
    expect_type(f, "closure")
    expect_identical(f(val), Gamma()$linkinv(val))
    expect_identical(f, Gamma()$linkinv)
})

test_that("inv_link() works for inverse.gaussian() family objects", {
    f <- inv_link(inverse.gaussian())
    expect_type(f, "closure")
    expect_identical(f(val), inverse.gaussian()$linkinv(val))
    expect_identical(f, inverse.gaussian()$linkinv)
})

test_that("inv_link() works for quasi() family objects", {
    f <- inv_link(quasi())
    expect_type(f, "closure")
    expect_identical(f(val), quasi()$linkinv(val))
    expect_identical(f, quasi()$linkinv)
})

test_that("inv_link() works for quasibinomial() family objects", {
    f <- inv_link(quasibinomial())
    expect_type(f, "closure")
    expect_identical(f(val), quasibinomial()$linkinv(val))
    expect_identical(f, quasibinomial()$linkinv)
})

test_that("inv_link() works for quasipoisson() family objects", {
    f <- inv_link(quasipoisson())
    expect_type(f, "closure")
    expect_identical(f(val), quasipoisson()$linkinv(val))
    expect_identical(f, quasipoisson()$linkinv)
})

test_that("inv_link() works for negbin() family objects", {
    theta <- 1.1
    f <- inv_link(negbin(theta = theta))
    expect_type(f, "closure")
    expect_identical(f(val), negbin(theta = theta)$linkinv(val))
    expect_identical(f, negbin(theta = theta)$linkinv)
})

test_that("inv_link() works for nb() family objects", {
    f <- inv_link(nb())
    expect_type(f, "closure")
    expect_identical(f(val), nb()$linkinv(val))
    expect_identical(f, nb()$linkinv)
})

test_that("inv_link() works for Tweedie() family objects", {
    p <- 1.1
    f <- inv_link(Tweedie(p = p))
    expect_type(f, "closure")
    expect_identical(f(val), Tweedie(p = p)$linkinv(val))
    expect_identical(f, Tweedie(p = p)$linkinv)
})

test_that("inv_link() works for tw() family objects", {
    f <- inv_link(tw())
    expect_type(f, "closure")
    expect_identical(f(val), tw()$linkinv(val))
    expect_identical(f, tw()$linkinv)
})

test_that("inv_link() works for scat() family objects", {
    f <- inv_link(scat())
    expect_type(f, "closure")
    expect_identical(f(val), scat()$linkinv(val))
    expect_identical(f, scat()$linkinv)
})

test_that("inv_link() works for betar() family objects", {
    f <- inv_link(betar())
    expect_type(f, "closure")
    expect_identical(f(val), betar()$linkinv(val))
    expect_identical(f, betar()$linkinv)
})

test_that("inv_link() works for ocat() family objects", {
    theta <- 1.1
    f <- inv_link(ocat(theta = theta))
    expect_type(f, "closure")
    expect_identical(f(val), ocat(theta = theta)$linkinv(val))
    expect_identical(f, ocat(theta = theta)$linkinv)
})

test_that("inv_link() works for ziP() family objects", {
    f <- inv_link(ziP())
    expect_type(f, "closure")
    expect_identical(f(val), ziP()$linkinv(val))
    expect_identical(f, ziP()$linkinv)
})

test_that("inv_link() works for cox.ph() family objects", {
    f <- inv_link(cox.ph())
    expect_type(f, "closure")
    expect_identical(f(val), cox.ph()$linkinv(val))
    expect_identical(f, cox.ph()$linkinv)
})

test_that("extract_link() works on gaussian() family objects", {
    ## link
    f <- extract_link(gaussian())
    expect_type(f, "closure")
    expect_identical(f(val), val)
    expect_identical(f, gaussian()$linkfun)
    ## inverse
    f <- extract_link(gaussian(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), val)
    expect_identical(f, gaussian()$linkinv)
})

test_that("extract_link() works on poisson() family objects", {
    ## link
    f <- extract_link(poisson())
    expect_type(f, "closure")
    expect_identical(f(val), log(val))
    expect_identical(f, poisson()$linkfun)
    ## inverse
    f <- extract_link(poisson(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), exp(val))
    expect_identical(f, poisson()$linkinv)
})

test_that("extract_link() works on binomial() family objects", {
    ## link
    f <- extract_link(binomial())
    expect_type(f, "closure")
    expect_identical(f(val), binomial()$linkfun(val))
    expect_identical(f, binomial()$linkfun)
    ## inverse
    f <- extract_link(binomial(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), binomial()$linkinv(val))
    expect_identical(f, binomial()$linkinv)
})

test_that("extract_link() works on Gamma() family objects", {
    ## link
    f <- extract_link(Gamma())
    expect_type(f, "closure")
    expect_identical(f(val), Gamma()$linkfun(val))
    expect_identical(f, Gamma()$linkfun)
    ## inverse
    f <- extract_link(Gamma(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), Gamma()$linkinv(val))
    expect_identical(f, Gamma()$linkinv)
})

test_that("extract_link() works on inverse.gaussian() family objects", {
    ## link
    f <- extract_link(inverse.gaussian())
    expect_type(f, "closure")
    expect_identical(f(val), inverse.gaussian()$linkfun(val))
    expect_identical(f, inverse.gaussian()$linkfun)
    ## inverse
    f <- extract_link(inverse.gaussian(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), inverse.gaussian()$linkinv(val))
    expect_identical(f, inverse.gaussian()$linkinv)
})

test_that("extract_link() works on quasi() family objects", {
    ## link
    f <- extract_link(quasi())
    expect_type(f, "closure")
    expect_identical(f(val), quasi()$linkfun(val))
    expect_identical(f, quasi()$linkfun)
    ## inverse
    f <- extract_link(quasi(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), quasi()$linkinv(val))
    expect_identical(f, quasi()$linkinv)
})

test_that("extract_link() works on quasibinomial() family objects", {
    ## link
    f <- extract_link(quasibinomial())
    expect_type(f, "closure")
    expect_identical(f(val), quasibinomial()$linkfun(val))
    expect_identical(f, quasibinomial()$linkfun)
    ## inverse
    f <- extract_link(quasibinomial(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), quasibinomial()$linkinv(val))
    expect_identical(f, quasibinomial()$linkinv)
})

test_that("extract_link() works on quasipoisson() family objects", {
    ## link
    f <- extract_link(quasipoisson())
    expect_type(f, "closure")
    expect_identical(f(val), quasipoisson()$linkfun(val))
    expect_identical(f, quasipoisson()$linkfun)
    ## inverse
    f <- extract_link(quasipoisson(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), quasipoisson()$linkinv(val))
    expect_identical(f, quasipoisson()$linkinv)
})

test_that("extract_link() works on negbin() family objects", {
    ## link
    theta = 1.1
    f <- extract_link(negbin(theta = theta))
    expect_type(f, "closure")
    expect_identical(f(val), negbin(theta = theta)$linkfun(val))
    expect_identical(f, negbin(theta = theta)$linkfun)
    ## inverse
    f <- extract_link(negbin(theta = theta), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), negbin(theta = theta)$linkinv(val))
    expect_identical(f, negbin(theta = theta)$linkinv)
})

test_that("extract_link() works on nb() family objects", {
    ## link
    f <- extract_link(nb())
    expect_type(f, "closure")
    expect_identical(f(val), nb()$linkfun(val))
    expect_identical(f, nb()$linkfun)
    ## inverse
    f <- extract_link(nb(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), nb()$linkinv(val))
    expect_identical(f, nb()$linkinv)
})

test_that("extract_link() works on Tweedie() family objects", {
    ## link
    p = 1.1
    f <- extract_link(Tweedie(p = p))
    expect_type(f, "closure")
    expect_identical(f(val), Tweedie(p = p)$linkfun(val))
    expect_identical(f, Tweedie(p = p)$linkfun)
    ## inverse
    f <- extract_link(Tweedie(p = p), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), Tweedie(p = p)$linkinv(val))
    expect_identical(f, Tweedie(p = p)$linkinv)
})

test_that("extract_link() works on tw() family objects", {
    ## link
    f <- extract_link(tw())
    expect_type(f, "closure")
    expect_identical(f(val), tw()$linkfun(val))
    expect_identical(f, tw()$linkfun)
    ## inverse
    f <- extract_link(tw(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), tw()$linkinv(val))
    expect_identical(f, tw()$linkinv)
})

test_that("extract_link() works on scat() family objects", {
    ## link
    f <- extract_link(scat())
    expect_type(f, "closure")
    expect_identical(f(val), scat()$linkfun(val))
    expect_identical(f, scat()$linkfun)
    ## inverse
    f <- extract_link(scat(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), scat()$linkinv(val))
    expect_identical(f, scat()$linkinv)
})

test_that("extract_link() works on betar() family objects", {
    ## link
    f <- extract_link(betar())
    expect_type(f, "closure")
    expect_identical(f(val), betar()$linkfun(val))
    expect_identical(f, betar()$linkfun)
    ## inverse
    f <- extract_link(betar(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), betar()$linkinv(val))
    expect_identical(f, betar()$linkinv)
})

test_that("extract_link() works on ziP() family objects", {
    ## link
    f <- extract_link(ziP())
    expect_type(f, "closure")
    expect_identical(f(val), ziP()$linkfun(val))
    expect_identical(f, ziP()$linkfun)
    ## inverse
    f <- extract_link(ziP(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), ziP()$linkinv(val))
    expect_identical(f, ziP()$linkinv)
})

test_that("extract_link() works on ocat() family objects", {
    theta = 1.1
    ## link
    f <- extract_link(ocat(theta = theta))
    expect_type(f, "closure")
    expect_identical(f(val), ocat(theta = theta)$linkfun(val))
    expect_identical(f, ocat(theta = theta)$linkfun)
    ## inverse
    f <- extract_link(ocat(theta = theta), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), ocat(theta = theta)$linkinv(val))
    expect_identical(f, ocat(theta = theta)$linkinv)
})

test_that("extract_link() works on cox.ph() family objects", {
    ## link
    f <- extract_link(cox.ph())
    expect_type(f, "closure")
    expect_identical(f(val), cox.ph()$linkfun(val))
    expect_identical(f, cox.ph()$linkfun)
    ## inverse
    f <- extract_link(cox.ph(), inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), cox.ph()$linkinv(val))
    expect_identical(f, cox.ph()$linkinv)
})
