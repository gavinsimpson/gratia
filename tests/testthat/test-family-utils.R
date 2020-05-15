## Test family and link utilities

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("gamm4")

context("Testing Family Utility Functions")

val <- 1

## check link and inv_link for models
set.seed(4234)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m_glm <- glm(y ~ x0, data = dat)
m_gam <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m_gaulss <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~ 1), data = dat,
                family = gaulss)
m_gamm <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
m_bam <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "fREML")
m_gamm4 <- gamm4(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

l <- list(mer = 1:3, gam = 1:3)

test_that("link() works with a glm() model", {
    f <- link(m_glm)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gam() model", {
    f <- link(m_gam)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gamm() model", {
    f <- link(m_gamm)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gamm4() model", {
    f <- link(m_gamm4)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkfun)
})

test_that("link.list() fails with a list that isn't a gamm4", {
    expect_error(link(l),
                 regexp = "`object` does not appear to a `gamm4` model object",
                 fixed = TRUE)
})

test_that("link() works with a bam() model", {
    f <- link(m_bam)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gam() gaulss model", {
    f <- link(m_gaulss)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkfun)
})

test_that("inv_link() works with a gam() model", {
    f <- inv_link(m_gam)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a glm() model", {
    f <- inv_link(m_glm)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a gamm() model", {
    f <- inv_link(m_gamm)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a gamm4() model", {
    f <- inv_link(m_gamm4)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a bam() model", {
    f <- inv_link(m_bam)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link.list() fails with a list that isn't a gamm4", {
    expect_error(inv_link(l),
                 regexp = "`object` does not appear to a `gamm4` model object",
                 fixed = TRUE)
})

test_that("inv_link() works with a gam() gaulss model", {
    f <- inv_link(m_gaulss)
    expect_type(f, "closure")
    expect_identical(f, gaussian()$linkinv)
})

## link
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

test_that("extract_link() works on gaulss() family objects", {
    fam <- gaulss()
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    
    ## scale parameter
    ## link
    f <- extract_link(fam, parameter = "scale")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    f <- extract_link(fam, parameter = "sigma")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "scale", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "sigma", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
})

test_that("extract_link() works on gammals() family objects", {
    fam <- gammals()
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    
    ## scale parameter
    ## link
    f <- extract_link(fam, parameter = "scale")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    f <- extract_link(fam, parameter = "sigma")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "scale", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "sigma", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
})

test_that("extract_link() works on twlss() family objects", {
    fam <- twlss()
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    
    ## scale parameter
    ## link
    f <- extract_link(fam, parameter = "scale")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    f <- extract_link(fam, parameter = "sigma")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "scale", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "sigma", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    
    ## power parameter
    ## link
    f <- extract_link(fam, parameter = "power")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[3L]]$linkfun(val))
    expect_identical(f, fam$linfo[[3L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "power", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[3L]]$linkinv(val))
    expect_identical(f, fam$linfo[[3L]]$linkinv)
})

test_that("extract_link() works on gevlss() family objects", {
    fam <- gevlss()
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    
    ## scale parameter
    ## link
    f <- extract_link(fam, parameter = "scale")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    f <- extract_link(fam, parameter = "sigma")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "scale", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "sigma", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    
    ## shape parameter, also xi
    ## link
    xi_val <- 0.5 # must be in range 0-1
    f <- extract_link(fam, parameter = "shape")
    expect_type(f, "closure")
    expect_identical(f(xi_val), fam$linfo[[3L]]$linkfun(xi_val))
    expect_identical(f, fam$linfo[[3L]]$linkfun)
    f <- extract_link(fam, parameter = "xi")
    expect_type(f, "closure")
    expect_identical(f(xi_val), fam$linfo[[3L]]$linkfun(xi_val))
    expect_identical(f, fam$linfo[[3L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "shape", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(xi_val), fam$linfo[[3L]]$linkinv(xi_val))
    expect_identical(f, fam$linfo[[3L]]$linkinv)
    f <- extract_link(fam, parameter = "xi", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(xi_val), fam$linfo[[3L]]$linkinv(xi_val))
    expect_identical(f, fam$linfo[[3L]]$linkinv)
})

test_that("extract_link() works on ziplss() family objects", {
    fam <- ziplss()
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkinv(val))
    expect_identical(f, fam$linfo[[1L]]$linkinv)
    
    ## scale parameter - really the zero-inflation bit
    ## link
    f <- extract_link(fam, parameter = "scale")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    f <- extract_link(fam, parameter = "pi")
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkfun(val))
    expect_identical(f, fam$linfo[[2L]]$linkfun)
    ## inverse
    f <- extract_link(fam, parameter = "scale", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "pi", inverse = TRUE)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
})

test_that("extract_link() works on mvn() family objects", {
    fam <- mvn(d = 2)
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location", which_eta = 1L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu", which_eta = 1L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)

    ## error if no `which_eta`
    expect_error(extract_link(fam, parameter = "mu"),
                 "Which linear predictor not specified; see 'which_eta'",
                 fixed = TRUE)

    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE,
                      which_eta = 2L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE,
                      which_eta = 2L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
})

test_that("extract_link() works on multinom() family objects", {
    fam <- multinom(K = 2)
    ## location parameter
    ## link
    f <- extract_link(fam, parameter = "location", which_eta = 1L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)
    f <- extract_link(fam, parameter = "mu", which_eta = 1L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[1L]]$linkfun(val))
    expect_identical(f, fam$linfo[[1L]]$linkfun)

    ## error if no `which_eta`
    expect_error(extract_link(fam, parameter = "mu"),
                 "Which linear predictor not specified; see 'which_eta'",
                 fixed = TRUE)

    ## inverse
    f <- extract_link(fam, parameter = "location", inverse = TRUE,
                      which_eta = 2L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
    f <- extract_link(fam, parameter = "mu", inverse = TRUE,
                      which_eta = 2L)
    expect_type(f, "closure")
    expect_identical(f(val), fam$linfo[[2L]]$linkinv(val))
    expect_identical(f, fam$linfo[[2L]]$linkinv)
})

## test internal link functions fail gracefully
test_that("gaussian_link() fails gracefully", {
    expect_error(gaussian_link(1), "'family' is not a family object")
    expect_error(gaussian_link(nb()), "'family' is not of type '\"gaussian\"'")
})

## test internal link functions fail gracefully
test_that("poisson_link() fails gracefully", {
    expect_error(poisson_link(1), "'family' is not a family object")
    expect_error(poisson_link(nb()), "'family' is not of type '\"poisson\"'")
})

## test internal link functions fail gracefully
test_that("binomial_link() fails gracefully", {
    expect_error(binomial_link(1), "'family' is not a family object")
    expect_error(binomial_link(nb()), "'family' is not of type '\"binomial\"'")
})

## test internal link functions fail gracefully
test_that("gamma_link() fails gracefully", {
    expect_error(gamma_link(1), "'family' is not a family object")
    expect_error(gamma_link(nb()), "'family' is not of type '\"Gamma\"'")
})

## test internal link functions fail gracefully
test_that("inverse_gaussian_link() fails gracefully", {
    expect_error(inverse_gaussian_link(1), "'family' is not a family object")
    expect_error(inverse_gaussian_link(nb()), "'family' is not of type '\"inverse.gaussian\"'")
})

## test internal link functions fail gracefully
test_that("quasi_link() fails gracefully", {
    expect_error(quasi_link(1), "'family' is not a family object")
    expect_error(quasi_link(nb()), "'family' is not of type '\"quasi\"'")
})

## test internal link functions fail gracefully
test_that("quasi_poisson_link() fails gracefully", {
    expect_error(quasi_poisson_link(1), "'family' is not a family object")
    expect_error(quasi_poisson_link(nb()), "'family' is not of type '\"quasipoisson\"'")
})

## test internal link functions fail gracefully
test_that("quasi_binomial_link() fails gracefully", {
    expect_error(quasi_binomial_link(1), "'family' is not a family object")
    expect_error(quasi_binomial_link(nb()), "'family' is not of type '\"quasibinomial\"'")
})

## test internal link functions fail gracefully
test_that("nb_link() fails gracefully", {
    expect_error(nb_link(1), "'family' is not a family object")
    expect_error(nb_link(tw()), "'family' is not of type '\"Negative Binomial\"'")
})

## test internal link functions fail gracefully
test_that("tw_link() fails gracefully", {
    expect_error(tw_link(1), "'family' is not a family object")
    expect_error(tw_link(nb()), "'family' is not of type '\"Tweedie\"'")
})

## test internal link functions fail gracefully
test_that("beta_link() fails gracefully", {
    expect_error(beta_link(1), "'family' is not a family object")
    expect_error(beta_link(nb()), "'family' is not of type '\"Beta regression\"'")
})

## test internal link functions fail gracefully
test_that("scaled_t_link() fails gracefully", {
    expect_error(scaled_t_link(1), "'family' is not a family object")
    expect_error(scaled_t_link(nb()), "'family' is not of type '\"scaled t\"'")
})

## test internal link functions fail gracefully
test_that("ocat_link() fails gracefully", {
    expect_error(ocat_link(1), "'family' is not a family object")
    expect_error(ocat_link(nb()), "'family' is not of type '\"Ordered Categorical\"'")
})

## test internal link functions fail gracefully
test_that("zip_link() fails gracefully", {
    expect_error(zip_link(1), "'family' is not a family object")
    expect_error(zip_link(nb()), "'family' is not of type '\"zero inflated Poisson\"'")
})

## test internal link functions fail gracefully
test_that("cox_ph_link() fails gracefully", {
    expect_error(cox_ph_link(1), "'family' is not a family object")
    expect_error(cox_ph_link(nb()), "'family' is not of type '\"Cox PH\"'")
})

## test internal link functions fail gracefully
test_that("gaulss_link() fails gracefully", {
    expect_error(gaulss_link(1), "'family' is not a family object")
    expect_error(gaulss_link(nb()), "'family' is not of type '\"gaulss\"'")
})

## test internal link functions fail gracefully
test_that("twlss_link() fails gracefully", {
    expect_error(twlss_link(1), "'family' is not a family object")
    expect_error(twlss_link(nb()), "'family' is not of type '\"twlss\"'")
})

## test internal link functions fail gracefully
test_that("gevlss_link() fails gracefully", {
    expect_error(gevlss_link(1), "'family' is not a family object")
    expect_error(gevlss_link(nb()), "'family' is not of type '\"gevlss\"'")
})

## test internal link functions fail gracefully
test_that("gammals_link() fails gracefully", {
    expect_error(gammals_link(1), "'family' is not a family object")
    expect_error(gammals_link(nb()), "'family' is not of type '\"gammals\"'")
})

## test internal link functions fail gracefully
test_that("ziplss_link() fails gracefully", {
    expect_error(ziplss_link(1), "'family' is not a family object")
    expect_error(ziplss_link(nb()), "'family' is not of type '\"ziplss\"'")
})

## test internal link functions fail gracefully
test_that("mvn_link() fails gracefully", {
    expect_error(mvn_link(1), "'family' is not a family object")
    expect_error(mvn_link(nb()), "'family' is not of type '\"Multivariate normal\"'")
})

## test internal link functions fail gracefully
test_that("multinom_link() fails gracefully", {
    expect_error(multinom_link(1), "'family' is not a family object")
    expect_error(multinom_link(nb()), "'family' is not of type '\"multinom\"'")
})

## test other gamm4 family utils
test_that("family.gamm4 works for a gamm4 object", {
    fam <- family(m_gamm4)
    expect_s3_class(fam, class = "family")
    expect_equal(fam, gaussian())
})

test_that("family.gamm4 throws an error when passed a non-gamm4 object", {
    expect_error(family(l),
                 regexp = "`object` does not appear to a `gamm4` model object",
                 fixed = TRUE)
})

## test gamm family
test_that("family.gamm works for a gamm object", {
    fam <- family(m_gamm)
    expect_s3_class(fam, class = "family")
    expect_equal(fam, gaussian())
})

