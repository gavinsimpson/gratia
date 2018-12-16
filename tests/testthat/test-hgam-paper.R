## Tests for models in the HGAM paper

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("datasets")
library("vdiffr")
theme_set(theme_grey())

context("test-hgam-paper-models")

## data load and prep
data(CO2, package = "datasets")
CO2 <- transform(CO2, Plant_uo = factor(Plant, ordered=FALSE))
data(bird_move, package = "gratia")
ctrl <- gam.control(nthreads = 2)

## tests
## CO2
test_that("draw() can plot CO2 model 1", {
    skip_on_cran()
    skip_on_travis()
    CO2_mod1 <- gam(log(uptake) ~ s(log(conc), k = 5, bs = "tp") +
                        s(Plant_uo, k = 12, bs = "re"),
                    data = CO2, method = "REML", family = gaussian(),
                    control = ctrl)
    plt <- draw(CO2_mod1, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-1", plt)
})

test_that("draw() can plot CO2 model 2", {
    skip_on_cran()
    skip_on_travis()
    suppressWarnings(
        CO2_mod2 <- gam(log(uptake) ~ s(log(conc), k = 5, m = 2) +
                            s(log(conc), Plant_uo, k = 5,  bs = "fs", m = 2),
                        data = CO2, method = "REML", family = gaussian(),
                        control = ctrl)
    )
    plt <- draw(CO2_mod2, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-2", plt)
})

## We show smooths 1, 14, 3, 5, 10, 13 in the paper code
test_that("draw() can plot CO2 model 3", {
    skip_on_cran()
    CO2_mod3 <- gam(log(uptake) ~ s(log(conc), k = 5, m = 2, bs = "tp") +
                        s(log(conc), by = Plant_uo, k = 5, m = 1, bs = "tp") +
                        s(Plant_uo, bs = "re", k = 12),
                    data = CO2, method = "REML",
                    control = ctrl)
    plt <- draw(CO2_mod3, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-3", plt)
})

test_that("draw() can plot CO2 model 4", {
    skip_on_cran()
    skip_on_travis()
    CO2_mod4 <- gam(log(uptake) ~ s(log(conc), Plant_uo, k=5, bs="fs", m=2),
                    data=CO2, method="REML",
                    control = ctrl)
    plt <- draw(CO2_mod4, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-4", plt)
})

test_that("draw() can plot CO2 model 5", {
    skip_on_cran()
    skip_on_travis()
    CO2_mod5 <- gam(log(uptake) ~ s(log(conc), by = Plant_uo, k = 5, bs = "tp", m = 2) +
                        s(Plant_uo, bs="re", k=12),
                    data = CO2, method = "REML",
                    control = ctrl)
    plt <- draw(CO2_mod5, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-5", plt)
})

## bird_move
test_that("draw() can plot bird_move model 1", {
    skip_on_cran()
    skip_on_travis()
    bird_mod1 <- gam(count ~ te(week, latitude, bs=c("cc", "tp"), k = c(10, 10)),
                     data = bird_move, method = "REML", family = poisson(),
                     knots = list(week = c(0, 52)),
                     control = ctrl)
    plt <- draw(bird_mod1)
    expect_doppelganger("hgam-paper-bird-move-model-1", plt)
})

test_that("draw() can plot bird_move model 2", {
    skip_on_cran()
    skip_on_travis()
    bird_mod2 <- gam(count ~ te(week, latitude, bs=c("cc", "tp"),
                                k = c(10, 10), m = 2) +
                         t2(week, latitude, species, bs = c("cc", "tp", "re"),
                            k = c(10, 10, 6), m = 2, full = TRUE),
                     data = bird_move, method = "REML", family = poisson(),
                     knots = list(week = c(0, 52)),
                     control = ctrl)
    plt <- draw(bird_mod2)
    expect_doppelganger("hgam-paper-bird-move-model-2", plt)
})

test_that("draw() can plot bird_move model 3", {
    skip_on_cran()
    ## skip_on_travis()
    bird_mod3 <- gam(count ~ species +
                         te(week, latitude, bs = c("cc", "tp"),
                            k = c(10, 10), m = 2) +
                         te(week, latitude, by = species, bs = c("cc", "tp"),
                            k = c(10, 10), m = 1),
                     data = bird_move, method = "REML", family = poisson(),
                     knots = list(week = c(0, 52)),
                     control = ctrl)
    plt <- draw(bird_mod3)
    expect_doppelganger("hgam-paper-bird-move-model-3", plt)
})

test_that("draw() throws message with bird_move model 4", {
    skip_on_cran()
    skip_on_travis()
    bird_mod4 <- gam(count ~ t2(week, latitude, species, bs = c("cc", "tp", "re"),
                                k = c(10, 10, 6), m = c(2, 2, 2)),
                     data = bird_move, method = "REML", family = poisson(),
                     knots = list(week = c(0, 52)),
                     control = ctrl)
    ## There's nothing we can currently do, as
    expect_message(draw(bird_mod4), "Unable to draw any of the model terms.",
                   fixed = FALSE)
})

test_that("draw() can plot bird_move model 5", {
    skip_on_cran()
    skip_on_travis()
    bird_mod5 <- gam(count ~ species +
                         te(week, latitude, by = species,
                            bs = c("cc", "tp"), k = c(10, 10), m = 2),
                     data = bird_move, method = "REML", family = poisson(),
                     knots = list(week = c(0, 52)),
                     control = ctrl)
    plt <- draw(bird_mod5)
    expect_doppelganger("hgam-paper-bird-move-model-5", plt)
})
