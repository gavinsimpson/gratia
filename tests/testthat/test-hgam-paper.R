## Tests for models in the HGAM paper

## load packages - delete a lot of these as we don't explicitly need them
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("datasets")
## library("MASS")
## library("stringr")
library("gamm4")
## library("tidyr")
## library("viridis")
## library("cowplot")
## library("kableExtra")
## library("docxtools")
## library("knitr")
## library("tibble")
## library("dplyr")
library("vdiffr")
theme_set(theme_grey())

context("test-hgam-paper-models")

## data load and prep
data(CO2, package = "datasets")
CO2 <- transform(CO2, Plant_uo = factor(Plant, ordered=FALSE))

##bird_move <- read.csv(system.file("data", "bird_move.csv", package = "gratia"))
data(bird_move, package = "gratia")

## fit models
## CO2
CO2_mod1 <- gam(log(uptake) ~ s(log(conc), k = 5, bs = "tp") +
                    s(Plant_uo, k = 12, bs = "re"),
                data = CO2, method = "REML", family = gaussian())

suppressWarnings(
    CO2_mod2 <- gam(log(uptake) ~ s(log(conc), k = 5, m = 2) +
                        s(log(conc), Plant_uo, k = 5,  bs = "fs", m = 2),
                    data = CO2, method = "REML", family = gaussian())
)

CO2_mod3 <- gam(log(uptake) ~ s(log(conc), k = 5, m = 2, bs = "tp") +
                  s(log(conc), by = Plant_uo, k = 5, m = 1, bs = "tp") +
                  s(Plant_uo, bs = "re", k = 12),
                data = CO2, method = "REML")

CO2_mod4 <- gam(log(uptake) ~ s(log(conc), Plant_uo, k=5, bs="fs", m=2),
                data=CO2, method="REML")

CO2_mod5 <- gam(log(uptake) ~ s(log(conc), by = Plant_uo, k = 5, bs = "tp", m = 2) +
                    s(Plant_uo, bs="re", k=12),
                data = CO2, method = "REML")

## bird_move
bird_mod1 <- gam(count ~ te(week, latitude, bs=c("cc", "tp"), k = c(10, 10)),
                 data = bird_move, method = "REML", family = poisson(),
                 knots = list(week = c(0, 52)))

## bird_mod2 <- gam(count ~ te(week, latitude, bs=c("cc", "tp"),
##                             k = c(10, 10), m = 2) +
##                      t2(week, latitude, species, bs = c("cc", "tp", "re"),
##                         k = c(10, 10, 6), m = 2, full = TRUE),
##                  data = bird_move, method = "REML", family = poisson(),
##                  knots = list(week = c(0, 52)))

## bird_mod3 <- gam(count ~ species +
##                      te(week, latitude, bs = c("cc", "tp"), k = c(10, 10), m = 2) +
##                      te(week, latitude, by = species, bs = c("cc", "tp"), k = c(10, 10), m = 1),
##                  data = bird_move, method = "REML", family = poisson(),
##                  knots = list(week = c(0, 52)))

## bird_mod4 <- gam(count ~ t2(week, latitude, species, bs = c("cc", "tp", "re"),
##                             k = c(10, 10, 6), m = c(2, 2, 2)),
##                  data = bird_move, method = "REML", family = poisson(),
##                  knots = list(week = c(0, 52)))

## bird_mod5 <- gam(count ~ species + te(week, latitude, by = species,
##                                       bs= c("cc", "tp"), k = c(10, 10), m=2),
##                  data = bird_move, method = "REML", family = poisson(),
##                  knots = list(week = c(0, 52)))

## tests
## CO2
test_that("draw() can plot CO2 model 1", {
    plt <- draw(CO2_mod1, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-1", plt)
})

test_that("draw() can plot CO2 model 2", {
    plt <- draw(CO2_mod2, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-2", plt)
})

## We show smooths 1, 14, 3, 5, 10, 13 in the paper code
test_that("draw() can plot CO2 model 3", {
    plt <- draw(CO2_mod3, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-3", plt)
})

test_that("draw() can plot CO2 model 4", {
    plt <- draw(CO2_mod4, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-4", plt)
})

test_that("draw() can plot CO2 model 5", {
    plt <- draw(CO2_mod5, inc_mean = TRUE)
    expect_doppelganger("hgam-paper-co2-model-5", plt)
})

## bird_move
## test_that("draw() can plot bird_move model 1", {
##     plt <- draw(bird_mod1)
##     expect_doppelganger("hgam-paper-bird-move-model-1", plt)
## })

## test_that("draw() can plot bird_move model 2", {
##     plt <- draw(bird_mod2)
##     expect_doppelganger("hgam-paper-bird-move-model-2", plt)
## })
