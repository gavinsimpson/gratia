## Tests for models in the HGAM paper

## load packages
# library("testthat")
# library("gratia")
# library("mgcv")
# library("ggplot2")
# library("datasets")

## Need a local wrapper to allow conditional use of vdiffr
# `expect_doppelganger` <- function(title, fig, ...) {
#  testthat::skip_if_not_installed("vdiffr")
#  vdiffr::expect_doppelganger(title, fig, ...)
# }

## data load and prep
data(CO2, package = "datasets")
CO2 <- transform(CO2, Plant_uo = factor(Plant, ordered = FALSE))
data(bird_move, package = "gratia")
data(zooplankton, package = "gratia")
zooplankton <- transform(zooplankton, year_f = factor(year))

## use several threads to speed up some fits
ctrl <- gam.control(nthreads = 3)

## the first training and testing data set will be used to compare dynamics of
## plankton communities in Lake Mendota
zoo_train <- subset(zooplankton, year %% 2 == 0 & lake == "Mendota")
zoo_test <- subset(zooplankton, year %% 2 == 1 & lake == "Mendota")

## The second training and testing set will compare Daphnia mendotae dynamics
## among four lakes
daphnia_train <- subset(zooplankton, year %% 2 == 0 & taxon == "D. mendotae")
daphnia_test <- subset(zooplankton, year %% 2 == 1 & taxon == "D. mendotae")

## tests
## CO2
test_that("draw() can plot CO2 model 1", {
  skip_on_cran()
  skip_on_ci()
  CO2_mod1 <- bam(
    log(uptake) ~ s(log(conc), k = 5, bs = "tp") +
      s(Plant_uo, k = 12, bs = "re"),
    data = CO2, method = "fREML", family = gaussian(),
    control = ctrl
  )
  plt <- draw(CO2_mod1, overall_uncertainty = TRUE, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-co2-model-1", plt)

  expect_silent(d <- derivatives(CO2_mod1))
})

test_that("draw() can plot CO2 model 2", {
  skip_on_cran()
  skip_on_ci()
  expect_warning(
    CO2_mod2 <- bam(
      log(uptake) ~ s(log(conc), k = 5, m = 2) +
        s(log(conc), Plant_uo, k = 5, bs = "fs", m = 2),
      data = CO2, method = "fREML", family = gaussian(),
      control = ctrl
    ),
    "model has repeated 1-d smooths of same variable."
  )
  plt <- draw(CO2_mod2, overall_uncertainty = TRUE, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-co2-model-2", plt)

  expect_silent(d <- derivatives(CO2_mod2))
})

## We show smooths 1, 14, 3, 5, 10, 13 in the paper code
test_that("draw() can plot CO2 model 3", {
  skip_on_cran()
  skip_on_ci()
  CO2_mod3 <- bam(
    log(uptake) ~ s(log(conc), k = 5, m = 2, bs = "tp") +
      s(log(conc), by = Plant_uo, k = 5, m = 1, bs = "tp") +
      s(Plant_uo, bs = "re", k = 12),
    data = CO2, method = "fREML",
    control = ctrl
  )
  plt <- draw(CO2_mod3, overall_uncertainty = TRUE, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-co2-model-3", plt)

  expect_silent(d <- derivatives(CO2_mod3))
})

test_that("draw() can plot CO2 model 4", {
  skip_on_cran()
  skip_on_ci()
  CO2_mod4 <- bam(
    log(uptake) ~
      s(log(conc), Plant_uo, k = 5, bs = "fs", m = 2),
    data = CO2, method = "fREML",
    control = ctrl
  )
  plt <- draw(CO2_mod4, overall_uncertainty = TRUE, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-co2-model-4", plt)

  expect_silent(d <- derivatives(CO2_mod4))
})

test_that("draw() can plot CO2 model 5", {
  skip_on_cran()
  skip_on_ci()
  CO2_mod5 <- bam(
    log(uptake) ~ s(log(conc),
      by = Plant_uo, k = 5, bs = "tp",
      m = 2
    ) +
      s(Plant_uo, bs = "re", k = 12),
    data = CO2, method = "fREML",
    control = ctrl
  )
  plt <- draw(CO2_mod5, overall_uncertainty = TRUE, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-co2-model-5", plt)

  expect_silent(d <- derivatives(CO2_mod5))
})

## bird_move
test_that("draw() can plot bird_move model 1", {
  skip_on_cran()
  skip_on_ci()
  bird_mod1 <- bam(
    count ~ te(week, latitude,
      bs = c("cc", "tp"),
      k = c(10, 10)
    ),
    data = bird_move, method = "fREML", family = poisson(),
    knots = list(week = c(0, 52)),
    control = ctrl, discrete = TRUE
  )
  plt <- draw(bird_mod1, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-bird-move-model-1", plt)
})

test_that("draw() can plot bird_move model 2", {
  skip_on_cran()
  skip_on_ci()
  # expect_warning(
    bird_mod2 <- bam(
      count ~ te(week, latitude,
        bs = c("cc", "tp"),
        k = c(10, 10), m = 2
      ) +
        t2(week, latitude, species,
          bs = c("cc", "tp", "re"),
          k = c(10, 10, 6), m = 2, full = TRUE
        ),
      data = bird_move, method = "fREML", family = poisson(),
      knots = list(week = c(0, 52)),
      control = ctrl, discrete = FALSE
    )#,
    #"fitted rates numerically 0 occurred"
  #)
  plt <- draw(bird_mod2, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-bird-move-model-2", plt)
})

test_that("draw() can plot bird_move model 3", {
  skip_on_cran()
  skip_on_ci()
  bird_mod3 <- bam(
    count ~ species +
      te(week, latitude,
        bs = c("cc", "tp"),
        k = c(10, 10), m = 2
      ) +
      te(week, latitude,
        by = species, bs = c("cc", "tp"),
        k = c(10, 10), m = 1
      ),
    data = bird_move, method = "fREML", family = poisson(),
    knots = list(week = c(0, 52)),
    control = ctrl, discrete = TRUE
  )
  plt <- draw(bird_mod3, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-bird-move-model-3", plt)
})

test_that("draw() throws message with bird_move model 4", {
  skip_on_cran()
  skip_on_ci()
  expect_warning(
    bird_mod4 <- bam(
      count ~ t2(week, latitude, species,
        bs = c("cc", "tp", "re"),
        k = c(10, 10, 6), m = c(2, 2, 2)
      ),
      data = bird_move, method = "fREML", family = poisson(),
      knots = list(week = c(0, 52)),
      control = ctrl, discrete = TRUE
    ),
    "fitted rates numerically 0 occurred"
  )
  ## There's nothing we can currently do, as
  expect_silent(plt <- draw(bird_mod4, n = 25, rug = FALSE))
  expect_doppelganger("hgam-paper-bird-move-model-4", plt)
})

test_that("draw() can plot bird_move model 5", {
  skip_on_cran()
  skip_on_ci()
  bird_mod5 <- bam(
    count ~ species +
      te(week, latitude,
        by = species,
        bs = c("cc", "tp"), k = c(10, 10), m = 2
      ),
    data = bird_move, method = "fREML", family = poisson(),
    knots = list(week = c(0, 52)),
    control = ctrl, discrete = TRUE
  )
  plt <- draw(bird_mod5, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-bird-move-model-5", plt)
})

test_that("draw() can plot zoo_comm_mod model 4", {
  skip_on_cran()
  skip_on_ci()
  zoo_comm_mod4 <- bam(
    density_adj ~ s(day, taxon,
      bs = "fs",
      k = 10,
      xt = list(bs = "cc")
    ) +
      s(taxon, year_f, bs = "re"),
    data = zoo_train,
    knots = list(day = c(0, 365)),
    family = Gamma(link = "log"),
    method = "fREML", discrete = TRUE,
    drop.unused.levels = FALSE, control = ctrl
  )
  plt <- draw(zoo_comm_mod4, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-zoop-model-4", plt)
})

test_that("draw() can plot zoo_comm_mod model 5", {
  skip_on_cran()
  skip_on_ci()
  zoo_comm_mod5 <- bam(
    density_adj ~ s(day,
      by = taxon,
      k = 10, bs = "cc"
    ) +
      s(taxon, bs = "re") +
      s(taxon, year_f, bs = "re"),
    data = zoo_train,
    knots = list(day = c(0, 365)),
    family = Gamma(link = "log"),
    method = "fREML", discrete = TRUE,
    drop.unused.levels = FALSE, control = ctrl
  )
  plt <- draw(zoo_comm_mod5, rug = FALSE, n = 50)
  expect_doppelganger("hgam-paper-zoop-model-5", plt)
})
