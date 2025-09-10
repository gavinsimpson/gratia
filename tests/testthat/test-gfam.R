# testing gratia's ability to handle gfam models - not much yet
test_that("gfam data sim and model works", {
  skip_if_not_installed("mgcv", "1.9-0")
  expect_silent(
    gfam_df <- data_sim("gfam", n = 400, seed = 2,
      gfam_families = c("binary", "tweedie", "normal"))
  )
  expect_silent(
    m_gfam <- gam(cbind(y, index) ~ s(x0) + s(x1) + s(x2) + s(x3),
      family = gfam(list(binomial, tw, gaussian)),
      data = gfam_df, method = "REML")
  )
})

# test something like two species responding to same covariate, but one with
# very different variation
test_that("gratia can handle a two species negbin model with gfam", {
  skip_on_cran()
  # Number of data per species
  N <- 200
  ## simulate first species
  gfam_sp1 <- data_sim(
    "gwf2", scale = 1.5, theta = 2, # lower scale and theta
    n = N, seed = 25, dist = "negbin"
  )
  # simulate species 2:, larger theta and scale
  gfam_sp2 <- data_sim(
    "gwf2", scale = 2, theta = 4, # larger scale and theta
    n = N, seed = 25, dist = "negbin"
  )
  # bind together
  gfam_df <- gfam_sp1 |>
    dplyr::bind_rows(gfam_sp2) |>
    dplyr::mutate(spp = rep(c(1, 2), each = N), f_spp = factor(spp))
  # fit the model
  two_sp_gfam <- gam(
    cbind(y, spp) ~ f_spp + s(x, by = f_spp),
    data = gfam_df,
    method = "REML",
    family = gfam(list("nb", "nb"))
  )
  fam <- family(two_sp_gfam)
  expect_identical(
    fam$getTheta(),
    theta(two_sp_gfam)
  )
})

# needs more tests
test_that("gratia handles a complex gfam bird move", {
  skip_on_cran()
  skip_on_ci()

  data(bird_move, package = "gratia")
  ctrl <- gam.control(nthreads = 8)
  bird_move2 <- bird_move |>
    mutate(
      fam = as.numeric(species) |> as.integer()
    )
  bird_mod2_gfam <- gam(
    cbind(count, fam) ~ te(
      week, latitude, bs = c("cc", "tp"), k = c(10, 10), m = 2
    ) +
    t2(
      week, latitude, species,
      bs = c("cc", "tp", "re"),
      k = c(10, 10, 6), m = 2, full = TRUE
    ),
    data = bird_move2,
    method = "REML",
    family = gfam(list(nb, nb, nb, nb, nb, nb)),
    knots = list(week = c(0, 52)),
    control = ctrl,
  )
  expect_identical(
    family(bird_mod2_gfam)$getTheta(),
    theta(bird_mod2_gfam)
  )
})
