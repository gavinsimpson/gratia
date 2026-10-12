spherical_test_grid <- function(lat = seq(-89.9, 89.9, length.out = 7),
    lon = seq(-179.9, 179.9, length.out = 9)) {
  d <- expand.grid(north = lat, east = lon)
  d$.estimate <- sin(d$east * pi / 180) * cos(d$north * pi / 180)
  d$.se <- rep(0.2, nrow(d))
  d$.lower_ci <- d$.estimate - 0.4
  d$.upper_ci <- d$.estimate + 0.4
  d$.smooth <- "s(north,east)"
  d$.type <- "Spherical spline"
  d$.by <- NA_character_
  class(d) <- c("mgcv_smooth", class(d))
  d
}

spherical_plot <- function(object = spherical_test_grid(), ...) {
  plot_smooth.sos(object, variables = c("north", "east"), ...)
}

expect_valid_spherical_geometry <- function(object) {
  expect_true(all(sf::st_is_valid(object)))
  expect_false(any(sf::st_is_empty(object)))
  expect_true(all(is.finite(unlist(sf::st_geometry(object)))))
}
