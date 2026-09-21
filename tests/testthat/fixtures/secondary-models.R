# Loaded only by secondary_models(), after skip_on_cran().
# Core data and models are inherited from the shared setup environment.

su_m_quadvar <- gam(y ~ s(x0, x1, x2, x3), data = su_eg1, method = "REML")

su_m_quadvar_te <- gam(y ~ te(x0, x1, x2, x3, k = c(3, 3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_quadvar_t2 <- gam(y ~ t2(x0, x1, x2, x3, k = c(3, 3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_bivar_by_fac <- gam(y ~ fac + s(x, z, k = 40, by = fac),
  data = su_eg2_by, method = "REML"
)

## bcg() family
sim_bcg <- function(n = 400, seed = c(3, 9)) {
  # Simulate some gamma data?
  df <- data_sim("eg1", n = n, dist = "normal", scale = 1, seed = seed[1])
  df <- df |>
    mutate(
      f = f / 4,
      y = withr::with_seed(
        seed[2],
        rgamma(exp(f) * 0, shape = 1 / 0.5, scale = exp(f) * 0.5)
      )
    )
  df
}

m_bcg <- gam(
  y ~ s(x0) + s(x1) + s(x2) + s(x3),
  family = bcg(),
  data = sim_bcg(),
  method = "REML"
)

m_ordered_by <- gam(uptake ~ Plant + s(conc, k = 5) +
  s(conc, by = Plant, k = 5), data = CO2, method = "REML")

# Check no-error with re in ti #358
m_re_interaction <- gam(
  y ~ s(x0) + ti(x0, fac, bs = c("tp", "re"), k = 5) + s(x1, k = 20),
  data = df_fs, method = "ML"
)

# Expect error with re in ti #358
m_re_two_interaction <- gam(
  y ~ s(x0) + ti(x0, x1, fac, bs = c("tp", "tp", "re"), k = 5) + s(x1, k = 20),
  data = df_fs, method = "ML"
)

# -- A distributed lag model example -------------------------------------------
su_dlnm <- su_eg1 |>
  mutate(
    f_lag = cbind(
      dplyr::lag(f, 1),
      dplyr::lag(f, 2),
      dplyr::lag(f, 3),
      dplyr::lag(f, 4),
      dplyr::lag(f, 5)
    ),
    lag = matrix(1:5, ncol = 5)
  ) |>
  filter(!is.na(f_lag[, 5]))

# fit DLNM GAM
dlnm_m <- gam(y ~ te(f_lag, lag),
  data = su_dlnm,
  method = "REML"
)

#- - An AR(1) example using bam() with factor by -------------------------------
# from ?magic
## simulate truth
n <- 400
sig <- 2
df <- withr::with_seed(1, {
  x <- 0:(n - 1) / (n - 1)
  ## produce scaled covariance matrix for AR1 errors...
  rho <- 0.6
  V <- corMatrix(Initialize(corAR1(rho), data.frame(x = x)))
  Cv <- chol(V) # t(Cv) %*% Cv=V
  ## Simulate AR1 errors ...
  e1 <- t(Cv) %*% rnorm(n, 0, sig) # so cov(e) = V * sig^2
  e2 <- t(Cv) %*% rnorm(n, 0, sig) # so cov(e) = V * sig^2
  ## Observe truth + AR1 errors
  f1 <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
  f2 <- (1280 * x^4) * (1 - x)^4
  data.frame(
    x = rep(x, 2), f = c(f1, f2), y = c(f1 + e1, f2 + e2),
    series = as.factor(rep(c("A", "B"), each = n))
  )
})
# rm(x, f1, f2, e1, e2, V, Cv)
AR.start <- rep(FALSE, n * 2)
AR.start[c(1, n + 1)] <- TRUE
## fit GAM using `bam()` with known correlation
## first just to a single series
m_ar1 <- bam(y ~ s(x, k = 20),
  data = df[seq_len(n), ], rho = rho,
  AR.start = NULL
)
## now as a factor by smooth to model both series
m_ar1_by <- bam(y ~ series + s(x, k = 20, by = series),
  data = df, rho = rho,
  AR.start = AR.start
)

# Now add a known boundary condition
soap_fsb2 <- soap_fsb
soap_fsb2[[1]]$f <- mgcv::fs.test(
  soap_fsb2[[1]]$v, soap_fsb2[[1]]$w, b = 1, exclude = FALSE
)

m_soap_bndry <- gam(
  y ~ s(v, w, bs = "so", xt = list(bnd = soap_fsb2, nmax = 100)),
  data = soap_data, method = "REML", knots = soap_knots
)

## --- Nested boundary example ------------------------------------------------

soap_nested_bndry <- function(n = 100, a = 0.3, b = 0.3) {
  bndry <- list(
    list(x = 0, y = 0),
    list(x = 0, y = 0)
  )
  theta <- seq(0, 2 * pi, length = n)
  bndry[[1]]$x <- sin(theta)
  bndry[[1]]$y <- cos(theta)
  bndry[[2]]$x <- a + b * sin(theta)
  bndry[[2]]$y <- a + b * cos(theta)
  bndry
}

soap_nested_knots <- function(n_knots = 8, bndry) {
  y_grid <- x_grid <- seq(-1, 1, length = n_knots)
  x <- rep(x_grid, n_knots)
  y <- rep(y_grid, rep(n_knots, n_knots))
  idx <- mgcv::inSide(bndry, x, y)
  knots <- data.frame(x = x[idx], y = y[idx])
  knots
}

soap_nested_data <- function(n = 300, seed = 1, bndry) {
  f <- function(x, y) {
    exp(-(x - 0.3)^2 - (y - 0.3)^2)
  }
  df <- withr::with_seed(
    seed, {
      x <- runif(n) * 2 - 1
      y <- runif(n) * 2 - 1
      ind <- inSide(bndry, x, y)
      x <- x[ind]
      y <- y[ind]
      n <- length(x)
      z <- f(x, y) + rnorm(n) * 0.1
      tibble(x = x, y = y, z = z, f = f(x, y))
    }
  )
  df
}

sf_nested_bndry <- soap_nested_bndry()
sf_nested_knots <- soap_nested_knots(bndry = sf_nested_bndry)
sf_nested_df <- soap_nested_data(bndry = sf_nested_bndry)

m_soap_nested <- gam(
  z ~ s(
    x, y, k = c(30, 15), bs = "so", xt = list(bnd = sf_nested_bndry, nmax = 60)
  ),
  data = sf_nested_df, method = "REML", knots = sf_nested_knots
)
