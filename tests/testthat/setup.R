# Setup models for tests
library("mgcv")
library("gamm4")
library("scam")
library("dplyr")
library("tibble")
library("nlme")
library("ggplot2")

## Fit models
n_quick <- 300
quick_eg1 <- data_sim("eg1", n = n_quick, seed = 21)
quick_eg1_off <- quick_eg1 |> mutate(off = 2)
tiny_eg1 <- data_sim("eg1", n = 100, seed = 21)
su_eg1 <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 1)
su_eg2 <- data_sim("eg2", n = 2000, dist = "normal", scale = 0.5, seed = 42)
su_eg3 <- data_sim("eg3", n = 400, seed = 32)
su_eg4 <- data_sim("eg4", n = 400, dist = "normal", scale = 2, seed = 1)

su_m_quick_eg1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = quick_eg1,
  method = "REML"
)

m_tiny_eg1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = tiny_eg1,
  method = "REML"
)

su_m_quick_eg1_shrink <- gam(
  y ~ s(x0, bs = "ts") + s(x1, bs = "cs") +
    s(x2, bs = "ts") + s(x3, bs = "ts"),
  data = quick_eg1,
  method = "REML"
)

su_m_univar_4 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = su_eg1,
  method = "REML"
)

su_m_penalty <- gam(
  y ~ s(x0, bs = "cr") + s(x1, bs = "bs") +
    s(x2, k = 15) + s(x3, bs = "ps"),
  data = su_eg1,
  method = "REML"
)

su_m_bivar <- gam(y ~ s(x, z, k = 40), data = su_eg2, method = "REML")

su_m_bivar_ds <- gam(y ~ s(x, z, k = 20, bs = "ds"),
  data = su_eg2,
  method = "REML"
)

su_m_trivar <- gam(y ~ s(x0, x1, x2), data = su_eg1, method = "REML")

su_m_quadvar <- gam(y ~ s(x0, x1, x2, x3), data = su_eg1, method = "REML")

su_m_bivar_te <- gam(y ~ te(x, z, k = c(5, 5)), data = su_eg2, method = "REML")

su_m_bivar_ti <- gam(y ~ s(x, k = 5) + s(z, k = 5) + ti(x, z, k = c(5, 5)),
  data = su_eg2, method = "REML"
)

su_m_bivar_t2 <- gam(y ~ t2(x, z, k = c(5, 5)), data = su_eg2, method = "REML")

su_m_trivar_te <- gam(y ~ te(x0, x1, x2, k = c(3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_quadvar_te <- gam(y ~ te(x0, x1, x2, x3, k = c(3, 3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_trivar_t2 <- gam(y ~ t2(x0, x1, x2, k = c(3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_quadvar_t2 <- gam(y ~ t2(x0, x1, x2, x3, k = c(3, 3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_cont_by <- gam(y ~ s(x2, by = x1), data = su_eg3, method = "REML")

su_m_factor_by <- gam(y ~ fac + s(x2, by = fac) + s(x0),
  data = su_eg4, method = "REML"
)

su_m_factor_by_gamm <- gamm(y ~ fac + s(x2, by = fac) + s(x0),
  data = su_eg4, REML = TRUE
)

su_m_factor_by_gamm4 <- gamm4(y ~ fac + s(x2, by = fac) + s(x0),
  data = su_eg4, REML = TRUE
)

su_m_factor_by_bam <- bam(y ~ fac + s(x2, by = fac) + s(x0), data = su_eg4)

su_m_factor_by_x2 <- gam(y ~ fac + s(x2, by = fac),
  data = su_eg4, method = "REML"
)

su_m_su_eg4 <- gam(y ~ s(x0) + s(x1) + s(x2, by = fac),
  data = su_eg4, method = "REML"
)

if (packageVersion("mgcv") >= "1.8.41") {
  m_sz <- gam(y ~ s(x2) + s(fac, x2, bs = "sz") + s(x0),
    data = su_eg4, method = "REML"
  )

  # two factor sz smooth example from ?smooth.construct.sz.smooth.spec
  ## Example involving 2 factors
  two_factor_sz_example <- function(seed = NULL) {
    ## sort out the seed
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
    f1 <- function(x2) 2 * sin(pi * x2)
    f2 <- function(x2) exp(2 * x2) - 3.75887
    f3 <- function(x2) {
      0.2 * x2^11 * (10 * (1 - x2))^6 + 10 * (10 * x2)^3 *
        (1 - x2)^10
    }

    n <- 600
    x <- runif(n)
    f1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
    f2 <- factor(sample(c("foo", "bar"), n, replace = TRUE))

    mu <- f3(x)
    for (i in 1:3) mu <- mu + exp(2 * (2 - i) * x) * (f1 == levels(f1)[i])
    for (i in 1:2) mu <- mu + 10 * i * x * (1 - x) * (f2 == levels(f2)[i])
    y <- mu + rnorm(n)
    dat <- data.frame(y = y, x = x, f1 = f1, f2 = f2)
    dat
  }
  su_eg_sz_2_factor <- two_factor_sz_example(seed = 42)
  # using bam as it is so much faster
  m_sz_2f <- bam(
    y ~ s(x) + s(f1, x, bs = "sz") + s(f2, x, bs = "sz") +
      s(f1, f2, x, bs = "sz", id = 1),
    data = su_eg_sz_2_factor, method = "fREML"
  )
}

su_eg2_by <- su_eg2 %>%
  mutate(y = y + y^2 + y^3) %>%
  bind_rows(su_eg2) %>%
  mutate(fac = factor(rep(c("A", "B"), each = nrow(su_eg2))))
su_m_bivar_by_fac <- gam(y ~ fac + s(x, z, k = 40, by = fac),
  data = su_eg2_by,
  method = "REML"
)

su_gamm_univar_4 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = su_eg1,
  method = "REML"
)

m_1_smooth <- gam(y ~ s(x0), data = quick_eg1, method = "REML")

m_1_smooth_offset <- gam(y ~ s(x0) + offset(log(off)),
  data = quick_eg1_off, method = "REML"
)

m_gam <- su_m_univar_4

m_gamm <- su_gamm_univar_4

m_bam <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = su_eg1,
  method = "fREML"
)

m_gamgcv <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = su_eg1,
  method = "GCV.Cp"
)

m_gamm4 <- gamm4(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = su_eg1,
  REML = TRUE
)
m_gamm4_real <- m_gamm4
class(m_gamm4_real) <- append("gamm4", class(m_gamm4_real[-1L]))

m_gaulss <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~1),
  data = su_eg1,
  family = gaulss
)

m_scat <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = su_eg1,
  family = scat(), method = "REML"
)

# models with univariate tensor products
m_univar_te <- gam(y ~ te(x2), data = quick_eg1, method = "REML")
m_univar_ti <- gam(y ~ ti(x2), data = quick_eg1, method = "REML")
m_univar_t2 <- gam(y ~ t2(x2), data = quick_eg1, method = "REML")

m_lm <- lm(y ~ x0 + x1 + x2 + x3, data = quick_eg1)

m_glm <- glm(y ~ x0 + x1 + x2 + x3, data = quick_eg1)

data(CO2)
m_ordered_by <- gam(uptake ~ Plant + s(conc, k = 5) +
    s(conc, by = Plant, k = 5), data = CO2, method = "REML")

## -- rootogram models ----------------------------------------------------------
df_pois <- data_sim("eg1", dist = "poisson", n = 500L, scale = 0.2, seed = 42)
## fit the model
b_pois <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df_pois,
  method = "fREML", family = poisson()
)
m_nb <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df_pois,
  method = "REML", family = nb()
)
m_negbin <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df_pois,
  method = "REML", family = negbin(25)
)
m_tw <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df_pois,
  method = "REML", family = tw()
)

## -- fs smooths ----------------------------------------------------------------

## simulate example... from ?mgcv::factor.smooth.interaction
## simulate data...
df_fs <- withr::with_seed(0, {
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x, a = 2, b = -1) exp(a * x) + b
  f2 <- function(x) {
    0.2 * x^11 * (10 * (1 - x))^6 + 10 *
      (10 * x)^3 * (1 - x)^10
  }
  n <- 500
  nf <- 10
  fac <- sample(1:nf, n, replace = TRUE)
  x0 <- runif(n)
  x1 <- runif(n)
  x2 <- runif(n)
  a <- rnorm(nf) * .2 + 2
  b <- rnorm(nf) * .5
  f <- f0(x0) + f1(x1, a[fac], b[fac]) + f2(x2)
  fac <- factor(fac)
  y <- f + rnorm(n) * 2

  data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, fac = fac)
})
mod_fs <- gam(y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20),
  data = df_fs, method = "ML"
)

#-- A standard GAM with a simple random effect ---------------------------------
su_re <- quick_eg1
su_re$fac <- withr::with_seed(
  42,
  as.factor(sample(seq_len(10), n_quick, replace = TRUE))
)
su_re$X <- model.matrix(~ fac - 1, data = su_re)
su_re <- withr::with_seed(42, transform(su_re, y = y + X %*% rnorm(10) * 0.5))
rm1 <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) + s(x3),
  data = su_re, method = "ML"
)

#-- A factor by GAM with random effects ----------------------------------------
su_re2 <- su_eg4
su_re2$ranef <- withr::with_seed(
  42,
  as.factor(sample(1:20, nrow(su_eg4), replace = TRUE))
)
su_re2$X <- model.matrix(~ ranef - 1, data = su_re2)
su_re2 <- withr::with_seed(42, transform(su_re2, y = y + X %*% rnorm(20) * 0.5))
rm2 <- gam(y ~ fac + s(ranef, bs = "re", by = fac) + s(x0) + s(x1) + s(x2),
  data = su_re2, method = "ML"
)

# -- A distributed lag model example -------------------------------------------
su_dlnm <- su_eg1 %>%
  mutate(
    f_lag = cbind(
      dplyr::lag(f, 1),
      dplyr::lag(f, 2),
      dplyr::lag(f, 3),
      dplyr::lag(f, 4),
      dplyr::lag(f, 5)
    ),
    lag = matrix(1:5, ncol = 5)
  ) %>%
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

# A standard GAM with multiple factors
df_2_fac <- withr::with_seed(
  1,
  add_column(su_eg4, ff = factor(sample(LETTERS[1:4],
    nrow(su_eg4),
    replace = TRUE
  )))
)
# a GAM with multiple factor parametric terms
m_2_fac <- gam(y ~ fac * ff + s(x0) + s(x1) + s(x2),
  data = df_2_fac, method = "REML"
)
# a GAM with parametric terms (factor and linear) and smooth terms
m_para_sm <- gam(y ~ fac * ff + x0 + s(x1) + s(x2),
  data = df_2_fac, method = "REML"
)
# a GAM with parametric terms (factor and linear)
m_only_para <- gam(y ~ fac * ff + x0 + x1 + x2,
  data = df_2_fac, method = "REML"
)

# a GAM with weird parametric terms
m_poly <- gam(y ~ fac + ff + log(x0) + x1 + poly(x2, 2, raw = TRUE),
  data = df_2_fac, method = "REML"
)

# -- scam models --------------------------------------------------------------
data(smallAges)
smallAges$Error[1] <- 1.1
sw <- scam(Date ~ s(Depth, k = 5, bs = "mpd"),
  data = smallAges,
  weights = 1 / smallAges$Error, gamma = 1.4
)
sw_mdcx <- scam(Date ~ s(Depth, k = 5, bs = "mdcx"),
  data = smallAges,
  weights = 1 / smallAges$Error, gamma = 1.4
)
sw_mdcv <- scam(Date ~ s(Depth, k = 5, bs = "mdcv"),
  data = smallAges,
  weights = 1 / smallAges$Error, gamma = 1.4
)

# this should be folded into data_sim()
`sim_scam` <- function(n, seed = NULL) {
  ## sort out the seed
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
  # from ?scam, first example
  x1 <- runif(n) * 6 - 3
  f1 <- 3 * exp(-x1^2) # unconstrained term
  x2 <- runif(n) * 4 - 1
  f2 <- exp(4 * x2) / (1 + exp(4 * x2)) # monotone increasing smooth
  y <- f1 + f2 + rnorm(n) * .5
  tibble(x1 = x1, x2 = x2, y = y)
}
scam_dat <- sim_scam(n = 200, seed = 4)
## fit model, get results, and plot...
m_scam <- scam(y ~ s(x1, bs = "cr") + s(x2, bs = "mpi"), data = scam_dat)
m_scam_micx <- scam(y ~ s(x1, bs = "cr") + s(x2, bs = "micx"), data = scam_dat)
m_scam_micv <- scam(y ~ s(x1, bs = "cr") + s(x2, bs = "micv"), data = scam_dat)

# Ordered categorical model ocat()
n_categories <- 4
su_eg1_ocat <- data_sim("eg1",
  n = 200, dist = "ordered categorical",
  n_cat = n_categories, seed = 42
)
m_ocat <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  family = ocat(R = n_categories), data = su_eg1_ocat, method = "REML"
)

# Simon's spline on the sphere example from ?smooth.construct.sos.smooth.spec
`sim_sos_eg_data` <- function(n = 400, seed = NULL) {
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
  f <- function(la, lo) { ## a test function...
    sin(lo) * cos(la - 0.3)
  }
  ## generate with uniform density on sphere...
  lo <- runif(n) * 2 * pi - pi ## longitude
  la <- runif(3 * n) * pi - pi / 2
  ind <- runif(3 * n) <= cos(la)
  la <- la[ind]
  la <- la[seq_len(n)]
  ff <- f(la, lo)
  y <- ff + rnorm(n) * 0.2 ## test data
  out <- tibble(
    latitude = la * 180 / pi,
    longitude = lo * 180 / pi, y = y
  )
  out
}
sos_df <- sim_sos_eg_data(n = 400, seed = 0)
m_sos <- gam(y ~ s(latitude, longitude, bs = "sos", k = 60), data = sos_df)

# censored normal
cens_df <- quick_eg1 |>
  mutate(
    y = y - 5, # shift data down
    censored = case_when(y < 0 ~ -Inf, .default = y)
  )
cens_df$y_cens <- with(cens_df, cbind(y, censored))

m_censor <- bam(y_cens ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = cens_df,
  family = cnorm(), method = "fREML"
)

# examples for logical variables - examples if from mgcViz::pterms
logi_df <- data_sim("eg1", n = 600, dist = "normal", scale = 20, seed = 3) |>
  mutate(
    fac = as.factor(sample(c("A1", "A2", "A3"), 600, replace = TRUE)),
    logi = as.logical(sample(c(TRUE, FALSE), 600, replace = TRUE))
  )
m_logical <- gam(
  y ~ x0 + x1 + I(x1^2) + s(x2, bs = "cr", k = 12) + fac +
    x3:fac + I(x1 * x2) + logi,
  data = logi_df
)

# ziplss example
ziplss_data <- function(seed = 0) {
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x) exp(2 * x)
  f2 <- function(x) {
    0.2 * x^11 * (10 * (1 - x))^6 + 10 *
      (10 * x)^3 * (1 - x)^10
  }
  n <- 500
  df <- withr::with_seed(seed, {
    x0 <- runif(n)
    x1 <- runif(n)
    x2 <- runif(n)
    x3 <- runif(n)

    ## Simulate probability of potential presence...
    eta1 <- f0(x0) + f1(x1) - 3
    p <- binomial()$linkinv(eta1)
    y <- as.numeric(runif(n) < p) ## 1 for presence, 0 for absence

    ## Simulate y given potentially present (not exactly model fitted!)...
    ind <- y > 0
    eta2 <- f2(x2[ind]) / 3
    y[ind] <- rpois(exp(eta2), exp(eta2))
    data.frame(y, x0, x1, x2, x3)
  })
  df
}
ziplss_df <- ziplss_data()
m_ziplss <- gam(list(
  y ~ s(x2) + x3,
  ~ s(x0) + x1
), family = ziplss(), data = ziplss_df)

# TWLSS example from ?mgcv::twlss
twlss_df <- withr::with_seed(3, gamSim(1,
  n = 400, dist = "poisson",
  scale = 0.2, verbose = FALSE
) |>
  mutate(y = mgcv::rTweedie(exp(.data$f),
    p = 1.3,
    phi = 0.5
  ))) ## Tweedie response
## Fit a fixed p Tweedie, with wrong link ...
m_twlss <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~1, ~1),
  family = twlss(), data = twlss_df
)

# -- Models for 2d sz and fs basis smooths #249 -------------------------------
i_m <- c(1, 0.5)
i_xt <- list(bs = "ds", m = i_m)
i_sz <- gam(
  Petal.Width ~ s(Sepal.Length, Sepal.Width, bs = "ds", m = i_m) +
    s(Species, Sepal.Length, Sepal.Width, bs = "sz", xt = i_xt),
  method = "REML",
  data = iris
)

i_fs <- gam(
  Petal.Width ~ s(Sepal.Length, Sepal.Width, bs = "ds", m = i_m) +
    s(Sepal.Length, Sepal.Width, Species, bs = "fs", xt = i_xt),
  method = "REML",
  data = iris
)
rm(i_m, i_xt)

# -- Soap films ---------------------------------------------------------------
# soap film model from ?soap
`soap_fs_data` <- function(n = 600, bnd, seed = 0) {
  df <- withr::with_seed(seed, {
    v <- runif(n) * 5 - 1
    w <- runif(n) * 2 - 1
    y <- mgcv::fs.test(v, w, b = 1)
    ind <- mgcv::inSide(bnd, x = v, y = w) ## remove outsiders
    y <- y + rnorm(n) * 0.3 ## add noise
    y <- y[ind]
    v <- v[ind]
    w <- w[ind]
    tibble(y = y, v = v, w = w)
  })
  df
}

soap_fsb <- list(mgcv::fs.boundary())
names(soap_fsb[[1]]) <- c("v", "w")
soap_knots <- data.frame(v = rep(seq(-0.5, 3, by = 0.5), 4),
  w = rep(c(-0.6, -0.3, 0.3, 0.6), rep(8, 4)))
soap_data <- soap_fs_data(bnd = soap_fsb)
m_soap <- gam(y ~
    s(v, w, k = 30, bs = "so", xt = list(bnd = soap_fsb, nmax = 100)),
  data = soap_data, method = "REML", knots = soap_knots)
