# Setup models for tests
suppressPackageStartupMessages(
  {
    library("mgcv")
    library("gamm4")
    library("scam")
    library("dplyr")
    library("tibble")
    library("nlme")
    library("ggplot2")
  }
)

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
  data = su_eg2[seq_len(100), ],
  method = "REML"
)

su_m_trivar <- gam(y ~ s(x0, x1, x2), data = su_eg1, method = "REML")

su_m_bivar_te <- gam(y ~ te(x, z, k = c(5, 5)), data = su_eg2, method = "REML")

su_m_bivar_ti <- gam(y ~ s(x, k = 5) + s(z, k = 5) + ti(x, z, k = c(5, 5)),
  data = su_eg2, method = "REML"
)

su_m_bivar_t2 <- gam(y ~ t2(x, z, k = c(5, 5)), data = su_eg2, method = "REML")

su_m_trivar_te <- gam(y ~ te(x0, x1, x2, k = c(3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_trivar_t2 <- gam(y ~ t2(x0, x1, x2, k = c(3, 3, 3)),
  data = su_eg1, method = "REML"
)

su_m_cont_by <- gam(y ~ s(x2, by = x1), data = su_eg3, method = "REML")

su_m_factor_by <- gam(y ~ fac + s(x2, by = fac) + s(x0),
  data = su_eg4, method = "REML"
)

# for issue #285
su_m_factor_by_re <- gam(y ~ s(fac, bs = "re") + s(x2) +
    s(x2, by = fac, m = 1) + s(x0),
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

# Also used by the core data_slice tests; only its large model is secondary.
su_eg2_by <- su_eg2 |>
  mutate(y = y + y^2 + y^3) |>
  bind_rows(su_eg2) |>
  mutate(fac = factor(rep(c("A", "B"), each = nrow(su_eg2))))

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

## betar model
sim_betar <- function(n = 400, seed = 3) {
  df <- data_sim("eg1", n = n, seed = seed)
  mu <- binomial()$linkinv(df$f / 4 - 2)
  phi <- 0.5
  a <- mu*phi
  b <- phi - a
  df <- df |> mutate(
    y = withr::with_seed(seed, stats::rbeta(n, a, b)) 
  )
  df
}

m_betar <- gam(
  y ~ s(x0) + s(x1) + s(x2) + s(x3),
  method = "REML",
  family = betar(link="logit"),
  data = sim_betar()
)

## Gammals model
sim_gammals <- function(n = 400, seed = 9) {
  x <- withr::with_seed(seed, runif(4 * n))
  x0 <- x[1:n]
  x1 <- x[(n + 1):(n * 2)]
  x2 <- x[(n * 2 + 1):(n * 3)]
  x3 <- x[(n * 3 + 1):(n * 4)]
  mu <- exp(
    (gw_f0(x0) + gw_f2(x2)) / 5
  )
  theta <- exp(
    gw_f1(x1) / 2 - 2
  )
  tibble::tibble(
    y  = withr::with_seed(
      seed,
      rgamma(n, shape = 1 / theta, scale = mu * theta)
    ),
    x0 = x0,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

m_gammals <- gam(
  list(
    y ~ s(x0) + s(x2),
      ~ s(x1) + s(x3)
  ),
  method = "REML",
  family = gammals(),
  data = sim_gammals()
)

## Gumbls model
sim_gumbls <- function(n = 400, seed = 9) {
  x <- withr::with_seed(seed, runif(4 * n))
  x0 <- x[1:n]
  x1 <- x[(n + 1):(n * 2)]
  x2 <- x[(n * 2 + 1):(n * 3)]
  x3 <- x[(n * 3 + 1):(n * 4)]
  mu <- gw_f0(x0) + gw_f1(x1)
  beta <- exp(gw_f2(x2) / 5)
  tibble::tibble(
    y  = withr::with_seed(
      seed,
      mu - beta * log(-log(runif(n)))
    ),
    x0 = x0,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

m_gumbls <- gam(
  list(
    y ~ s(x0) + s(x1),
      ~ s(x2) + s(x3)
  ),
  method = "REML",
  family = gumbls(),
  data = sim_gammals()
)

# gevlss model
sim_gevlss <- function(n = 500, seed = 9) {
  qf_gev <- function(z, mu, sigma, xi) {
    # GEV inverse cdf
    xi[abs(xi) < 1e-8] <- 1e-8 # approximate xi = 0, by small xi
    x <- mu + ((-log(z))^-xi - 1) * sigma / xi
  }
  x <- withr::with_seed(seed, runif(5 * n)) # 5! 1 extra for y
  x0 <- x[1:n]
  x1 <- x[(n + 1):(n * 2)]
  x2 <- x[(n * 2 + 1):(n * 3)]
  x3 <- x[(n * 3 + 1):(n * 4)]
  y  <- x[(n * 4 + 1):(n * 5)]

  mu  <- gw_f2(x2)
  rho <- gw_f0(x0)
  xi <- (gw_f1(x1) - 4) / 9

  tibble::tibble(
    y = qf_gev(y, mu, exp(rho), xi),
    x0 = x0,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

# this produces a couple of warnings; think some parameter went (close to) out
# of bounds during fitting. Simon uses this example in ?gevlss so it is OK
suppressWarnings(
    m_gevlss <- gam(
    list(
      y ~ s(x2),
        ~ s(x0),
        ~ s(x1)
    ),
    method = "REML",
    family = gevlss(),
    data = sim_gevlss(),
    optimizer = "efs" # for robustness
  )
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

#utils::data(CO2, package = "datasets")
CO2 <- datasets::CO2
## -- rootogram models ---------------------------------------------------------
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
m_fs <- gam(y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20),
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

# A standard GAM with multiple factors
df_2_fac <- withr::with_seed(
  1,
  add_column(
    su_eg4,
    ff = factor(sample(LETTERS[1:4], nrow(su_eg4), replace = TRUE))
  )
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
# utils::data(smallAges, package = "gratia")
smallAges <- gratia::smallAges
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
sim_scam <- function(n, seed = NULL) {
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
sim_sos_eg_data <- function(n = 400, seed = NULL) {
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

m_censor <- gam(y_cens ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = cens_df,
  family = cnorm(), method = "REML"
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

# -- Soap films ---------------------------------------------------------------
# soap film model from ?soap
soap_fs_data <- function(n = 600, bnd, seed = 0) {
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
soap_knots <- data.frame(
  v = rep(seq(-0.5, 3, by = 0.5), 4),
  w = rep(c(-0.6, -0.3, 0.3, 0.6), rep(8, 4))
)
soap_data <- soap_fs_data(bnd = soap_fsb)

m_soap <- gam(
  y ~ s(v, w, k = 30, bs = "so", xt = list(bnd = soap_fsb, nmax = 100)),
  data = soap_data, method = "REML", knots = soap_knots
)

# This dies if you load the sf package - have emailed Simon about it
# m_soap_sep <- gam(
#   y ~ s(v, w, k = 30, bs = "sf", xt = list(bnd = soap_fsb, nmax = 100)) +
#     s(v, w, k = 30, bs = "sw", xt = list(bnd = soap_fsb, nmax = 100)),
#   data = soap_data, method = "REML", knots = soap_knots
# )

# -- End Soap films -----------------------------------------------------------

# Issue 284
df_284 <- data_sim("eg1", seed = 42)
df_284 <- df_284 |>
  mutate(
    month = factor(
      rep(
        month.abb[1:10],
        times = 40),
      levels = month.abb[1:10],
      ordered = TRUE
    ),
    var = as.factor(rep(letters[1:2], 400/2))
  )
m_284 <- gam(
  y ~ month + var + s(x0) + s(x1) + s(x2) + s(x3),
  data = df_284,
  method = "REML"
)

# multivariate normal model
sim_mvn_data <- function(n = 300, seed) {
  # from ?mvn
  V <- matrix(c(2, 1, 1, 2), 2, 2)
  withr::with_seed(seed,
    {
      x0 <- runif(n)
      x1 <- runif(n)
      x2 <- runif(n)
      x3 <- runif(n)
      y <- matrix(0, n, 2)
      # think my $rd can handle this, so get rid of loop by using the $rd from
      # fix_family_rd?
      for (i in 1:n) {
        mu <- c(gw_f0(x0[i]) + gw_f1(x1[i]), gw_f2(x2[i]))
        y[i,] <- mgcv::rmvn(1, mu, V)
      }
    }
  )
  dat <- tibble(
    y0 = y[,1],
    y1 = y[,2],
    x0 = x0,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
  dat
}
mvn_df <- sim_mvn_data(seed = 1234)
m_mvn <- gam(
  list(
    y0 ~ s(x0) + s(x1),
    y1 ~ s(x2) + s(x3)
  ),
  family = mvn(d = 2),
  data = mvn_df
)

## Multinomial model, using example from Simon's ?mgcv::multinom
sim_multinom_data <- function(n = 1000, seed) {
  # from ?mgcv::multinom
  f1 <- function(x) sin(3 * pi * x) * exp(-x)
  f2 <- function(x) x^3
  f3 <- function(x) 0.5 * exp(-x^2) - 0.2
  f4 <- function(x) 1
  withr::with_seed(seed,
    {
      x1 <- runif(n)
      x2 <- runif(n)
      eta1 <- 2 * (f1(x1) + f2(x2)) - 0.5
      eta2 <- 2 * (f3(x1) + f4(x2)) - 1
      p <- exp(cbind(0, eta1, eta2))
      p <- p / rowSums(p) # prob of each category
      cum_p <- t(apply(p, 1, cumsum)) # cumulative probability
      y <- apply(cum_p, 1, \(x) min(which(x > runif(1)))) - 1
    }
  )
  dat <- tibble(
    y  = y,
    x1 = x1,
    x2 = x2
  )
  dat
}
multinom_df <- sim_multinom_data(n = 1000, seed = 12345)
m_multinom <- gam(
  list(
    y ~ s(x1) + s(x2),
      ~ s(x1) + s(x2)
  ),
  family = multinom(K = 2),
  data = multinom_df
)

# ziP() example - issue #341
zip_data <- function(seed = 0, n = 400, theta = c(-2, 0.3)) {
  rzip <- function(gamma, theta = c(-2, 0.3)) {
    ## From ?ziP (c) Simon Wood
    ## generate zero inflated Poisson random variables, where 
    ## lambda = exp(gamma), eta = theta[1] + exp(theta[2])*gamma
    ## and 1-p = exp(-exp(eta)).
    y <- gamma
    n <- length(y)
    lambda <- exp(gamma)
    eta <- theta[1] + exp(theta[2]) * gamma
    p <- 1 - exp(-exp(eta))
    ind <- p > runif(n)
    y[!ind] <- 0
    np <- sum(ind)
    ## generate from zero truncated Poisson, given presence...
    y[ind] <- qpois(runif(np, dpois(0, lambda[ind]), 1), lambda[ind])
    y
  }
  df <- data_sim("eg1", seed = seed, n = n, scale = 2)
  df$y <- withr::with_seed(seed = seed, rzip(df$f / 4 - 1, theta = theta))
  df
}
zip_df <- zip_data(seed = 1)
  m_ziP <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), family = ziP(), data = zip_df)

# Secondary examples are fitted once, on demand, after the CRAN guard.
secondary_models <- make_secondary_models(environment())
