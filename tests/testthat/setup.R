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

# Fit reusable test models only when a test first requests them.
# The cache belongs to this setup environment and is shared across test files.
shared_test_fixture <- local({
  models <- new.env(parent = emptyenv())
  function(key, code) {
    if (!exists(key, envir = models, inherits = FALSE)) models[[key]] <- code
    models[[key]]
  }
})

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

delayedAssign("su_gamm_bivar_te", gamm(y ~ te(x, z, k = c(5, 5)),
  data = su_eg2, method = "REML"
))

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

delayedAssign("m_fs_gamm", gamm(
  y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20),
  data = df_fs, method = "ML"
))

delayedAssign("m_fs_gamm4", gamm4::gamm4(
  y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20),
  data = df_fs
))

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
# Arbitrary SOS coordinate names must retain latitude/longitude ordering.
sos_named_df <- setNames(as.data.frame(sos_df), c("north", "east", "y"))
m_sos_named <- gam(y ~ s(north, east, bs = "sos", k = 20), data = sos_named_df)
sos_by_df <- sos_named_df
sos_by_df$region <- factor(rep(c("a", "b"), length.out = nrow(sos_by_df)))
m_sos_by <- gam(y ~ region + s(north, east, bs = "sos", by = region, k = 15),
  data = sos_by_df)



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

# Shared component fixtures use a smaller PDE grid than the visual examples.
m_soap_sep <- gam(
  y ~ s(v, w, k = 10, bs = "sf", xt = list(bnd = soap_fsb, nmax = 40)) +
    s(v, w, k = 10, bs = "sw", xt = list(bnd = soap_fsb, nmax = 40)),
  data = soap_data, method = "REML", knots = soap_knots
)

# A nested domain with a known inner boundary exercises component offsets.
soap_component_boundary <- list(
  data.frame(v = cos(seq(0, 2 * pi, length.out = 61)),
    w = sin(seq(0, 2 * pi, length.out = 61))),
  data.frame(v = 0.3 * cos(seq(0, 2 * pi, length.out = 31)),
    w = 0.3 * sin(seq(0, 2 * pi, length.out = 31)), f = 0.5)
)
soap_component_data <- withr::with_seed(375, {
  d <- data.frame(v = runif(200, -1, 1), w = runif(200, -1, 1))
  keep <- mgcv::in.out(soap_component_boundary, as.matrix(d))
  d <- d[keep, ]
  d$y <- d$v + d$w^2 + rnorm(nrow(d), sd = 0.1)
  d
})
soap_component_knots <- expand.grid(v = seq(-0.8, 0.8, length.out = 6),
  w = seq(-0.8, 0.8, length.out = 6))
soap_component_knots <- soap_component_knots[
  mgcv::in.out(soap_component_boundary, as.matrix(soap_component_knots)), ]
m_soap_sep_known <- gam(
  y ~ s(v, w, k = 8, bs = "sf",
    xt = list(bnd = soap_component_boundary, nmax = 40)) +
    s(v, w, k = 8, bs = "sw",
      xt = list(bnd = soap_component_boundary, nmax = 40)),
  data = soap_component_data, method = "REML", knots = soap_component_knots
)

# Soap components can also form the bivariate margins of tensor products.
soap_tensor_data <- withr::with_seed(376,
  transform(soap_data, time = runif(nrow(soap_data))))
m_soap_tensor <- gam(
  y ~ te(v, w, time, d = c(2, 1), k = c(10, 4), bs = c("sf", "cr"),
    xt = list(list(bnd = soap_fsb, nmax = 40), NULL)) +
    te(v, w, time, d = c(2, 1), k = c(10, 4), bs = c("sw", "cr"),
      xt = list(list(bnd = soap_fsb, nmax = 40), NULL)),
  data = soap_tensor_data, method = "REML", knots = soap_knots
)

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

# Small, shared integration fixtures use the actual upstream gamm4 class.
# Preserve the test session's RNG while generating responses and fitting.
withr::with_seed(11011, {
  gamm4_data <- data.frame(
    x = runif(180), z = runif(180),
    fac = factor(rep(c("a", "b"), 90)),
    group = factor(rep(seq_len(12), each = 15)),
    off = rep(c(0.2, 0.5, 0.8), 60),
    weight = rep(c(1, 2), 90)
  )
  eta <- sin(4 * gamm4_data$x) + 0.7 * gamm4_data$z +
    0.8 * (gamm4_data$fac == "b") +
    seq(-1.5, 1.5, length.out = 12)[gamm4_data$group]
  gamm4_data$y <- eta + rnorm(180, sd = 0.4)
  gamm4_data$binary <- rbinom(180, 1, plogis(eta))
  gamm4_data$count <- rpois(180, exp(eta / 2 + gamm4_data$off))
  gamm4_gaussian <- gamm4::gamm4(y ~ fac + z + s(x, k = 5),
    random = ~(1 | group), data = gamm4_data)
  gamm4_binomial <- gamm4::gamm4(binary ~ fac + z + s(x, k = 5),
    random = ~(1 | group), data = gamm4_data, family = binomial())
  gamm4_poisson <- gamm4::gamm4(count ~ fac + z + s(x, k = 5) + offset(off),
    random = ~(1 | group), data = gamm4_data, family = poisson())
  gamm4_surface <- gamm4::gamm4(y ~ fac + t2(x, z, k = c(4, 4)),
    random = ~(1 | group), data = gamm4_data)
  gamm4_by <- gamm4::gamm4(y ~ fac + s(x, by = fac, k = 5),
    random = ~(1 | group), data = gamm4_data)
  gamm4_na_data <- gamm4_data
  gamm4_na_data$x[c(8, 14)] <- NA_real_
  gamm4_missing <- gamm4::gamm4(y ~ fac + z + s(x, k = 5),
    random = ~(1 | group), data = gamm4_na_data, weights = weight,
    subset = seq_len(160), na.action = na.exclude)
})


# Small grouped-family models shared by fitted draws and response simulation.
# Interleaved family membership exposes accidental training-order assumptions.
withr::with_seed(269272, {
  gfam_data <- data.frame(
    x = runif(180), family_id = rep(1:3, 60),
    off = rep(c(-0.2, 0.1, 0.3), 60),
    weight = rep(c(3, 2, 1), 60)
  )
  gfam_data$group <- factor(gfam_data$family_id)
  eta <- 0.2 + sin(4 * gfam_data$x) / 2 + gfam_data$off
  gfam_data$y <- numeric(180)
  i <- gfam_data$family_id == 1L
  gfam_data$y[i] <- rbinom(sum(i), gfam_data$weight[i], plogis(eta[i])) /
    gfam_data$weight[i]
  i <- gfam_data$family_id == 2L
  gfam_data$y[i] <- eta[i] + rnorm(sum(i), sd = 0.4 / sqrt(gfam_data$weight[i]))
  i <- gfam_data$family_id == 3L
  gfam_data$y[i] <- rpois(sum(i), exp(eta[i]))
  gfam_model <- gam(cbind(y, family_id) ~ group + s(x, k = 5) + offset(off),
    data = gfam_data, weights = weight, method = "REML",
    family = gfam(list(binomial(), gaussian(), poisson())))
  gfam_bam <- bam(cbind(y, family_id) ~ group + s(x, k = 5) + offset(off),
    data = gfam_data, weights = weight, method = "fREML", discrete = TRUE,
    family = gfam(list(binomial(), gaussian(), poisson())))
  gfam_matrix_data <- gfam_data
  gfam_matrix_data$response <- cbind(gfam_data$y, gfam_data$family_id)
  gfam_matrix_model <- gam(response ~ group + s(x, k = 5) + offset(off),
    data = gfam_matrix_data, weights = weight, method = "REML",
    family = gfam(list(binomial(), gaussian(), poisson())))
  gfam_missing_data <- gfam_data
  gfam_missing_data$x[c(7, 16)] <- NA_real_
  gfam_missing_model <- gam(cbind(y, family_id) ~ group + s(x, k = 5) + offset(off),
    data = gfam_missing_data, weights = weight, subset = seq_len(165),
    na.action = na.exclude, method = "REML", optimizer = c("outer", "bfgs"),
    family = gfam(list(binomial(), gaussian(), poisson())))
  gfam_gaussian_data <- data.frame(x = runif(160), family_id = rep(1:2, 80))
  gfam_gaussian_data$y <- sin(4 * gfam_gaussian_data$x) +
    rnorm(160, sd = c(0.25, 1.2)[gfam_gaussian_data$family_id])
  gfam_gaussian_model <- gam(cbind(y, family_id) ~ s(x, k = 5),
    data = gfam_gaussian_data, method = "REML",
    family = gfam(list(gaussian(), gaussian())))
})

withr::with_seed(269273, {
  gfam_extended_data <- data.frame(x = runif(240), family_id = rep(1:4, 60))
  gfam_extended_data$group <- factor(gfam_extended_data$family_id)
  mu <- exp(0.5 + sin(4 * gfam_extended_data$x) / 3)
  gfam_extended_data$y <- numeric(240)
  i <- gfam_extended_data$family_id == 1L
  gfam_extended_data$y[i] <- rnbinom(sum(i), mu = mu[i], size = 3)
  i <- gfam_extended_data$family_id == 2L
  gfam_extended_data$y[i] <- log(mu[i]) + rnorm(sum(i), sd = 0.35)
  i <- gfam_extended_data$family_id == 3L
  gfam_extended_data$y[i] <- rnbinom(sum(i), mu = mu[i], size = 1.5)
  i <- gfam_extended_data$family_id == 4L
  gfam_extended_data$y[i] <- mgcv::rTweedie(mu[i], p = 1.5, phi = 0.6)
  gfam_extended_model <- gam(cbind(y, family_id) ~ group + s(x, k = 5),
    data = gfam_extended_data, method = "REML",
    family = gfam(list(nb(), gaussian(), nb(), tw())))
})


# A scalar ordinal response is supported by gfam, while its response prediction
# probabilities cannot be represented as one numeric fitted mean per row.
withr::with_seed(107, {
  gfam_ordinal_data <- data.frame(x = runif(180), family_id = rep(1:2, 90))
  gfam_ordinal_data$group <- factor(gfam_ordinal_data$family_id)
  eta <- -0.4 + gfam_ordinal_data$x
  gfam_ordinal_data$y <- numeric(nrow(gfam_ordinal_data))
  i <- which(gfam_ordinal_data$family_id == 1)
  latent <- eta[i] + qlogis(runif(length(i)))
  gfam_ordinal_data$y[i] <- findInterval(latent, c(-Inf, -1, -0.2, 1, Inf))
  i <- which(gfam_ordinal_data$family_id == 2)
  gfam_ordinal_data$y[i] <- rpois(length(i), exp(eta[i]))
  gfam_ordinal_model <- gam(cbind(y, family_id) ~ group + s(x, k = 4),
    data = gfam_ordinal_data, family = gfam(list(ocat(R = 4), poisson())),
    method = "REML")
})


# Incomplete categorical support, shared by prediction-message regressions.
withr::with_seed(164, {
  factor_support_data <- expand.grid(
    a = factor(c("A", "B")), b = factor(c("low", "high")), replicate = 1:30
  )
  factor_support_data$x <- runif(nrow(factor_support_data))
  factor_support_data$y <- as.integer(factor_support_data$a) +
    as.integer(factor_support_data$b) + sin(3 * factor_support_data$x) +
    rnorm(nrow(factor_support_data), sd = 0.2)
  factor_support_data$group <- factor(rep(1:10, length.out = nrow(factor_support_data)))
  factor_support_group_effect <- rnorm(nlevels(factor_support_data$group), sd = 0.5)
  factor_support_data$y <- factor_support_data$y +
    factor_support_group_effect[as.integer(factor_support_data$group)]
  factor_support_data$keep <- with(factor_support_data, !(a == "B" & b == "low"))
  factor_support_observed <- subset(factor_support_data, keep)
  factor_support_character <- factor_support_observed
  factor_support_character$a <- as.character(factor_support_character$a)
  factor_support_character$b <- as.character(factor_support_character$b)
  factor_support_formula <- y ~ a + b + s(x, k = 5)
  factor_support_models <- list(
    gam = gam(factor_support_formula, data = factor_support_observed, method = "REML"),
    character = gam(factor_support_formula, data = factor_support_character, method = "REML"),
    interaction = gam(y ~ a * b + s(x, k = 5), data = factor_support_observed, method = "REML"),
    bam = bam(factor_support_formula, data = factor_support_observed),
    discrete = bam(factor_support_formula, data = factor_support_observed, discrete = TRUE),
    gamm = gamm(factor_support_formula, data = factor_support_observed, method = "REML"),
    gamm4 = gamm4(factor_support_formula, data = factor_support_observed, random = ~(1 | group))
  )
  factor_support_subset <- gam(factor_support_formula, data = factor_support_data,
    subset = keep, method = "REML")
  factor_support_missing_data <- factor_support_data
  factor_support_missing_data$y[!factor_support_missing_data$keep] <- NA_real_
  factor_support_missing <- gam(factor_support_formula, data = factor_support_missing_data,
    na.action = na.exclude, method = "REML")
  factor_support_observed$j <- as.integer(factor_support_observed$a) - 1L
  factor_support_transformed <- gam(y ~ factor(j %% 2) + b + s(x, k = 5),
    data = factor_support_observed, method = "REML")
  factor_support_local <- local({
    foo <- function(j) factor(j %% 2)
    list(model = gam(y ~ foo(j) + b + s(x, k = 5),
      data = factor_support_observed, method = "REML"), envir = environment())
  })
  factor_support_three_data <- expand.grid(
    a = factor(c("A", "B")), b = factor(c("low", "high")),
    c = factor(c("left", "right")), replicate = 1:15
  )
  factor_support_three_data <- subset(factor_support_three_data,
    !(a == "B" & b == "high" & c == "right"))
  factor_support_three_data$x <- runif(nrow(factor_support_three_data))
  factor_support_three_data$y <- as.integer(factor_support_three_data$a) +
    as.integer(factor_support_three_data$b) + as.integer(factor_support_three_data$c) +
    sin(factor_support_three_data$x) + rnorm(nrow(factor_support_three_data), sd = 0.2)
  factor_support_three <- gam(y ~ a + b + c + s(x, k = 5),
    data = factor_support_three_data, method = "REML")
  factor_support_observed$category <- sample(1:3, nrow(factor_support_observed), replace = TRUE)
  factor_support_observed$binary <- factor(sample(0:1, nrow(factor_support_observed), replace = TRUE))
  factor_support_response <- gam(category ~ a + b + s(x, k = 5),
    data = factor_support_observed, family = ocat(R = 3), method = "REML")
  factor_support_binary <- gam(binary ~ a + s(x, k = 5),
    data = factor_support_observed, family = binomial(), method = "REML")
})

# Small shared fixtures for random-effect tensor plotting (#169).
withr::with_seed(169, {
  tensor_re_data <- data.frame(x = runif(220, 0.1, 1), z = runif(220),
    u = runif(220), f = factor(rep(c("c", "a", "b", "d"), 55),
      levels = c("c", "a", "b", "d")),
    g = ordered(rep(c("B", "A"), each = 110), levels = c("B", "A")))
  tensor_re_data$y <- sin(4 * tensor_re_data$x) + tensor_re_data$z +
    as.numeric(tensor_re_data$f) / 4 + rnorm(220, sd = 0.2)
  tensor_re_models <- lapply(c("te", "ti", "t2"), function(constructor) {
    gam(as.formula(paste0("y ~ s(u, k = 4) + ", constructor,
      "(f, x, z, bs = c('re', 'tp', 'tp'), k = c(4, 3, 3))")),
      data = tensor_re_data, method = "REML")
  })
  names(tensor_re_models) <- c("te", "ti", "t2")
  tensor_re_four <- gam(y ~ ti(x, z, f, g,
    bs = c("tp", "tp", "re", "re"), k = c(3, 3, 4, 3)),
    data = tensor_re_data, method = "REML")
  tensor_re_slices <- gam(y ~ t2(f, x, z, u,
    bs = c("re", "tp", "tp", "tp"), k = c(4, 3, 3, 3)),
    data = tensor_re_data, method = "REML")
  tensor_re_curves <- gam(y ~ te(f, x, g,
    bs = c("re", "tp", "re"), k = c(4, 3, 3)),
    data = tensor_re_data, method = "REML")
  tensor_re_numeric <- gam(y ~ ti(x, z, u,
    bs = c("tp", "tp", "re"), k = c(3, 3, 3)),
    data = tensor_re_data, method = "REML")
  tensor_re_raw <- gam(y ~ ti(f, log(x), z,
    bs = c("re", "tp", "tp"), k = c(4, 3, 3)),
    data = tensor_re_data, method = "REML")
})

withr::with_seed(1691, {
  tensor_re_category_data <- expand.grid(f = factor(c("c", "a", "b"),
      levels = c("c", "a", "b")), g = ordered(c("B", "A"), levels = c("B", "A")),
    h = factor(c("ii", "i"), levels = c("ii", "i")), replicate = 1:5)
  tensor_re_category_data$y <- rnorm(nrow(tensor_re_category_data))
  tensor_re_categories <- gam(y ~ te(f, g, h, bs = "re", k = c(3, 3, 3)),
    data = tensor_re_category_data, method = "REML")
})



withr::with_seed(1693, {
  tensor_re_many_data <- data.frame(x = runif(170),
    f = factor(rep(sprintf("level%02d", 1:17), 10)))
  tensor_re_many_data$y <- sin(4 * tensor_re_many_data$x) + rnorm(170)
  tensor_re_many <- gam(y ~ te(x, f, bs = c("tp", "re"), k = c(3, 17)),
    data = tensor_re_many_data, method = "REML")
})

withr::with_seed(1695, {
  tensor_re_numeric_surface <- gam(y ~ te(u, x, z,
    bs = c("re", "tp"), d = c(1, 2), k = c(3, 6)),
    data = tensor_re_data, method = "REML")
})

# Shared MRF fixtures: area order differs from the penalty parameterization;
# the final fitted-domain area has no observations in one model.
mrf_models <- local(withr::with_seed(903, {
  areas <- c("r4", "r1", "r7", "r2", "r8", "r3", "r6", "r5")
  nb <- stats::setNames(lapply(seq_along(areas), function(i) {
    areas[intersect(seq_along(areas), c(i - 1L, i + 1L))]
  }), areas)
  d <- data.frame(area = factor(rep(areas, each = 30), levels = areas),
    x = runif(240), fac = factor(rep(c("B", "A"), 120), levels = c("B", "A")),
    z = runif(240, 0.5, 1.5))
  d$y <- sin(as.integer(d$area)) + d$x + rnorm(240, sd = 0.3)
  S <- diag(vapply(nb, length, integer(1)))
  dimnames(S) <- list(areas, areas)
  for (a in areas) S[a, nb[[a]]] <- -1
  # Two components are a valid intrinsic MRF with multiple null directions.
  disconnected <- S
  disconnected[areas[4], areas[5]] <- disconnected[areas[5], areas[4]] <- 0
  disconnected[cbind(4:5, 4:5)] <- disconnected[cbind(4:5, 4:5)] - 1
  polys <- stats::setNames(lapply(seq_along(areas), function(i) {
    cbind(c(i - 1, i, i, i - 1, i - 1), c(0, 0, 1, 1, 0))
  }), areas)
  xt <- list(nb = nb)
  out <- list(data = d, areas = areas, nb = nb, penalty = S,
    full = gam(y ~ s(area, bs = "mrf", xt = xt) + s(x, k = 5),
      data = d, method = "REML"),
    low = gam(y ~ s(area, bs = "mrf", k = 5, xt = xt),
      data = d, method = "REML"),
    penalty_model = gam(y ~ s(area, bs = "mrf", xt = list(penalty = S)),
      data = d, method = "REML"),
    disconnected = gam(y ~ s(area, bs = "mrf", xt = list(penalty = disconnected)),
      data = d, method = "REML"),
    polygon = gam(y ~ s(area, bs = "mrf", k = 5, xt = list(polys = polys)),
      data = d, method = "REML"),
    by = gam(y ~ fac + s(area, bs = "mrf", by = fac, xt = xt),
      data = d, method = "REML"),
    numeric_by = gam(y ~ s(area, bs = "mrf", by = z, xt = xt),
      data = d, method = "REML"),
    bam = bam(y ~ s(area, bs = "mrf", k = 5, xt = xt),
      data = d, method = "fREML", drop.unused.levels = FALSE))
  absent <- d[as.character(d$area) != areas[8], ]
  absent$area <- droplevels(absent$area)
  out$unobserved <- gam(y ~ s(area, bs = "mrf", k = 5, xt = xt),
    data = absent, knots = list(area = factor(areas)), method = "REML")
  out$unobserved_full <- gam(y ~ s(area, bs = "mrf", xt = xt),
    data = absent, knots = list(area = factor(areas)), method = "REML")
  out$transformed <- gam(y ~ s(factor(area), bs = "mrf", k = 5, xt = xt),
    data = d, method = "REML")
  out
}))

# Compact shared fixtures for smooth simultaneous-band numerical/plot tests.
smooth_band_data <- withr::with_seed(314, local({
  d <- data.frame(x = runif(160), z = runif(160),
    f = factor(rep(c("b", "a"), 80), levels = c("b", "a")))
  d$y <- sin(6 * d$x) + d$z^2 + as.numeric(d$f) + rnorm(160, sd = 0.3)
  d
}))
smooth_band_model <- gam(y ~ f + s(x, k = 8) + s(z, k = 6),
  data = smooth_band_data, method = "REML")
smooth_band_transformed <- gam(y ~ s(log(x + 1), k = 8) + s(z, by = x, k = 6),
  data = smooth_band_data, method = "REML")


# Scalar, uncensored observations from families that also support censoring.
# Shared with diagnostics tests; these references do not simulate censoring.
withr::with_seed(26701, {
  gfam_censored_data <- data.frame(x = runif(120), family_id = rep(1:3, 40))
  gfam_censored_models <- lapply(c("cnorm", "clog", "cpois"), function(name) {
    d <- gfam_censored_data
    d$y <- rnorm(120)
    d$y[d$family_id == 3L] <- rpois(40, 2)
    if (name == "cpois") d$y[d$family_id == 1L] <- rpois(40, 2)
    gam(cbind(y, family_id) ~ factor(family_id) + s(x, k = 4), data = d,
      family = gfam(list(get(name, asNamespace("mgcv"))(), gaussian(), poisson())),
      method = "REML")
  })
  names(gfam_censored_models) <- c("cnorm", "clog", "cpois")
})


# Extended components with fitted shape parameters and custom residual hooks.
withr::with_seed(26702, {
  gfam_custom_models <- lapply(c("betar", "scat"), function(name) {
    d <- data.frame(x = runif(160), family_id = rep(1:2, 80))
    d$y <- rnorm(160)
    d$y[d$family_id == 1L] <- if (name == "betar") rbeta(80, 2, 3) else
      rt(80, 5) * 0.4
    gam(cbind(y, family_id) ~ factor(family_id) + s(x, k = 4), data = d,
      family = gfam(list(get(name, asNamespace("mgcv"))(), gaussian())),
      method = "REML")
  })
  names(gfam_custom_models) <- c("betar", "scat")
})


# Unequal component sizes, disjoint predictors, tied and constant predictors.
# The formula keeps its private fitting data without leaking temporary names.
gfam_rank_model <- local(withr::with_seed(26703, {
  d <- data.frame(family_id = rep(1:3, c(15, 25, 9)),
    x = c(rep(c(0, 1, 2), 5), rep(0:4, 5), rep(0, 9)))
  d$y <- c(-7, 10, 30)[d$family_id] + .2 * d$x + rnorm(nrow(d), sd = .3)
  d$x[c(2, 18)] <- NA_real_
  gam(cbind(y, family_id) ~ factor(family_id) + x, data = d,
    family = gfam(list(gaussian(), gaussian(), gaussian())), method = "REML",
    na.action = na.exclude)
}))

gfam_gamma_model <- local(withr::with_seed(26704, {
  d <- data.frame(family_id = rep(1:3, c(30, 35, 40)), x = runif(105))
  mu <- exp(.2 + .4 * d$x)
  d$y <- numeric(nrow(d))
  i <- d$family_id == 1L
  d$y[i] <- rgamma(sum(i), shape = 4, scale = mu[i] / 4)
  i <- d$family_id == 2L
  d$y[i] <- log(mu[i]) + rnorm(sum(i), sd = .35)
  i <- d$family_id == 3L
  d$y[i] <- rpois(sum(i), mu[i])
  gam(cbind(y, family_id) ~ factor(family_id) + s(x, k = 4), data = d,
    family = gfam(list(Gamma(link = "log"), gaussian(), poisson())), method = "REML")
}))

conditional_difference_fixture <- function(family = stats::gaussian()) {
  shared_test_fixture(paste("conditional-difference", family$family, family$link, sep = ":"), {
    withr::local_seed(45)
    d <- expand.grid(x = seq(0.1, 0.9, length.out = 30),
      a = factor(c("B", "A", "C"), levels = c("B", "A", "C")),
      b = factor(c("low", "high")))
    d$exposure <- runif(nrow(d), 0.5, 2)
    eta <- 0.4 + 0.3 * as.integer(d$a) + sin(d$x * 3) +
      0.2 * (d$b == "high") * as.integer(d$a)
    d$y <- if (family$family == "poisson") {
      rpois(nrow(d), exp(eta) * d$exposure)
    } else eta + rnorm(nrow(d), sd = 0.3)
    m <- mgcv::gam(y ~ a * b + s(x, by = a, k = 5) + offset(log(exposure)),
      data = d, family = family, method = "REML")
    list(model = with_model_envir(m, environment()), data = d)
  })
}

conditional_surface_fixture <- function(family = stats::gaussian()) {
  shared_test_fixture(paste("conditional-surface", family$family, family$link, sep = ":"), {
    withr::local_seed(418)
    d <- data.frame(x = runif(240), z = runif(240), u = runif(240),
      a = factor(rep(c("A", "B", "C"), 80)),
      b = factor(rep(c("low", "high"), each = 120)))
    eta <- 0.3 + sin(4 * d$x) * cos(3 * d$z) +
      as.integer(d$a) * d$x / 3 + 0.2 * (d$b == "high") + d$u
    d$y <- if (family$family == "poisson") rpois(nrow(d), exp(eta)) else
      eta + rnorm(nrow(d), sd = 0.2)
    mgcv::gam(y ~ a + b + s(x, z, by = a, k = 8) + s(u, k = 4),
      data = d, family = family, method = "REML")
  })
}

adjustment_fixture <- function() {
  shared_test_fixture("sampling-covariance-adjustment", {
    withr::local_seed(630)
    d <- data.frame(x = seq(0, 1, length.out = 120))
    d$y <- sin(2 * pi * d$x) + rnorm(120, sd = .5)
    mgcv::gam(y ~ s(x, k = 6), data = d, method = "REML")
  })
}

tube_fixture <- function(bs = "cr", by = FALSE) {
  shared_test_fixture(paste("tube", bs, by, sep = ":"), {
    withr::local_seed(7021)
    d <- data.frame(
      x = seq(0, 1, length.out = 90),
      group = factor(rep(c("a", "b", "c"), 30)), z = rep(1, 90)
    )
    d$y <- sin(5 * d$x) + if (by) as.integer(d$group) * d$x else 0
    d$y <- d$y + rnorm(90, sd = .3)
    model <- if (by) {
      mgcv::gam(y ~ group + s(x, by = group, bs = bs, k = 6), data = d, method = "REML")
    } else {
      mgcv::gam(y ~ s(x, bs = bs, k = 6), data = d, method = "REML")
    }
    list(model = model, data = d)
  })
}

covariate_plot_fixture <- function(n = 240L) {
  shared_test_fixture(paste("residual-covariate", n, sep = ":"), {
    withr::local_seed(912)
    d <- data.frame(x = runif(n, .1, 2), z = runif(n),
      f = factor(rep(c("a", "b", "c"), length.out = n)),
      g = factor(rep(seq_len(12), length.out = n)), exposure = runif(n, 1, 2))
    d$y <- 3 * (d$x - 1)^2 + as.numeric(d$f) + rnorm(n, sd = .3)
    list(data = d, model = mgcv::gam(y ~ x + s(z, k = 5) + f,
      data = d, method = "REML"))
  })
}

conditional_combos_fixture <- function() {
  shared_test_fixture("conditional-factor-combinations", {
    withr::local_seed(42)
    d <- data.frame(
      a = factor(rep(c("a", "a", "b"), c(40, 10, 10))),
      b = factor(rep(c("x", "y", "y"), c(40, 10, 10))),
      x = runif(60)
    )
    d$y <- as.integer(d$a) + as.integer(d$b) + d$x + rnorm(60)
    list(model = gam(y ~ a + b + x, data = d, method = "REML"), data = d)
  })
}

smoothing_research_fixture <- function(method = "REML", bam = FALSE,
                                       linked = FALSE, fixed = FALSE,
                                       tensor = FALSE, known_scale = FALSE) {
  key <- paste("smoothing-research", method, bam, linked, fixed,
    tensor, known_scale, sep = ":")
  shared_test_fixture(key, {
    withr::local_seed(501)
    d <- data.frame(x = runif(180), z = runif(180), w = runif(180, .5, 2))
    d$o <- .2 + .3 * d$x
    d$y <- sin(6 * d$x) + cos(3 * d$z) + d$o + rnorm(180, sd = .3)
    f <- if (tensor) y ~ te(x, z, k = c(4, 4)) + offset(o) else
      if (linked) y ~ s(x, k = 5, id = "same") + s(z, k = 5, id = "same") + offset(o) else
      if (fixed) y ~ s(x, k = 5, sp = .1) + s(z, k = 5) + offset(o) else
        y ~ s(x, k = 5) + s(z, k = 5) + offset(o)
    fitter <- if (bam) mgcv::bam else mgcv::gam
    fit <- fitter(f, data = d, weights = w, method = method,
      scale = if (known_scale) .09 else 0)
    list(model = fit, data = d, formula = f)
  })
}

smoothing_family_fixture <- function(name, engine = "gam", method = "ML",
                                     linked = FALSE, fixed = FALSE, tensor = FALSE,
                                     missing = FALSE, call_offset = FALSE,
                                     censored = TRUE) {
  key <- paste("smoothing-family", name, engine, method, linked,
    fixed, tensor, missing, call_offset, censored, sep = ":")
  shared_test_fixture(key, {
    withr::local_seed(if (name == "ziP") 943 else 941)
    n <- if (name == "ziP") 600L else 320L
    d <- data.frame(x = runif(n), z = runif(n), w = runif(n, .8, 1.2))
    d$o <- .1 + .15 * d$x
    eta <- .2 + 1.2 * sin(6 * d$x) + cos(7 * d$z) + d$o
    family <- switch(name,
      gaussian_log = gaussian("log"), poisson_sqrt = poisson("sqrt"),
      binomial_probit = binomial("probit"), binomial_matrix = binomial(),
      binomial_factor = binomial(), Tweedie = mgcv::Tweedie(p = 1.5),
      negbin = mgcv::negbin(theta = 3), ocat = mgcv::ocat(R = 4),
      ziP = mgcv::ziP(b = .3),
      gfam = mgcv::gfam(list(gaussian(), mgcv::nb())),
      gfam_ocat = mgcv::gfam(list(gaussian(), mgcv::ocat(R = 4))),
      inverse.gaussian = inverse.gaussian("log"),
      if (name %in% c("poisson", "binomial", "Gamma", "quasipoisson", "quasibinomial"))
        get(name, asNamespace("stats"))(link = if (name == "Gamma") "log" else
          if (name %in% c("binomial", "quasibinomial")) "logit" else "log") else
        get(name, asNamespace("mgcv"))())
    # Native families can refer to exported mgcv helpers through the search
    # path. Supply them locally when fitting namespace-only test fixtures.
    family <- sampling_smoothing_family_bindings(family)
    y <- switch(name,
      gaussian_log = exp(eta) + rnorm(n, sd = .12),
      poisson = , poisson_sqrt = , cpois = , quasipoisson = rpois(n, exp(eta)),
      binomial = , quasibinomial = , binomial_matrix = , binomial_probit = {
        trials <- sample(8:15, n, replace = TRUE)
        probability <- if (name == "binomial_probit") pnorm(eta) else plogis(eta)
        success <- rbinom(n, trials, probability)
        d$w <- sample(1:2, n, replace = TRUE)
        if (name == "binomial_matrix") cbind(success, trials - success) else {
          d$w <- d$w * trials
          success / trials
        }
      },
      binomial_factor = {d$w <- sample(1:2, n, replace = TRUE)
        factor(rbinom(n, 1, plogis(eta)), labels = c("no", "yes"))},
      Gamma = rgamma(n, shape = 8, scale = exp(eta) / 8),
      nb = , negbin = rnbinom(n, mu = exp(eta), size = 3),
      betar = rbeta(n, plogis(eta) * 15, (1 - plogis(eta)) * 15),
      scat = eta + .3 * rt(n, df = 5),
      tw = , Tweedie = mgcv::rTweedie(exp(eta), p = 1.5, phi = .2),
      cnorm = eta + rnorm(n, sd = .3),
      clog = eta + rlogis(n, scale = .2),
      bcg = exp(eta + rnorm(n, sd = .3)),
      ocat = , gfam_ocat = 1L + rowSums(outer(eta + rlogis(n), c(-1, .5, 2), `>`)),
      gfam = rnbinom(n, mu = exp(eta), size = 3),
      ziP = {
        d$w <- rep(1, n)
        lambda <- exp(eta)
        probability <- -expm1(-exp(-.5 + 1.5 * eta))
        presence <- runif(n) < probability
        y <- numeric(n)
        y[presence] <- qpois(runif(sum(presence), dpois(0, lambda[presence]), 1),
          lambda[presence])
        y
      },
      inverse.gaussian = {
        mu <- exp(eta); v <- rnorm(n)^2
        a <- mu + mu^2 * v / 20 - mu / 20 * sqrt(40 * mu * v + mu^2 * v^2)
        ifelse(runif(n) <= mu / (mu + a), a, mu^2 / a)
      })
    if (censored && name %in% c("cnorm", "clog", "cpois", "bcg")) {
      limits <- cbind(y, y)
      # Include left, right and interval censoring, without censoring all rows.
      right <- seq_len(n) %% 8L == 0L
      left <- seq_len(n) %% 8L == 1L
      interval <- seq_len(n) %% 8L == 2L
      limits[right, 1] <- y[right] - if (name == "cpois") .5 else .1
      limits[right, 2] <- Inf
      limits[left, 1] <- y[left] + if (name == "cpois") .5 else .1
      limits[left, 2] <- if (name == "bcg") 0 else -Inf
      limits[interval, 1] <- pmax(if (name == "bcg") 1e-4 else -Inf,
        y[interval] - if (name == "cpois") .5 else .05)
      limits[interval, 2] <- y[interval] + if (name == "cpois") .5 else .05
      if (name == "cpois") {
        limits <- cbind(y, y)
        right <- right & y > 4.5
        left <- left & y < 2.5
        interval <- interval & y > 1.5 & y < 5.5
        limits[right, ] <- cbind(rep(4.5, sum(right)), rep(Inf, sum(right)))
        limits[left, ] <- cbind(rep(2.5, sum(left)), rep(-Inf, sum(left)))
        limits[interval, ] <- cbind(rep(1.5, sum(interval)), rep(5.5, sum(interval)))
      }
      y <- limits
    }
    if (name %in% c("gfam", "gfam_ocat")) {
      d$group <- factor(rep(1:2, length.out = n))
      first <- d$group == 1
      y[first] <- eta[first] + rnorm(sum(first), sd = .3)
      y <- cbind(y, as.integer(d$group))
    }
    d$y <- y
    if (missing) d$x[c(5, 18)] <- NA_real_
    f <- if (name %in% c("gfam", "gfam_ocat"))
      y ~ group + s(x, by = group, k = 5) + s(z, by = group, k = 5) else
      if (tensor) y ~ te(x, z, k = c(4, 4)) else
      if (linked) y ~ s(x, k = 5, id = "same") + s(z, k = 5, id = "same") else
      if (fixed) y ~ s(x, k = 5, sp = .1) + s(z, k = 5) else
        y ~ s(x, k = 5) + s(z, k = 5)
    if (!call_offset) f <- update(f, . ~ . + offset(o))
    fitter <- get(engine, asNamespace("mgcv"))
    fit <- fitter(f, data = d, weights = w, family = family, method = method,
      offset = if (call_offset) d$o else NULL, na.action = na.exclude,
      control = mgcv::gam.control(epsilon = 1e-8, maxit = 100L))
    list(model = fit, data = d, formula = f)
  })
}

scasm_sampling_fixture <- function() {
  shared_test_fixture("scasm-sampling", {
    skip_if_not(exists("scasm", asNamespace("mgcv")))
    withr::with_seed(91, {
      d <- data.frame(x = seq(0, 1, length.out = 80),
        off = seq(-0.2, 0.2, length.out = 80))
      d$y <- d$off + rnorm(nrow(d), sd = 0.5)
      mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 7) + offset(off),
        data = d, bs = 6)
    })
  })
}

scasm_interval_fixture <- function(tensor = FALSE) {
  shared_test_fixture(paste("scasm-interval", tensor, sep = ":"), {
    skip_if_not(exists("scasm", asNamespace("mgcv")))
    withr::with_seed(9, {
      d <- data.frame(x = runif(100), z = runif(100), off = seq(-.2, .2, length.out = 100))
      d$y <- d$x + d$z + d$off + rnorm(100, sd = .3)
      form <- if (tensor) {
        y ~ te(x, z, bs = c("sc", "cr"), xt = list("m+", NULL), k = c(5, 4)) + offset(off)
      } else y ~ s(x, bs = "sc", xt = "m+", k = 6) + offset(off)
      mgcv::scasm(form, data = d, bs = 8)
    })
  })
}

scasm_integration_fixture <- function() {
  shared_test_fixture("scasm-integration", {
    withr::with_seed(29, {
      d <- data.frame(x = runif(90), off = seq(-0.2, 0.2, length.out = 90))
      d$y <- d$x + d$off + rnorm(90, sd = 0.3)
      mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6) + offset(off),
        data = d, bs = 8)
    })
  })
}


# Compact sampling references retain their fitted coefficient dimensions.
delayedAssign("sampling_interface_model",
  gam(mpg ~ s(wt, k = 4), data = mtcars))
delayedAssign("sampling_linear_model",
  gam(mpg ~ wt, data = mtcars, method = "REML"))

# The term-variance references deliberately include correlated covariates,
# random slopes, and missing rows. Reuse each fit across its point and draw tests.
variance_total_models <- function() {
  shared_test_fixture("variance-totals", {
    withr::local_preserve_seed()
    d <- variance_terms_data()
    d$y <- 2 * d$x + 2 * d$z + sin(6 * d$w) + (d$fac == "b") +
      d$off + rnorm(nrow(d), sd = 0.3)
    lapply(list(gam, bam), function(fit) {
      fit(y ~ x + z + fac * poly(w, 2) + s(w, k = 6) + offset(off),
        data = d, method = "REML")
    })
  })
}

variance_mixed_fixture <- function(missing = FALSE) {
  shared_test_fixture(paste("variance-mixed", missing, sep = ":"), {
    withr::local_preserve_seed()
    d <- variance_terms_data()
    if (missing) d <- d[sample(nrow(d)), ]
    d$y <- d$y - d$off
    if (missing) d$x[c(4, 40)] <- NA_real_
    models <- if (missing) {
      list(
        gamm(y ~ x + z + fac + s(w, k = 6), random = list(group = ~1 + z),
          data = d, subset = z > 0.1, na.action = na.exclude, method = "REML"),
        gamm4::gamm4(y ~ x + z + fac + s(w, k = 6), random = ~(1 + z | group),
          data = d, subset = z > 0.1, na.action = na.exclude)
      )
    } else {
      list(
        gamm(y ~ x + z + fac + s(w, k = 6), random = list(group = ~1 + z),
          data = d, method = "REML"),
        gamm4::gamm4(y ~ x + z + fac + s(w, k = 6), random = ~(1 + z | group),
          data = d)
      )
    }
    list(models = models, data = d)
  })
}

delayedAssign("variance_scam_model", local({
  withr::local_preserve_seed()
  d <- variance_terms_data()
  scam::scam(y ~ z + s(x, bs = "mpi", k = 6), data = d)
}))

delayedAssign("variance_intercept_model", local({
  withr::local_preserve_seed()
  d <- variance_terms_data()
  gam(y ~ 1, data = d)
}))

shared_bam_surface <- function() {
  shared_test_fixture("bam-surface", {
    bam(y ~ te(x, z, k = c(5, 5)), data = su_eg2, method = "fREML")
  })
}

sampling_test_model <- function(family = stats::gaussian(), bam = FALSE,
                                discrete = FALSE) {
  key <- paste("chain-sampling", family$family, family$link, bam, discrete, sep = ":")
  shared_test_fixture(key, {
    withr::with_seed(913, {
      d <- data.frame(x = runif(60), z = runif(60), off = rep(0.2, 60), w = rep(c(1, 2), 30))
      eta <- sin(2 * pi * d$x) / 2 + d$off
      d$y <- if (family$family == "poisson") rpois(60, exp(eta)) else
        if (family$family == "binomial") rbinom(60, 5, plogis(eta)) / 5 else
        if (family$family == "Gamma") rgamma(60, shape = 3, scale = exp(eta) / 3) else
        if (grepl("negative binomial", family$family, ignore.case = TRUE)) rnbinom(60, mu = exp(eta), size = 2) else
        rnorm(60, eta, 0.5)
      if (family$family == "binomial") d$w <- d$w * 5
      if (bam) mgcv::bam(y ~ s(x, k = 5) + offset(off), data = d,
        family = family, weights = w, discrete = discrete) else
        mgcv::gam(y ~ s(x, k = 5) + offset(off), data = d,
          family = family, weights = w, method = "REML")
    })
  })
}

family_mh_model <- function(family, bam = FALSE, discrete = FALSE) {
  key <- paste("family-target", family$family, family$link, bam, discrete, sep = ":")
  shared_test_fixture(key, {
    withr::with_seed(281, {
      d <- data.frame(x = runif(100), off = runif(100, -0.1, 0.1),
        w = rep(c(1, 2), 50))
      eta <- 0.3 + sin(2 * pi * d$x) / 2 + d$off
      d$y <- switch(family$family,
        gaussian = rnorm(100, eta, 0.5),
        poisson = rpois(100, exp(eta)),
        binomial = rbinom(100, 5, plogis(eta)) / 5,
        Gamma = rgamma(100, 3, scale = exp(eta) / 3),
        rnbinom(100, mu = exp(eta), size = 2))
      if (family$family == "binomial") d$w <- d$w * 5
      if (bam) mgcv::bam(y ~ s(x, k = 5) + offset(off), data = d,
        family = family, weights = w, discrete = discrete) else
        mgcv::gam(y ~ s(x, k = 5) + offset(off), data = d,
          family = family, weights = w, method = "REML")
    })
  })
}

scasm_shape_fixture <- function(tensor = FALSE) {
  key <- paste("scasm-shape", tensor, sep = ":")
  shared_test_fixture(key, {
    withr::with_seed(801, {
      d <- data.frame(x = runif(150), z = runif(150))
      d$y <- exp(d$x) + d$z + rnorm(150, sd = .2)
      form <- if (tensor) {
        y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5),
          xt = list(c("m+", "c+"), NULL), np = FALSE)
      } else y ~ s(x, bs = "sc", xt = c("m+", "c+"), k = 6)
      mgcv::scasm(form, data = d, bs = 20)
    })
  })
}

cyclic_polar_data <- function() {
  d <- data.frame(x = seq(0.1, 0.9, length.out = 80))
  d$z <- rep(seq(0, 1, length.out = 20), 4)
  d$f <- factor(rep(c("a", "b"), each = 40))
  d$y <- sin(2 * pi * d$x) + 0.2 * cos(35 * d$x) + d$z
  d
}

cyclic_polar_model <- function(bs = "cc") {
  key <- paste("cyclic-polar", bs, sep = ":")
  shared_test_fixture(key, {
    mgcv::gam(y ~ s(x, bs = bs, k = 7), data = cyclic_polar_data(),
      knots = list(x = c(0, 1)), method = "REML")
  })
}

surrogate_test_data <- function(n = 180L) {
  withr::with_seed(359, {
    dat <- data.frame(x = runif(n, -2, 2), off = runif(n, -.4, .4),
      w = runif(n, .5, 2))
    latent <- .3 + sin(dat$x) + dat$off + rlogis(n)
    dat$y <- findInterval(latent, c(-1, 0, 2)) + 1L
    dat
  })
}

surrogate_test_fit <- function(dat = surrogate_test_data(),
                               family = mgcv::ocat(R = 4)) {
  fit <- function() {
    mgcv::gam(y ~ s(x, k = 6) + offset(off), data = dat,
      family = family, weights = w, method = "REML")
  }
  if (missing(dat) && missing(family)) {
    shared_test_fixture("surrogate-ordinal", fit())
  } else fit()
}

# The QQ and worm examples use the same trial counts and observed responses.
diagnostic_binomial_data <- function() {
  shared_test_fixture("diagnostic-binomial-data", {
    dat <- data_sim("eg1", n = 200, dist = "binary", scale = .33, seed = 0)
    p <- binomial()$linkinv(dat$f)
    n <- withr::with_seed(0, sample(c(1, 3), 200, replace = TRUE))
    withr::with_seed(0, transform(dat, y = rbinom(n, n, p), n = n))
  })
}

delayedAssign("diagnostic_binomial_model", local({
  dat <- diagnostic_binomial_data()
  gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
    family = binomial, data = dat, weights = n, method = "REML")
}))

response_delta_fixture <- function(family = poisson()) {
  key <- paste("response-delta", family$family, family$link, sep = ":")
  shared_test_fixture(key, {
    withr::local_seed(32)
    d <- data.frame(x = seq(-1, 1, length.out = 400))
    eta <- .8 + .45 * d$x
    d$y <- if (family$family == "poisson") rpois(nrow(d), exp(eta)) else
      if (family$family == "binomial") rbinom(nrow(d), 1, plogis(eta)) else
        eta + rnorm(nrow(d), sd = .3)
    mgcv::gam(y ~ x, data = d, family = family, method = "REML")
  })
}

multinom_surrogate_fit <- function(n = 150L, action = na.omit, missing = FALSE, k = 2L) {
  key <- paste("multinomial-surrogate", n, missing, k,
    paste(deparse(action), collapse = ""), sep = ":")
  shared_test_fixture(key, {
    withr::with_seed(819, {
      d <- data.frame(x = runif(n, -2, 2), z = runif(n), off = rnorm(n, sd = .2))
      mu <- cbind(0, .3 + d$x + d$off)
      if (k == 2L) mu <- cbind(mu, -.4 + d$z - d$x / 2)
      u <- mu - log(-log(matrix(runif(length(mu)), n)))
      d$y <- max.col(u) - 1L
      if (missing) d$x[c(3, 9)] <- NA_real_
      f <- if (k == 2L) list(y ~ x + offset(off), ~ z + x) else list(y ~ x + offset(off))
      mgcv::gam(f, family = mgcv::multinom(K = k), data = d, na.action = action)
    })
  })
}

random_effect_fixture <- function(family = stats::poisson(), fs = FALSE,
                                  bam = FALSE, ...) {
  set.seed(42)
  d <- data.frame(x = stats::runif(240),
    g = factor(rep(letters[1:12], each = 20)),
    h = factor(rep(LETTERS[1:4], 60)))
  eta <- 0.3 + sin(3 * d$x) + stats::rnorm(12, sd = .7)[d$g]
  d$y <- if (family$family == "gaussian") eta + stats::rnorm(240, sd = .3) else
    if (family$family == "binomial") stats::rbinom(240, 1, family$linkinv(eta)) else
      stats::rpois(240, exp(eta))
  formula <- if (fs) y ~ x + s(x, g, bs = "fs", k = 4) else
    y ~ s(x, k = 5) + s(g, bs = "re")
  fit <- if (bam) mgcv::bam else mgcv::gam
  key <- paste("random-effect", family$family, family$link, fs, bam,
    paste(serialize(list(...), NULL), collapse = ""), sep = ":")
  shared_test_fixture(key, {
    model <- fit(formula, data = d, family = family,
      method = if (bam) "fREML" else "REML", ...)
    attr(model, "fixture_data") <- d
    model
  })
}

delayedAssign("dharma_poisson_model",
  glm(count ~ spray, family = poisson(), data = InsectSprays))

tmb_data <- function() {
  set.seed(928)
  d <- data.frame(x = runif(180), z = runif(180),
    g = factor(rep(seq_len(18), 10)))
  d$y <- sin(6 * d$x) + cos(6 * d$z) + rnorm(18)[d$g] + rnorm(180, sd = .3)
  d
}

# Only read-only diagnostics and plots share this native model.
# Covariance comparisons and tests that alter native state fit fresh models.
glmmtmb_gaussian_fixture <- function() {
  shared_test_fixture("glmmtmb-gaussian-random-smooth", {
    withr::local_preserve_seed()
    d <- tmb_data()
    list(
      model = glmmTMB::glmmTMB(y ~ s(x, k = 6) + (1 | g), d, REML = TRUE),
      data = d
    )
  })
}

log_lik_test_data <- function() {
  withr::with_seed(278, {
    d <- data.frame(x = seq(-1, 1, length.out = 72),
      off = rep(c(-.2, .1, .3), 24), w = rep(c(1, 2, 3), 24))
    d$y <- 1 + .4 * d$x + d$off + rnorm(nrow(d), sd = .6)
    d
  })
}

log_lik_ordinary_fixture <- function(name, unit_weights = FALSE) {
  shared_test_fixture(paste("log-lik-ordinary", name, unit_weights, sep = ":"), {
    d <- log_lik_test_data()
    f <- get(name, asNamespace("stats"))(
      link = if (name == "gaussian") "identity" else "log")
    d$y <- switch(name,
      gaussian = d$y,
      poisson = withr::with_seed(1, rpois(nrow(d), exp(.5 + d$x))),
      Gamma = withr::with_seed(2, rgamma(nrow(d), 3, scale = exp(d$x) / 3)),
      inverse.gaussian = withr::with_seed(5, rlnorm(nrow(d), .5 + d$x, .4)))
    mgcv::gam(y ~ x + offset(off), data = d, family = f,
      weights = if (unit_weights) rep(1, nrow(d)) else w, method = "REML")
  })
}

log_lik_grouped_fixture <- function(unit_weights = FALSE) {
  shared_test_fixture(paste("log-lik-grouped", unit_weights, sep = ":"), {
    d <- log_lik_test_data()
    d$family_id <- rep(1:2, nrow(d) / 2)
    d <- withr::with_seed(15, {
      mu <- exp(.2 + .4 * d$x + d$off)
      d$y <- rgamma(nrow(d), 4, scale = mu / 4)
      i <- d$family_id == 2L
      v <- rnorm(sum(i))^2
      a <- mu[i] + mu[i]^2 * v / 20 -
        mu[i] / 20 * sqrt(40 * mu[i] * v + mu[i]^2 * v^2)
      d$y[i] <- ifelse(runif(sum(i)) <= mu[i] / (mu[i] + a), a, mu[i]^2 / a)
      d
    })
    mgcv::gam(cbind(y, family_id) ~ factor(family_id) + x + offset(off),
      data = d, weights = if (unit_weights) rep(1, nrow(d)) else w,
      family = mgcv::gfam(list(Gamma(link = "log"), inverse.gaussian(link = "log"))),
      method = "REML")
  })
}

# Small models needed to distinguish missing intercepts from first coefficients.
intercept_test_model <- function(type = "gaulss") {
  shared_test_fixture(paste0("intercept-", type), {
    switch(type,
      gaulss = gam(list(y ~ s(x0, k = 5), ~ s(x1, k = 5)),
        data = tiny_eg1, family = gaulss(), method = "REML"),
      gaulss_no_location = gam(list(y ~ 0 + s(x0, k = 5), ~ s(x1, k = 5)),
        data = tiny_eg1, family = gaulss(), method = "REML"),
      gaulss_no_scale = gam(list(y ~ s(x0, k = 5), ~ 0 + s(x1, k = 5)),
        data = tiny_eg1, family = gaulss(), method = "REML"),
      # The shared block adds an intercept to both predictors. mgcv drops
      # one redundant column; the scale predictor retains two contributions.
      gaulss_shared = withCallingHandlers(
        gam(list(y ~ x0, ~ s(x2, k = 5), 1 + 2 ~ s(x1, k = 5)),
          data = tiny_eg1, family = gaulss(), method = "REML"),
        warning = function(w) {
          if (grepl("^dropping unidentifiable parametric terms", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }),
      gam = gam(y ~ 0 + x0 + s(x1, k = 5), data = tiny_eg1, method = "REML"),
      gam_linear = gam(y ~ 0 + x0, data = tiny_eg1, method = "REML"),
      glm = glm(y ~ 0 + x0, data = tiny_eg1))
  })
}
