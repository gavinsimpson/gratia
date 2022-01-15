# Setup models for tests
library("testthat")
library("mgcv")
library("gamm4")
library("scam")
library("dplyr")
library("nlme")

## Need a local wrapper to allow conditional use of vdiffr
`expect_doppelganger` <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, ...)
}

## Fit models
quick_eg1 <- data_sim("eg1", n = 200, seed = 1)
su_eg1 <- data_sim("eg1", n = 1000,  dist = "normal", scale = 2, seed = 1)
su_eg2 <- data_sim("eg2", n = 5000, dist = "normal", scale = 1, seed = 1)
su_eg3 <- data_sim("eg3", n = 400, seed = 32)
su_eg4 <- data_sim("eg4", n = 400,  dist = "normal", scale = 2, seed = 1)

su_m_quick_eg1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
                      data = su_eg1,
                      method = "REML")

su_m_univar_4 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
                     data = su_eg1,
                     method = "REML")

su_m_penalty <- gam(y ~ s(x0, bs = 'cr') + s(x1, bs = 'bs') +
                      s(x2, k = 15) + s(x3, bs = 'ps'),
                    data = su_eg1,
                    method = "REML")

su_m_bivar <- gam(y ~ s(x, z, k = 40),
                  data = su_eg2,
                  method = "REML")

su_m_factor_by <- gam(y ~ fac + s(x2, by = fac) + s(x0),
                      data = su_eg4,
                      method = "REML")

su_m_factor_by_x2 <- gam(y ~ fac + s(x2, by = fac),
                         data = su_eg4,
                         method = "REML")

su_gamm_univar_4 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3),
                         data = su_eg1,
                        method = "REML")

m_1_smooth <- gam(y ~ s(x0), data = quick_eg1, method = "REML")
m_gam <- su_m_univar_4
# m_gam    <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = su_eg1,
#                 method = "REML")
m_gamm <- su_gamm_univar_4
# m_gamm   <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = su_eg1,
#                  method = "REML")
m_bam    <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = su_eg1,
                method = "fREML")
m_gamgcv <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = su_eg1,
                method = "GCV.Cp")
m_gamm4  <- gamm4(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = su_eg1,
                  REML = TRUE)

#-- A standard GAM with a simple random effect ---------------------------------
su_re <- su_eg1
set.seed(42)
su_re$fac <- as.factor(sample(seq_len(20), 1000, replace = TRUE))
su_re$X <- model.matrix(~ fac - 1, data = su_re)
su_re <- transform(su_re, y = y + X %*% rnorm(20) * 0.5)
rm1 <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) + s(x3),
           data = su_re, method = "ML")

#-- A distributed lag model example --------------------------------------------
su_dlnm <- su_eg1 %>%
  mutate(f_lag = cbind(dplyr::lag(f, 1),
                       dplyr::lag(f, 2),
                       dplyr::lag(f, 3),
                       dplyr::lag(f, 4),
                       dplyr::lag(f, 5)),
         lag = matrix(1:5, ncol = 5)) %>%
  filter(!is.na(f_lag[, 5]))

# fit DLNM GAM
dlnm_m <- gam(y ~ te(f_lag, lag), data = su_dlnm,
              method = "REML")

#-- An AR(1) example using bam() with factor by --------------------------------
# from ?magic
## simulate truth
set.seed(1)
n <- 400
sig <- 2
x <- 0:(n-1)/(n-1)
## produce scaled covariance matrix for AR1 errors...
rho <- 0.6
V <- corMatrix(Initialize(corAR1(rho), data.frame(x = x)))
Cv <- chol(V)  # t(Cv) %*% Cv=V
## Simulate AR1 errors ...
e1 <- t(Cv) %*% rnorm(n, 0, sig) # so cov(e) = V * sig^2
e2 <- t(Cv) %*% rnorm(n, 0, sig) # so cov(e) = V * sig^2
## Observe truth + AR1 errors
f1 <- 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f2 <- (1280 * x^4) * (1- x)^4
df <- data.frame(x = rep(x, 2), f = c(f1, f2), y = c(f1 + e1, f2 + e2),
                 series = as.factor(rep(c("A", "B"), each = n)))
#rm(x, f1, f2, e1, e2, V, Cv)
AR.start <- rep(FALSE, n*2)
AR.start[c(1, n+1)] <- TRUE
## fit GAM using `bam()` with known correlation
## first just to a single series
m_ar1 <- bam(y ~ s(x, k = 20), data = df[seq_len(n), ], rho = rho,
             AR.start = NULL)
## now as a factor by smooth to model both series
m_ar1_by <- bam(y ~ series + s(x, k = 20, by = series), data = df, rho = rho,
                AR.start = AR.start)