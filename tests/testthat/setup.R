# Setup models for tests
library("mgcv")
library("gamm4")
library("scam")
library("dplyr")

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

# A distributed lag model example
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
