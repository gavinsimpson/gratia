# Test glmmTMB support
library("glmmTMB")
data("sleepstudy", package = "lme4")

suppressWarnings(m_glmmTMB <- glmmTMB(Reaction ~ s(log(Days + 1), k = 4),
    data = sleepstudy, REML = TRUE))
