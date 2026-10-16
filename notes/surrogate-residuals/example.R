library("mgcv")
library("gratia")

set.seed(359)
dat <- data.frame(x = runif(400, -2, 2))
latent <- sin(dat$x) + rlogis(nrow(dat))
dat$y <- findInterval(latent, c(-1, 0, 2)) + 1L
fit <- gam(y ~ s(x), data = dat, family = ocat(R = 4), method = "REML")

residuals <- surrogate_residuals(fit, seed = 42)
diagnostics <- appraise(fit, type = "surrogate", seed = 42)
diagnostics

output <- "notes/surrogate-residuals"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(file.path(output, "issue-359-appraisal.png"), diagnostics,
  width = 10, height = 8, dpi = 160)

plots <- patchwork::wrap_plots(
  qq_plot(fit, type = "surrogate", seed = 42),
  worm_plot(fit, type = "surrogate", seed = 42),
  residuals_linpred_plot(fit, type = "surrogate", seed = 42),
  residuals_covariate_plot(fit, covariates = "x", type = "surrogate", seed = 42),
  ncol = 2)
ggplot2::ggsave(file.path(output, "issue-359-diagnostic-plots.png"), plots,
  width = 12, height = 9, dpi = 160)

scalable <- appraise(fit, type = "surrogate", seed = 42, scalable = TRUE)
ggplot2::ggsave(file.path(output, "issue-359-scalable-appraisal.png"), scalable,
  width = 10, height = 8, dpi = 160)
