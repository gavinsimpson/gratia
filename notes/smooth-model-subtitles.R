# Run from the package root with an output directory as the first argument:
# Rscript notes/smooth-model-subtitles.R /tmp/gratia-model-subtitles
devtools::load_all(quiet = TRUE)
library("mgcv")
library("ggplot2")

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else tempdir()
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(128)
d <- data.frame(x = runif(600), z = runif(600))
d$y <- rnbinom(600, mu = exp(1 + sin(6 * d$x)),
  size = exp(1 + cos(6 * d$x)))
d$y[runif(600) < plogis(-1 + 2 * cos(6 * d$x))] <- 0
m_tmb <- glmmTMB::glmmTMB(y ~ s(x, k = 4), ziformula = ~s(x, k = 4),
  dispformula = ~s(x, k = 4), family = glmmTMB::nbinom2(), data = d,
  control = glmmTMB::glmmTMBControl(optimizer = optim,
    optArgs = list(method = "BFGS")))
p_tmb <- draw(m_tmb, ncol = 3) +
  patchwork::plot_annotation(title = "glmmTMB: three model parts")

# The same Tweedie data show the different parameter orders in mgcv and GJRM.
set.seed(14)
d$y <- mgcv::rTweedie(exp(sin(6 * d$x)), p = 1.5, phi = exp(.3 * d$z))
m_mgcv <- gam(list(y ~ s(x, k = 4), ~ s(z, k = 4), ~ s(z, k = 4)),
  family = twlss(), data = d, method = "REML")
p_mgcv <- draw(m_mgcv, ncol = 3) +
  patchwork::plot_annotation(title = "mgcv: Tweedie distributional parameters")

# GJRM renamed margin to family in development versions.
distribution_arg <- if ("family" %in% names(formals(GJRM::gamlss))) {
  "family"
} else "margin"
m_gjrm <- do.call(GJRM::gamlss, c(list(formula = list(y ~ s(x, k = 4),
  ~ s(z, k = 4), ~ s(z, k = 4)), data = d),
  setNames(list("TW"), distribution_arg)))
p_gjrm <- draw(m_gjrm, ncol = 3) +
  patchwork::plot_annotation(title = "GJRM: Tweedie distributional parameters")

set.seed(821)
d$fac <- factor(rep(c("a", "b"), 300))
d$y <- sin(6 * d$x) + rnorm(nrow(d), sd = exp(.4 * d$z))
m_by <- gam(list(y ~ fac + s(x, by = fac, k = 5), ~ s(z, k = 5)),
  family = gaulss(), data = d)
p_by <- draw(m_by, grouped_by = TRUE, parametric = FALSE, ncol = 2) +
  patchwork::plot_annotation(title = "mgcv: Gaussian parameters and grouped by smooths")

for (name in c("tmb", "mgcv", "gjrm", "by")) {
  ggsave(file.path(output_dir, paste0(name, ".png")), get(paste0("p_", name)),
    width = 12, height = 3.8, dpi = 140, bg = "white")
}
combined <- patchwork::wrap_plots(lapply(list(p_tmb, p_mgcv, p_gjrm),
  function(p) patchwork::wrap_elements(full = p)), ncol = 1)
ggsave(file.path(output_dir, "model-subtitles.png"), combined,
  width = 12, height = 11.4, dpi = 140, bg = "white")

# Several smooths in one predictor do not trigger model subtitles.
single_formula <- y ~ s(x, k = 5) + s(z, k = 5)
single_mgcv <- gam(single_formula, data = d)
single_tmb <- glmmTMB::glmmTMB(single_formula, data = d)
single_gjrm <- GJRM::gamlss(list(single_formula), data = d)
p_single <- patchwork::wrap_plots(
  draw(single_mgcv, select = 1, wrap = FALSE)[[1]] + labs(caption = "mgcv"),
  draw(single_tmb, select = 1, wrap = FALSE)[[1]] + labs(caption = "glmmTMB"),
  draw(single_gjrm, select = 1, parametric = FALSE)[[1]] + labs(caption = "GJRM"),
  ncol = 3) + patchwork::plot_annotation(
    title = "Single-predictor models: no model subtitles")
ggsave(file.path(output_dir, "single-predictor.png"), p_single,
  width = 12, height = 3.8, dpi = 140, bg = "white")
