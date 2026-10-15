# Plot the main internal summaries. Arguments: CSV directory, figure directory.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
library("ggplot2")
x <- read.csv(file.path(args[1L], "smoothing-coverage-summary.csv"))
cases <- c("normal_known", "normal_weak", "normal_null", "poisson_sparse",
  "binomial_rare", "nb", "beta", "student")
x <- subset(x, stratum == "interior" & case %in% cases)
attempted <- unique(x$attempted)
stopifnot(length(attempted) == 1L)
x$case <- factor(x$case, levels = cases)
x$fitter <- factor(paste(x$engine, x$method),
  levels = c("gam ML", "gam REML", "bam ML", "bam REML", "bam fREML"))
x$sampler <- factor(x$sampler, levels = c("conditional", "unconditional", "mixture"))
x$lower <- pmax(0, x$coverage - 1.96 * x$coverage_mcse)
x$upper <- pmin(1, x$coverage + 1.96 * x$coverage_mcse)
dir.create(args[2L], recursive = TRUE, showWarnings = FALSE)
for (quantity in c("link", "derivative")) {
  d <- x[x$quantity == quantity, ]
  plot <- ggplot(d, aes(fitter, coverage, colour = sampler, shape = sampler)) +
    geom_hline(yintercept = .95, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .25, position = position_dodge(.65)) +
    geom_point(position = position_dodge(.65), size = 1.5) +
    geom_text(data = subset(d, sampler == "mixture" & returned < attempted),
      aes(label = paste0("n=", returned)), vjust = -1.1, size = 2.3,
      position = position_nudge(x = .65 / 3), show.legend = FALSE) +
    facet_wrap(~case, ncol = 4L, scales = "free_y", labeller = as_labeller(c(normal_known = "Gaussian: known dispersion", normal_weak = "Gaussian: weak smooth", normal_null = "Gaussian: no smooth", poisson_sparse = "Sparse Poisson", binomial_rare = "Rare binomial", nb = "Negative binomial", beta = "Beta", student = "Student-t"))) +
    scale_colour_manual(values = c("#0072B2", "#009E73", "#D55E00")) +
    scale_y_continuous(labels = scales::label_percent(accuracy = .1)) +
    labs(x = NULL, y = "Coverage among returned intervals", colour = NULL, shape = NULL,
      title = if (quantity == "link") "Linear predictor: 95% pointwise intervals" else
        "Derivative of the first smooth: 95% pointwise intervals",
      subtitle = "Interior grid average; bars show 1.96 Monte Carlo standard errors across independent datasets",
      caption = paste0(attempted, " datasets per case. Panels have different vertical scales. Labels show mixture availability below ", attempted, ".\nUse paired comparisons in the report when a method fails; separate successful subsets can change apparent coverage.")) +
    theme_bw(base_size = 10) + theme(legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
  ragg::agg_png(file.path(args[2L], paste0("smoothing-coverage-", quantity, ".png")), width = 2600, height = 1550, res = 200)
  print(plot)
  dev.off()
}
