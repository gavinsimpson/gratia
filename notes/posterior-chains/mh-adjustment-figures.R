args <- commandArgs(TRUE)
destination <- if (length(args)) args[1L] else "notes/posterior-chains"
d <- utils::read.csv(file.path(destination, "mh-adjustment-coverage.csv"))
d <- d[d$region == "interior" & d$cohort == "all_returned", ]
samplers <- c("conditional_gaussian", "conditional_mh", "adjusted_mh", "unconditional_gaussian")
labels <- c("Conditional Gaussian", "Conditional MH", "Adjusted MH", "Unconditional Gaussian")
colours <- c("#767676", "#0072B2", "#D55E00", "#009E73")
cases <- c("normal_known", "normal_weak", "normal_null", "binomial_rare")
case_labels <- c("Strong Gaussian smooth", "Weak Gaussian smooth", "Absent Gaussian smooth", "Rare binary outcomes")
grDevices::png(file.path(destination, "mh-adjustment-coverage.png"),
  width = 1800, height = 1500, res = 160)
par(mfrow = c(4, 2), mar = c(3.1, 10.5, 2.8, 1), oma = c(2, 0, 2, 0), las = 1)
for (i in seq_along(cases)) for (engine in c("gam", "bam")) {
  z <- d[d$case == cases[i] & d$engine == engine & d$quantity == "derivative", ]
  z <- z[match(samplers, z$sampler), ]
  y <- rev(seq_along(samplers))
  plot(z$coverage * 100, y, xlim = c(45, 101), ylim = c(.5, 4.5), yaxt = "n",
    xlab = "Coverage (%)", ylab = "", pch = 19, col = colours, cex = 1.1,
    main = paste(case_labels[i], if (engine == "gam") "— GAM REML" else "— BAM fREML"))
  axis(2, at = y, labels = labels, tick = FALSE, cex.axis = .85)
  abline(v = 95, lty = 2, col = "grey40")
  segments(100 * (z$coverage - 1.96 * z$coverage_mcse), y,
    100 * (z$coverage + 1.96 * z$coverage_mcse), y, col = colours, lwd = 1.5)
}
mtext("Pointwise 95% intervals for smooth derivatives; interior evaluation points",
  outer = TRUE, side = 3, cex = 1, line = .4)
mtext("Bars: ±1.96 dataset-level Monte Carlo SE. Dashed line: nominal coverage.",
  outer = TRUE, side = 1, cex = .9, line = .1)
grDevices::dev.off()
grDevices::png(file.path(destination, "mh-adjustment-width.png"),
  width = 1800, height = 1500, res = 160)
par(mfrow = c(4, 2), mar = c(3.1, 10.5, 2.8, 1), oma = c(2, 0, 2, 0), las = 1)
for (i in seq_along(cases)) for (engine in c("gam", "bam")) {
  z <- d[d$case == cases[i] & d$engine == engine & d$quantity == "derivative", ]
  z <- z[match(samplers, z$sampler), ]
  y <- rev(seq_along(samplers))
  limits <- range(c(0, z$width + 2 * z$width_mcse))
  plot(z$width, y, xlim = limits, ylim = c(.5, 4.5), yaxt = "n",
    xlab = "Average interval width", ylab = "", pch = 19, col = colours, cex = 1.1,
    main = paste(case_labels[i], if (engine == "gam") "— GAM REML" else "— BAM fREML"))
  axis(2, at = y, labels = labels, tick = FALSE, cex.axis = .85)
  segments(pmax(0, z$width - 1.96 * z$width_mcse), y,
    z$width + 1.96 * z$width_mcse, y, col = colours, lwd = 1.5)
}
mtext("Widths of pointwise smooth-derivative intervals; interior evaluation points",
  outer = TRUE, side = 3, cex = 1, line = .4)
mtext("Bars: ±1.96 dataset-level Monte Carlo SE. Panel scales differ.",
  outer = TRUE, side = 1, cex = .9, line = .1)
grDevices::dev.off()
