library("mgcv")
set.seed(12)
d <- data.frame(x = runif(160), z = runif(160))
d$y <- -d$x - d$z + rnorm(160, sd = 0.01)
for (constructor in c("ti", "t2")) {
  formula <- as.formula(paste0("y ~ ", constructor,
    "(x, z, bs = c('sc', 'sc'), xt = list('m+', 'm+'), k = c(6, 6))"))
  m <- scasm(formula, data = d)
  cat(constructor, ": inequalities = ", nrow(m$Ain),
    "; fitted change = ", diff(predict(m,
      data.frame(x = c(0.25, 0.75), z = 0.5))), "\n", sep = "")
}
