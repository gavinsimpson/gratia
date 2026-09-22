conditional_difference_fixture <- function(family = stats::gaussian()) {
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
}

# Reconstruct the two explicit prediction scenarios from returned covariates.
conditional_difference_endpoints <- function(cd, model) {
  covars <- setdiff(model_vars(model), attr(cd, "by"))
  a <- b <- as.data.frame(cd[covars])
  for (nm in attr(cd, "by")) {
    prefix <- if (length(attr(cd, "by")) == 1) ".level" else nm
    a[[nm]] <- cd[[paste0(prefix, "_1")]]
    b[[nm]] <- cd[[paste0(prefix, "_2")]]
  }
  list(a = a, b = b)
}
