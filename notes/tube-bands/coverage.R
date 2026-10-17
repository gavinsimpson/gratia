# Refit experiments distinguish Gaussian-process calibration from smoothing bias.
# These checks assess containment on a dense grid, not certified continuum
# containment of the unknown generating function.
pkgload::load_all(quiet = TRUE)
library("mgcv")
library("gratia")
replicates <- as.integer(Sys.getenv("N_REFITS", "200"))
scenarios <- data.frame(
  scenario = c("broad", "broad_corrected", "nearly_linear", "peak",
    "peak_small_basis", "boundary_uneven", "binomial", "poisson", "groups_equal", "groups_different"),
  family = c(rep("gaussian", 6), "binomial", "poisson", "gaussian", "gaussian"),
  k = c(8, 8, 8, 40, 4, 10, 8, 8, 8, 8),
  unconditional = c(FALSE, TRUE, rep(FALSE, 8)))
truth <- function(x, scenario, order = 0L) {
  broad <- switch(order + 1L, sin(2 * pi * x), 2 * pi * cos(2 * pi * x), -4 * pi^2 * sin(2 * pi * x))
  if (scenario == "nearly_linear") return(switch(order + 1L, 2 * x, rep(2, length(x)), rep(0, length(x))) + .0001 * broad)
  if (scenario %in% c("peak", "peak_small_basis")) {
    z <- (x - .6) / .06
    peak <- 2 * exp(-z^2)
    return(broad + switch(order + 1L, peak, -2 * z / .06 * peak, (4 * z^2 - 2) / .06^2 * peak))
  }
  if (scenario == "boundary_uneven") return((-8)^order * exp(-8 * x))
  broad
}
run_scenario <- function(j) {
  spec <- scenarios[j, ]
  results <- list()
  for (replicate in seq_len(replicates)) {
    set.seed(820000 + j * 1000 + replicate)
    x <- if (spec$scenario == "boundary_uneven") c(0, sort(runif(178)^2), 1) else seq(0, 1, length.out = 180)
    if (spec$family != "gaussian") x <- seq(0, 1, length.out = 400)
    d <- data.frame(x = x)
    eta <- truth(x, spec$scenario)
    groups <- grepl("^groups", spec$scenario)
    if (groups) {
      d$group <- factor(rep(c("a", "b"), length.out = length(x)))
      if (spec$scenario == "groups_different") eta <- eta +
        (d$group == "b") * (.6 + .3 * cos(6 * x))
    }
    fam <- switch(spec$family, gaussian = gaussian(), binomial = binomial(), poisson = poisson())
    d$y <- switch(spec$family, gaussian = eta + rnorm(length(x), sd = .3),
      binomial = rbinom(length(x), 1, plogis(eta)), poisson = rpois(length(x), exp(1 + eta)))
    k <- spec$k
    model <- if (groups) gam(y ~ group + s(x, by = group, bs = "cr", k = k),
      data = d, method = "REML") else gam(y ~ s(x, bs = "cr", k = k), data = d, family = fam, method = "REML")
    at <- seq(.02, .98, length.out = 401)
    quantities <- if (groups) "difference" else c("smooth", "derivative1", "derivative2")
    for (quantity in quantities) {
      start <- proc.time()["elapsed"]
      answer <- tryCatch({
        if (quantity == "difference") {
          nd <- expand.grid(x = at, group = levels(d$group))
          band <- difference_smooths(model, select = "s(x)", data = nd,
            group_means = TRUE, interval = "simultaneous", simultaneous_method = "tube")
          target <- if (spec$scenario == "groups_equal") rep(0, length(at)) else -.6 - .3 * cos(6 * at)
          center <- band$.diff
        } else if (quantity == "smooth") {
          band <- smooth_estimates(model, data = data.frame(x = at),
            interval = "simultaneous", simultaneous_method = "tube", unconditional = spec$unconditional)
          target <- truth(at, spec$scenario) - mean(truth(x, spec$scenario))
          center <- band$.estimate
        } else {
          order <- as.integer(sub("derivative", "", quantity))
          band <- derivatives(model, data = data.frame(x = at), order = order,
            type = "central", interval = "simultaneous", simultaneous_method = "tube",
            unconditional = spec$unconditional)
          target <- truth(at, spec$scenario, order)
          center <- band$.derivative
        }
        data.frame(scenario = spec$scenario, replicate = replicate, quantity = quantity,
          covered = all(target >= band$.lower_ci & target <= band$.upper_ci),
          mean_width = mean(band$.upper_ci - band$.lower_ci),
          maximum_error = max(abs(center - target)), error = "")
      }, error = function(e) data.frame(scenario = spec$scenario, replicate = replicate,
        quantity = quantity, covered = NA, mean_width = NA_real_, maximum_error = NA_real_, error = conditionMessage(e)))
      answer$seconds <- unname(proc.time()["elapsed"] - start)
      results[[length(results) + 1L]] <- answer
    }
  }
  result <- do.call(rbind, results)
  write.csv(result, file.path("notes/tube-bands", paste0("refits-", spec$scenario, ".csv")), row.names = FALSE)
  result
}
write.csv(scenarios, "notes/tube-bands/experiment-config.csv", row.names = FALSE)
selected <- Sys.getenv("SCENARIOS", "")
ids <- if (nzchar(selected)) match(strsplit(selected, ",", fixed = TRUE)[[1]], scenarios$scenario) else seq_len(nrow(scenarios))
stopifnot(!anyNA(ids))
if (Sys.getenv("RUN_REFITS", "true") == "true") {
  results <- parallel::mclapply(ids, run_scenario, mc.cores = min(2L, length(ids)))
  if (any(vapply(results, inherits, logical(1), "try-error"))) stop("A refit experiment failed")
}
all <- do.call(rbind, lapply(file.path("notes/tube-bands", paste0("refits-", scenarios$scenario, ".csv")), read.csv))
summaries <- lapply(split(all, interaction(all$scenario, all$quantity, drop = TRUE)), function(d) {
  ok <- !is.na(d$covered)
  ci <- if (any(ok)) binom.test(sum(d$covered[ok]), sum(ok))$conf.int else c(NA, NA)
  data.frame(scenario = d$scenario[1], quantity = d$quantity[1],
    fits = nrow(d), failures = sum(!ok), coverage = mean(d$covered[ok]), lower_95 = ci[1], upper_95 = ci[2],
    mean_width = mean(d$mean_width[ok]), mean_seconds = mean(d$seconds))
})
summary <- do.call(rbind, summaries)
write.csv(summary, "notes/tube-bands/refit-coverage.csv", row.names = FALSE)
print(summary, row.names = FALSE)
