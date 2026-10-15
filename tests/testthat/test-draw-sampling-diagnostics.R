diagnostic_plot_draws <- function() {
  ids <- data.frame(.draw = 101:116, .chain = rep(c(2L, 7L), each = 8L),
    .iteration = rep(11:18, 2L))
  x <- cbind(a = c(1:8, 9:16), b = rep(c(1, 1, 2, 2, 3, 3, 4, 4), 2L))
  sampling_finish(x, ids, coefficient_names = c("a", "b"))
}

test_that("the native trace plot preserves chain, iteration and coefficient values", {
  b <- diagnostic_plot_draws()
  diagnostics <- sampling_diagnostics(b)
  expect_s3_class(diagnostics, "sampling_diagnostics")
  expect_null(diagnostics$coefficients)
  p <- draw(diagnostics, select = c("b", "a"))
  expect_s3_class(p, "ggplot")
  expect_identical(levels(p$data$.chain), c("2", "7"))
  expect_identical(levels(p$data$.variable), c("b", "a"))
  expect_identical(p$data$.iteration, rep(11:18, 4L))
  expect_equal(p$data$.value, as.vector(b[, c("b", "a")]))
  expect_identical(p$labels$x, "Retained iteration")
  expect_silent(ggplot2::ggplot_build(p))
  expect_silent(ggplot2::ggplot_build(draw(diagnostics, scales = "free")))
  # Row order in the coefficient matrix must not change iteration order.
  shuffled <- b[16:1, , drop = FALSE]
  meta <- attr(b, "sampling")
  meta$draw_ids <- meta$draw_ids[16:1, ]
  attr(shuffled, "sampling") <- meta
  expect_equal(draw(sampling_diagnostics(shuffled))$data,
    draw(diagnostics)$data)
  # Native plot generation does not require posterior or bayesplot.
  local_mocked_bindings(sampling_require = function(...) stop("optional dependency"))
  expect_silent(draw(diagnostics))
  expect_silent(draw(diagnostics, type = "rank"))
  # Printing diagnostics must not print the stored draw matrix.
  text <- capture.output(print(diagnostics))
  expect_true(any(grepl("16 retained draws in 2 chain", text, fixed = TRUE)))
  expect_false(any(grepl("101", text, fixed = TRUE)))
})

test_that("rank histograms pool chains, count ties and reveal separation", {
  b <- diagnostic_plot_draws()
  withr::local_seed(12)
  seed <- .Random.seed
  p <- draw(sampling_diagnostics(b), type = "rank", n_bins = 4)
  expect_identical(.Random.seed, seed)
  expect_silent(ggplot2::ggplot_build(p))
  counts <- p$data
  # Last point completes the final histogram step; it is not another bin.
  a2 <- subset(counts, .variable == "a" & .chain == "2")
  a7 <- subset(counts, .variable == "a" & .chain == "7")
  expect_equal(a2$.rank, c(0, 4, 8, 12, 16))
  expect_equal(a2$.count[1:4], c(4, 4, 0, 0))
  expect_equal(a7$.count[1:4], c(0, 0, 4, 4))
  expect_equal(p$layers[[1L]]$data$yintercept, 2)
  # Each tied value occurs four times across the two chains. Average ranks
  # are 2.5, 6.5, 10.5 and 14.5, so every chain has two draws in every bin.
  for (chain in c("2", "7")) {
    tied <- subset(counts, .variable == "b" & .chain == chain)
    expect_equal(tied$.count[1:4], rep(2, 4))
  }
  expect_identical(levels(counts$.chain), c("2", "7"))
  expect_identical(levels(counts$.variable), c("a", "b"))
})

test_that("native diagnostic plots validate selection and chain layouts", {
  b <- diagnostic_plot_draws()
  d <- sampling_diagnostics(b)
  expect_equal(draw(d, select = "b")$data, draw(d, select = 2L)$data)
  expect_equal(draw(d, select = c(FALSE, TRUE))$data, draw(d, select = 2L)$data)
  expect_error(draw(d, select = "missing"), "Unknown coefficient.*missing")
  expect_error(draw(d, select = c(TRUE, NA)), "non-missing")
  expect_error(draw(d, select = TRUE), "one non-missing")
  for (select in list(integer(), c(1L, 1L))) {
    expect_error(draw(d, select = select), "at least one.*without duplicates")
  }
  for (select in list(0, -1, 3, 1.5, NA_real_, Inf)) {
    expect_error(draw(d, select = select), "positive positions")
  }
  for (n_bins in list(0, 1.5, NA_real_, Inf)) {
    expect_error(draw(d, type = "rank", n_bins = n_bins), "n_bins.*positive integer")
  }
  missing <- d
  missing$draws <- NULL
  expect_error(draw(missing), "rerun sampling with diagnostics = TRUE")
  incomplete <- d
  incomplete$draws <- b[-1, ]
  attr(incomplete$draws, "sampling") <- attr(b, "sampling")
  expect_error(draw(incomplete), "lost rows")
  nonfinite <- d
  nonfinite$draws[1, 1] <- Inf
  expect_error(draw(nonfinite), "must all be finite")
  # A one-coefficient, one-chain array must keep all three dimensions.
  one <- sampling_finish(matrix(1:8, ncol = 1), sampling_layout(8, 1),
    coefficient_names = "one", diagnostics = FALSE)
  attr(one, "sampling") <- list(draw_ids = sampling_layout(8, 1))
  expect_silent(ggplot2::ggplot_build(draw(sampling_diagnostics(one))))
  expect_error(draw(sampling_diagnostics(one), type = "rank"), "at least two chains")
  many <- sampling_finish(matrix(seq_len(80), ncol = 1), sampling_layout(80, 10))
  expect_silent(ggplot2::ggplot_build(draw(sampling_diagnostics(many))))
  scale <- ggplot2::scale_colour_manual(values = c("red", "blue"))
  p <- draw(d, discrete_colour = scale, scales = "fixed", ncol = 1)
  expect_equal(p$scales$get_scales("colour")$palette(2), c("red", "blue"))
})

test_that("derived samples retain original coefficient traces without changing draws", {
  skip_if_not_installed("posterior")
  withr::local_seed(934)
  dat <- data.frame(x = seq(0, 1, length.out = 40), y = rnorm(40))
  model <- mgcv::gam(y ~ s(x, k = 4), data = dat, method = "REML")
  plain <- post_draws(model, n_draws = 400, chains = 2, method = "mh",
    burnin = 100, seed = 8)
  b <- post_draws(model, n_draws = 400, chains = 2, method = "mh",
    burnin = 100, seed = 8, diagnostics = TRUE)
  expect_identical(as.vector(b), as.vector(plain))
  saved <- attr(b, "sampling")$coefficient_draws
  expect_null(attr(saved, "sampling"))
  expected <- draw(sampling_diagnostics(b))$data
  objects <- list(
    fitted_samples(model, method = "user", draws = b),
    posterior_samples(model, method = "user", draws = b, seed = 9),
    smooth_samples(model, method = "user", draws = b, n_grid = 3),
    derivative_samples(model, method = "user", draws = b, focal = "x", n_grid = 3),
    add_fitted_samples(dat, model, method = "user", draws = b),
    add_posterior_samples(dat, model, method = "user", draws = b, seed = 9),
    add_smooth_samples(dat, model, method = "user", draws = b))
  for (x in objects) {
    d <- sampling_diagnostics(x)
    expect_identical(d, sampling_diagnostics(b))
    expect_equal(draw(d)$data, expected)
    expect_silent(ggplot2::ggplot_build(draw(d, type = "rank")))
  }
  without <- fitted_samples(model, method = "user", draws = plain)
  expect_error(draw(sampling_diagnostics(without)), "rerun sampling")
  # Coefficient selection must plot actual returned draws, not the saved full matrix.
  selected <- post_draws(model, method = "user", draws = b, index = c(3, 1))
  p <- draw(sampling_diagnostics(selected))
  expect_equal(p$data$.value, as.vector(selected))
  expect_identical(levels(p$data$.variable), colnames(selected))
  # The internal covariance adjustment must refresh plotted draws as well as summaries.
  adjusted <- sampling_mh_covariance_adjust(model, b)
  result <- fitted_samples(model, method = "user", draws = adjusted)
  expect_equal(draw(sampling_diagnostics(result))$data$.value, as.vector(adjusted))
  original <- attr(adjusted, "sampling")$covariance_adjustment$source_diagnostics
  expect_identical(original, sampling_diagnostics(b))
})


test_that("unnamed matrices recover diagnostic names without changing source draws", {
  b <- diagnostic_plot_draws()
  meta <- attr(b, "sampling")
  meta$coefficient_draws <- b
  attr(meta$coefficient_draws, "sampling") <- NULL
  colnames(b) <- NULL
  attr(b, "sampling") <- meta
  original <- b
  d <- sampling_diagnostics(b)
  expect_identical(colnames(d$draws), c("a", "b"))
  expect_identical(b, original)
  expect_identical(as.vector(d$draws), as.vector(b))
  expect_silent(ggplot2::ggplot_build(draw(d, select = "a")))
  # Historical diagnostics may contain summaries but no retained matrix.
  meta$coefficient_draws <- NULL
  meta$coefficients <- data.frame(variable = c("a", "b"))
  attr(b, "sampling") <- meta
  expect_identical(colnames(sampling_diagnostics(b)$draws), c("a", "b"))
  # Never apply names from a larger coefficient matrix to an unnamed subset.
  subset <- b[, 1, drop = FALSE]
  attr(subset, "sampling") <- meta
  expect_null(colnames(sampling_diagnostics(subset)$draws))
})

test_that("trace plots retain iteration identifiers larger than R integers", {
  b <- diagnostic_plot_draws()
  meta <- attr(b, "sampling")
  meta$draw_ids$.iteration <- rep(2^31 + seq_len(8), 2L)
  attr(b, "sampling") <- meta
  p <- draw(sampling_diagnostics(b))
  expect_identical(p$data$.iteration, rep(meta$draw_ids$.iteration, 2L))
  expect_false(anyNA(p$data$.iteration))
  expect_silent(ggplot2::ggplot_build(p))
})
