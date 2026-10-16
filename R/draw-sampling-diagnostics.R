#' Plot coefficient-chain diagnostics
#'
#' Native ggplot2 displays of retained coefficient chains. Trace plots show
#' movement over iterations, including repeated values after MH rejections.
#' Rank plots compare overlaid histogram outlines for the ranks of each
#' coefficient pooled across chains. Chains exploring the same distribution
#' should have approximately uniform rank histograms. These plots complement
#' the numerical summaries; similar-looking chains alone do not establish
#' convergence.
#'
#' @param object A [sampling_diagnostics()] object.
#' @param type Either `"trace"` or `"rank"`.
#' @param select Coefficients to plot, as exact names, positive integer
#'   positions, or a logical vector with one entry per coefficient. The default
#'   plots every available coefficient.
#' @param n_bins Positive integer; number of common pooled-rank bins. Used only
#'   for rank plots. Tied draws receive their average rank, without jitter.
#' @param scales Facet scales, either `"fixed"` or `"free"`. By default trace
#'   plots have separate coefficient scales and rank plots share a count scale.
#' @param ncol,nrow Number of columns or rows in the panel layout.
#' @param alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Transparency of the chain lines.
#' @param linewidth Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Width of the chain lines.
#' @param discrete_colour An optional ggplot2 discrete colour scale. Defaults
#'   follow gratia's smooth plots: Okabe-Ito for up to nine chains, then hue.
#' @param ... Additional arguments passed to [ggplot2::facet_wrap()].
#'
#' @details
#' The trace x axis counts retained iterations within each chain, rather than
#' the globally unique draw number. Burn-in is discarded by the samplers and
#' cannot be displayed. With thinning, successive plotted iterations represent
#' successive retained draws, not successive MH transitions.
#'
#' Rank plots require at least two chains. Ranks are computed separately for
#' each selected coefficient, pooling all chains. A dashed line shows the
#' average count per bin. Independent continuous draws from the same
#' distribution should give approximately flat histograms. Autocorrelation
#' and ties affect the histogram variation; the
#' reference line is not a confidence bound.
#'
#' Plots always describe coefficient draws, including when diagnostics are
#' extracted from response samples or smooths. For transformed quantities use
#' [as_mcmc_draws()] with external diagnostic tools. Sampling with
#' `diagnostics = TRUE` retains coefficient draws for plotting from those
#' outputs. Coefficient matrices can be plotted whenever their sampling
#' metadata are available, even without numerical diagnostic summaries.
#'
#' @return A [ggplot2::ggplot()] object.
#' @seealso [sampling_diagnostics()], [as_mcmc_draws()]
#' @export
#' @examples
#' load_mgcv()
#' dat <- data_sim("eg1", n = 100, seed = 42)
#' model <- gam(y ~ s(x0, k = 5), data = dat, method = "REML")
#' b <- post_draws(model, n_draws = 400, chains = 4, method = "mh",
#'   burnin = 100, seed = 281)
#' diagnostics <- sampling_diagnostics(b)
#' draw(diagnostics, select = 1:2)
#' draw(diagnostics, type = "rank", select = 1:2, n_bins = 10)
draw.sampling_diagnostics <- function(object, type = c("trace", "rank"),
    select = NULL, n_bins = 20L, scales = NULL, ncol = NULL, nrow = NULL,
    alpha = 0.7, linewidth = 0.4, discrete_colour = NULL, ...) {
  gratia_plot_arguments(c("alpha", "linewidth"), environment())
  type <- match.arg(type)
  if (is.null(object$draws)) stop(
    "Coefficient draws are unavailable; rerun sampling with diagnostics = TRUE, ",
    "or extract diagnostics from a coefficient matrix with chain metadata.",
    call. = FALSE)
  draws <- object$draws
  variables <- colnames(draws) %||% paste0("b", seq_len(NCOL(draws)))
  selected <- sampling_plot_select(select, variables)
  # The converter checks the layout, including filtered or duplicated rows.
  values <- as_mcmc_draws(draws[, selected, drop = FALSE] |>
    structure(sampling = attr(draws, "sampling")), format = "array")
  if (any(!is.finite(values))) stop(
    "Selected coefficient draws must all be finite.", call. = FALSE)
  dimensions <- dim(values)
  if (type == "rank" && dimensions[2L] < 2L) stop(
    "Rank plots require at least two chains; use type = 'trace' for one chain.",
    call. = FALSE)
  if (is.null(scales)) scales <- if (type == "trace") "free" else "fixed"
  scales <- match.arg(scales, c("free", "fixed"))
  iterations <- utils::type.convert(dimnames(values)[[1L]], as.is = TRUE)
  data <- expand.grid(.iteration = iterations,
    .chain = dimnames(values)[[2L]], .variable = dimnames(values)[[3L]],
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  data$.chain <- factor(data$.chain, levels = dimnames(values)[[2L]])
  data$.variable <- factor(data$.variable, levels = dimnames(values)[[3L]])
  data$.value <- as.vector(values)
  if (type == "trace") {
    plt <- gratia_plot(data, ggplot2::aes(x = .data$.iteration,
      y = .data$.value, colour = .data$.chain, group = .data$.chain)) +
      geom_gratia_chain_line(alpha = alpha, linewidth = linewidth) +
      ggplot2::labs(x = "Retained iteration", y = "Coefficient")
  } else {
    n_bins <- sampling_count(n_bins, "n_bins")
    total <- dimensions[1L] * dimensions[2L]
    breaks <- seq(0, total, length.out = n_bins + 1L)
    groups <- split(seq_len(nrow(data)), data$.variable)
    counts <- lapply(groups, function(rows) {
      ranks <- rank(data$.value[rows], ties.method = "average")
      bins <- cut(ranks, breaks = breaks, labels = FALSE,
        include.lowest = TRUE, right = TRUE)
      chains <- split(bins, data$.chain[rows])
      do.call(rbind, lapply(seq_along(chains), function(j) {
        count <- tabulate(chains[[j]], nbins = n_bins)
        data.frame(.rank = breaks, .count = c(count, count[length(count)]),
          .chain = names(chains)[j], .variable = as.character(data$.variable[rows[1L]]))
      }))
    })
    counts <- do.call(rbind, counts)
    counts$.variable <- factor(counts$.variable, levels = levels(data$.variable))
    counts$.chain <- factor(counts$.chain, levels = levels(data$.chain))
    plt <- gratia_plot(counts, ggplot2::aes(x = .data$.rank,
      y = .data$.count, colour = .data$.chain, group = .data$.chain)) +
      geom_gratia_chain_reference_hline(yintercept = dimensions[1L] / n_bins,
        linetype = "dashed", colour = "grey50") +
      geom_gratia_chain_step(alpha = alpha, linewidth = linewidth, direction = "hv") +
      ggplot2::labs(x = "Pooled rank", y = "Draws per bin")
  }
  palette <- prepare_smooth_scales(dimensions[2L], discrete_colour = discrete_colour)
  plt + palette$colour + ggplot2::labs(colour = "Chain") +
    ggplot2::facet_wrap(ggplot2::vars(.data$.variable),
      scales = if (scales == "free") "free_y" else "fixed",
      ncol = ncol, nrow = nrow, ...)
}

sampling_plot_select <- function(select, variables) {
  n <- length(variables)
  if (is.null(select)) select <- seq_len(n)
  if (is.character(select)) {
    selected <- match(select, variables)
  } else if (is.logical(select)) {
    if (length(select) != n || anyNA(select)) stop(
      "Logical 'select' must have one non-missing entry per coefficient.",
      call. = FALSE)
    selected <- which(select)
  } else if (is.numeric(select) && all(is.finite(select)) &&
      all(select == floor(select) & select >= 1 & select <= n)) {
    selected <- as.integer(select)
  } else stop(
    "'select' must contain coefficient names, positive positions, or logical entries.",
    call. = FALSE)
  if (anyNA(selected)) stop("Unknown coefficient in 'select': ",
    paste(select[is.na(selected)], collapse = ", "), call. = FALSE)
  if (!length(selected) || anyDuplicated(selected)) stop(
    "'select' must choose at least one coefficient without duplicates.", call. = FALSE)
  selected
}
