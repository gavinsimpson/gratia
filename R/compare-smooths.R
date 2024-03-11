#' Compare smooths across models
#'
#' @param model Primary model for comparison.
#' @param ... Additional models to compare smooths against those of `model`.
#' @param smooths `r lifecycle::badge("deprecated")` Use `select` instead.
#' @param select character; select which smooths to compare. The default
#'   (`NULL`) means all smooths in `model` will be compared. Numeric `select`
#'   indexes the smooths in the order they are specified in the formula and
#'   stored in `model`. Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#'
#' @inheritParams smooth_estimates
#'
#' @export
#'
#' @importFrom rlang dots_list
#' @importFrom dplyr group_by
#' @importFrom lifecycle deprecated is_present
#'
#' @examples
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 5)
#' }
#' load_mgcv()
#' dat <- data_sim("eg1", seed = 2)
#'
#' ## models to compare smooths across - artificially create differences
#' m1 <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
#'   data = dat, method = "REML"
#' )
#' m2 <- gam(y ~ s(x0, bs = "ts") + s(x1, bs = "ts") + s(x2, bs = "ts") +
#'   s(x3, bs = "ts"), data = dat, method = "REML")
#'
#' ## build comparisons
#' comp <- compare_smooths(m1, m2)
#' comp
#' ## notice that the result is a nested tibble
#'
#' draw(comp)
#' \dontshow{
#' options(op)
#' }
`compare_smooths` <- function(model, ...,
    select = NULL,
    smooths = deprecated(),
    n = 100,
    data = NULL,
    unconditional = FALSE,
    overall_uncertainty = TRUE,
    partial_match = FALSE) {
  if (lifecycle::is_present(smooths)) {
    lifecycle::deprecate_warn("0.8.9.9", "compare_smooths(smooths)",
      "compare_smooths(select)")
    select <- smooths
  }
  ## grab ...
  dots <- rlang::dots_list(..., .named = TRUE)
  model_names <- c(deparse(substitute(model)), names(dots))
  if (length(dots) < 1L) {
    stop("Need at least two models to compare smooths",
      call. = FALSE
    )
  }
  ## combine model and others into a list
  models <- append(list(model), dots)

  if (is.null(smooths)) {
    smooths <- Reduce(union, lapply(models, smooths))
  } else {
    # user supplied smooth vector, check that those smooths exist in models
  }

  ## loop over the smooths, applying smooth_estimates to each model
  sm_est <- lapply(models, smooth_estimates,
    select = select,
    n = n,
    data = data,
    uncondtional = unconditional,
    overall_uncertainty = overall_uncertainty,
    unnest = FALSE, partial_match = partial_match
  )

  ## loop over list of smooth estimates and add model column
  for (i in seq_along(sm_est)) {
    sm_est[[i]] <- add_column(sm_est[[i]],
      .model = model_names[i],
      .before = 1L
    )
  }

  `unnest_nest` <- function(x) {
    x |>
      group_by(.data$.smooth) |>
      group_split() |>
      purrr::map(unnest, cols = all_of("data")) |>
      purrr::map(nest, data = !all_of(c(
        ".model", ".smooth",
        ".type", ".by"
      ))) |>
      bind_rows()
  }
  sm_est <- purrr::map(sm_est, unnest_nest)

  sm_est <- bind_rows(sm_est) |>
    arrange(.data$.smooth)
  class(sm_est) <- c("compare_smooths", class(sm_est))
  sm_est
}

#' Plot comparisons of smooths
#'
#' @param object of class `"compare_smooths"`, the result of a call to
#'   [gratia::compare_smooths()].
#' @inheritParams draw.gam
#'
#' @export
#' @importFrom dplyr group_split
#' @importFrom purrr map
#' @importFrom patchwork wrap_plots
`draw.compare_smooths` <- function(object,
                                   ncol = NULL, nrow = NULL,
                                   guides = "collect",
                                   ...) {
  l <- group_split(object, .data$.smooth)

  plts <- map(l, plot_comparison_of_smooths)

  ## return
  n_plots <- length(plts)
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots / ncol)
  }
  wrap_plots(plts,
    byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
    ...
  )
}

#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot geom_ribbon geom_line
`plot_comparison_of_smooths` <- function(object, coverage = 0.95, ...) {
  ## get the covariate labels
  sm_vars <- vars_from_label(unique(object[[".smooth"]]))
  ## unnest data cols
  object <- unnest(object, cols = all_of("data"))

  ## compute the critical value
  crit <- coverage_normal(coverage)

  ## add the frequentist confidence interval
  object <- object |> add_confint(coverage = coverage)

  ## basic plot
  plt <- ggplot(object, aes(
    x = .data[[sm_vars[1L]]],
    y = .data[[".estimate"]],
    group = .data[[".model"]]
  ))

  ## add uncertainty bands
  plt <- plt + geom_ribbon(
    aes(
      ymin = .data[[".lower_ci"]],
      ymax = .data[[".upper_ci"]],
      fill = .data[[".model"]]
    ),
    alpha = 0.2
  )

  ## add smooth lines
  plt <- plt + geom_line(aes(colour = .data[[".model"]]))

  ## Add labels
  plt <- plt + labs(
    colour = "Model", fill = "Model",
    title = unique(object[[".smooth"]]),
    y = "Estimate"
  )

  plt
}
