# assemble() returns a list of prepared ggplot2 objects

#' Prepare plots via `ggplot2` and assembles them as a list
#'
#' Generic function for assembling plot objects created from R objects, using
#' the `ggplot2` package.
#'
#' @param object and R object to plot.
#' @param ... arguments passed to other methods.
#'
#' @return A list of [ggplot2::ggplot()] objects.
#'
#' @author Gavin L. Simpson
#'
#' @export
`assemble` <- function(object, ...) {
  UseMethod("assemble")
}

#' Plot estimated smooths from a fitted GAM
#'
#' Plots estimated smooths from a fitted GAM model in a similar way to
#' `mgcv::plot.gam()` but instead of using base graphics, [ggplot2::ggplot()]
#' is used instead.
#'
#' @inheritParams draw.gam
#' @param ... Arguments to other methods; not used.
#'
#' @note Internally, plots of each smooth are created using [ggplot2::ggplot()]
#'   and composed into a single plot using [patchwork::wrap_plots()]. As a
#'   result, it is not possible to use `+` to add to the plots in the way one
#'   might typically work with `ggplot()` plots. Instead, use the `&` operator;
#'   see the examples.
#'
#' @return A list of [ggplot2::ggplot()] objects.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom ggplot2 scale_colour_discrete scale_colour_continuous
#'   scale_fill_distiller
#' @importFrom dplyr mutate rowwise ungroup left_join group_split summarise
#' @importFrom purrr pluck map_lgl in_parallel map map_chr
#' @importFrom rlang expr_label
#' @importFrom utils packageVersion getFromNamespace
#' @importFrom stringr str_split_fixed
#' @importFrom cli cli_alert_info
#' @export
#'
#' @examples
#' load_mgcv()
#'
#' # simulate some data
#' df1 <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' # fit GAM
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df1, method = "REML")
#'
#' # assemble plots of all smooths
#' assemble(m1)
#'
#' # can be passed to patchwork::wrap_plots()
#' suppressPackageStartupMessages(library("patchwork"))
#' assemble(m1) |> wrap_plots(ncol = 2, nrow = 2)
`assemble.gam` <- function(
  object,
  data = NULL,
  select = NULL,
  parametric = FALSE,
  terms = NULL,
  residuals = FALSE,
  scales = c("free", "fixed"),
  ci_level = 0.95,
  n = 100,
  n_3d = 16,
  n_4d = 4,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  constant = NULL,
  fun = NULL,
  dist = 0.1,
  rug = TRUE,
  distinct_rug = TRUE,
  contour = TRUE,
  grouped_by = FALSE,
  ci_alpha = 0.2,
  ci_col = "black",
  smooth_col = "black",
  resid_col = "steelblue3",
  contour_col = "black",
  n_contour = NULL,
  partial_match = FALSE,
  discrete_colour = NULL,
  discrete_fill = NULL,
  continuous_colour = NULL,
  continuous_fill = NULL,
  position = "identity",
  angle = NULL,
  ncol = NULL, nrow = NULL,
  guides = "keep", widths = NULL, heights = NULL,
  crs = NULL,
  default_crs = NULL,
  lims_method = "cross",
  wrap = TRUE,
  caption = TRUE,
  envir = environment(formula(object)),
  ...
) {
  .call <- match.call()

  # fixed or free scale?
  scales <- match.arg(scales)

  # adding a caption?
  caption <- as.logical(caption)

  # fix up default scales
  # if (is.null(discrete_colour)) {
  #    discrete_colour <- scale_colour_discrete()
  # }
  if (is.null(continuous_colour)) {
    continuous_colour <- scale_colour_continuous()
  }
  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }

  # if not using select, set parametric TRUE if not set to FALSE
  if (!is.null(select)) {
    if (is.null(parametric)) {
      parametric <- FALSE
    }
  } else {
    if (is.null(parametric)) {
      parametric <- TRUE
    }
  }

  # sort out n_3d and n_4d. If these are `NULL` then do sensible thing at set
  # them small. Default is 3 for n_4d and for 12 for n_3d, but if we have a kD
  # smooth (k >= 4) we want to

  S <- smooths(object) # vector of smooth labels - "s(x)"

  # select smooths
  select <-
    check_user_select_smooths(
      smooths = S, select = select,
      partial_match = partial_match,
      model_name = expr_label(substitute(object))
    )

  # this is needed for the parametric terms below
  sm_plts <- NULL
  ylims <- NULL

  # do we have any smooths to plot?
  if (length(select) > 0L) {
    # evaluate all requested smooths
    sm_eval <- smooth_estimates(object,
      select = S[select],
      n = n,
      n_3d = n_3d,
      n_4d = n_4d,
      data = data,
      unconditional = unconditional,
      overall_uncertainty = overall_uncertainty,
      dist = dist,
      clip = TRUE,
      unnest = FALSE
    )

    # grab tensor term order if present, if not it is NULL & that's OK
    tensor_term_order <- attr(sm_eval, "tensor_term_order")

    # add confidence interval
    sm_eval <- sm_eval |>
      rowwise() |>
      mutate(data = list(add_confint(.data$data, coverage = ci_level))) |>
      ungroup()

    # Take the range of the smooths & their confidence intervals now
    # before we put rug and residuals on
    if (utils::packageVersion("dplyr") > "1.0.10") {
      sm_rng <- sm_eval |>
        rowwise() |>
        utils::getFromNamespace("reframe", "dplyr")(rng =
          range(c(data$.estimate, data$.lower_ci, data$.upper_ci))) |>
        pluck("rng")
    } else {
      sm_rng <- sm_eval |>
        rowwise() |>
        summarise(rng = range(c( # FIXME: summarise() -> reframe()
          data$.estimate, data$.lower_ci,
          data$.upper_ci
        ))) |>
        pluck("rng")
    }

    # Add partial residuals if requested - by default they are
    # At the end of this, sm_eval will have a new list column containing the
    # partial residuals, `partial_residual`
    p_resids_rng <- NULL
    if (isTRUE(residuals)) {
      if (is.null(residuals(object)) || is.null(weights(object))) {
        residuals <- FALSE
      } else {
        # get residuals in a suitable format
        p_resids <- nested_partial_residuals(object, terms = S[select])

        # compute the range of residuals for each smooth
        # p_resids_rng <- p_resids |>
        #     rowwise() |>
        #     dplyr::reframe(rng =
        #         range(.data$partial_residual$partial_residual)) |>
        #     pluck("rng")
        if (utils::packageVersion("dplyr") > "1.0.10") {
          p_resids_rng <- p_resids |>
            rowwise() |>
            utils::getFromNamespace("reframe", "dplyr")(
              rng = range(.data$partial_residual$partial_residual)) |>
            pluck("rng")
        } else {
          p_resids_rng <- p_resids |>
            rowwise() |>
            summarise(
              rng =
                range(.data$partial_residual$partial_residual)
            ) |>
            pluck("rng")
        }
        # merge with the evaluated smooth
        sm_eval <- suppress_matches_multiple_warning(
          left_join(sm_eval, p_resids, by = ".smooth")
        )
      }
    }

    # add rug data?
    if (isTRUE(rug)) {
      # get rug data in a suitable format
      rug_data <- nested_rug_values(
        object,
        terms = S[select],
        distinct = distinct_rug
      )

      # merge with the evaluated smooth
      sm_eval <- suppress_matches_multiple_warning(
        left_join(sm_eval, rug_data, by = ".smooth")
      )
    }

    # need to figure out scales if "fixed"
    if (isTRUE(identical(scales, "fixed"))) {
      ylims <- range(sm_rng, p_resids_rng)
    }

    # draw smooths
    sm_l <- if (isTRUE(grouped_by)) {
      sm_levels <- unique(sm_eval$.smooth)
      levs <- unique(str_split_fixed(sm_eval$.smooth, ":", n = 2)[, 1])
      sm_l <- sm_eval |>
        mutate(
          ..smooth.. = factor(.data$.smooth, levels = S[select]),
          .term = str_split_fixed(.data$.smooth, ":", n = 2)[, 1],
          ..by.. = if_else(is.na(.data$.by), "..no_level..", .data$.by)
        ) |>
        relocate(".term", .before = 1L)
      grp_by_levs <- unique(sm_l$"..by..")
      sm_l <- sm_l |>
        group_split(factor(.data$.term, levels = sm_levels),
          factor(.data$"..by..", levels = grp_by_levs))
      # sometimes the steps to get the order right above don't work
      sm_l_levs <- vapply(sm_l, \(x) unique(x$.term), character(1L))
      if (!identical(unique(sm_l_levs), levs)) {
        names(sm_l) <- sm_l_levs
        sm_l <- sm_l[levs]
        names(sm_l) <- NULL
      }
      sm_l
    } else {
      # the factor is to reorder to way the smooths entered the model
      sm_eval <- add_column(
        sm_eval,
        .term = str_split_fixed(sm_eval$.smooth, ":", n = 2)[, 1]
      )
      group_split(sm_eval, factor(.data$.smooth, levels = S[select]))
    }
    sm_plts <- map(sm_l,
      draw_smooth_estimates,
      constant = constant,
      fun = fun,
      contour = contour,
      contour_col = contour_col,
      n_contour = n_contour,
      ci_alpha = ci_alpha,
      ci_col = ci_col,
      smooth_col = smooth_col,
      resid_col = resid_col,
      partial_match = partial_match,
      discrete_colour = discrete_colour,
      discrete_fill = discrete_fill,
      continuous_colour = continuous_colour,
      continuous_fill = continuous_fill,
      angle = angle,
      ylim = ylims,
      crs = crs,
      default_crs = default_crs,
      lims_method = lims_method,
      tensor_term_order = tensor_term_order,
      caption = caption,
      grouped_by = grouped_by,
      ... # FIXME: temporary fix to allow captions to be suppressed-ish
    )

    # Parallel version; currently not working for some weird ggproto thing
    # that needs some debugging
    # sm_plts2 <- map(
    #   sm_l,
    #   in_parallel(
    #     \(sm, ...) draw_smooth_estimates(
    #       sm,
    #       constant = constant, fun = fun, contour = contour,
    #       contour_col = contour_col, n_contour = n_contour,
    #       ci_alpha = ci_alpha, ci_col = ci_col, smooth_col = smooth_col,
    #       resid_col = resid_col, partial_match = partial_match,
    #       discrete_colour = discrete_colour,
    #       discrete_fill = discrete_fill,
    #       continuous_colour = continuous_colour,
    #       continuous_fill = continuous_fill,
    #       angle = angle, ylim = ylims, crs = crs, default_crs = default_crs,
    #       lims_method = lims_method, tensor_term_order = tensor_term_order,
    #       caption = caption, ...
    #     ),
    #     constant = constant, fun = fun, contour = contour,
    #     contour_col = contour_col, n_contour = n_contour,
    #     ci_alpha = ci_alpha, ci_col = ci_col, smooth_col = smooth_col,
    #     resid_col = resid_col, partial_match = partial_match,
    #     discrete_colour = discrete_colour, discrete_fill = discrete_fill,
    #     continuous_colour = continuous_colour,
    #     continuous_fill = continuous_fill,
    #     angle = angle, ylims = ylims, crs = crs, default_crs = default_crs,
    #     lims_method = lims_method, tensor_term_order = tensor_term_order,
    #     caption = caption,
    #     draw_smooth_estimates = draw_smooth_estimates, ...
    #   )
    # )

    # filter out NULLs as those are types of smooths we can't plot (yet)
    no_plot <- map_lgl(sm_plts, is.null)
    sm_plts <- sm_plts[!no_plot]
    sm_l <- sm_l[!no_plot]
    sm_plt_nms <- map_chr(sm_l, .f = \(x) unique(x$.term))
    #sm_eval <- sm_eval |> add_column(
    #  .term = sm_plt_nms, .before = 1L
    #)
    ## sm_eval <- sm_eval[!no_plot, ]
    # set names on sm_plts
    if (length(sm_plts) > 0L) {
      sm_plts <- setNames(sm_plts, sm_plt_nms)
      #sm_plts <- if (grouped_by) {
      #  setNames(sm_plts, sm_plt_nms)
      #} else {
      #  setNames(sm_plts, sm_plt_nms)
      #}
    }
  } # end stuff for smooths...

  # Are we plotting parametric effects too?
  if (isTRUE(parametric)) {
    if (length(parametric_terms(object)) == 0L) {
      message("The model contains no parametric terms")
      parametric <- FALSE
    } else {
      para <- parametric_effects(object,
        select = terms, data = data,
        unconditional = unconditional,
        unnest = TRUE, ci_level = ci_level, envir = envir
      )

      if (is.null(para)) {
        parametric <- FALSE
      } else {
        # Add CI
        # crit <- coverage_normal(ci_level)
        # object <- mutate(para,
        #    .lower_ci = .data$.partial - (crit * .data$.se),
        #    .upper_ci = .data$.partial + (crit * .data$.se))
        object <- para |> add_confint(coverage = ci_level)
        # need to alter the ylim if scales are fixed
        if (isTRUE(identical(scales, "fixed"))) {
          ylims <- range(
            ylims, object$.partial, object$.upper_ci,
            object$.lower_ci
          )
        }

        f_levels <- attr(para, "factor_levels")

        para_plts <- para |>
          group_by(.data$.term) |>
          group_map(
            .keep = TRUE,
            .f = ~ draw_parametric_effect(.x,
              ci_level = ci_level,
              ci_col = ci_col,
              ci_alpha = ci_alpha,
              line_col = smooth_col,
              constant = constant,
              fun = fun,
              rug = rug,
              position = position,
              angle = angle,
              ylim = ylims,
              factor_levels = f_levels
            )
          )
      }
    }
  }

  # filter out NULLs as those are types of smooths we can't plot (yet)
  #no_plot <- map_lgl(sm_plts, is.null)
  #sm_plts <- sm_plts[!no_plot]
  #sm_eval <- sm_eval[!no_plot,]
  #sm_l <- sm_l |> bind_rows()
  #sm_l <- sm_l[!no_plot] |> bind_rows()

  ## set names on sm_plts
  #if (length(sm_plts) > 0L) {
  #  sm_plts <- if (grouped_by) {
  #    setNames(sm_plts, map_chr(sm_l, .f = \(x) unique(x$.term)))
  #  } else {
  #    # setNames(sm_plts, sm_eval$.smooth)
  #    setNames(sm_plts, sm_l$.smooth)
  #  }
  #}

  if (isTRUE(parametric)) {
    para_nm <- unique(para$.term)
    para_plts <- setNames(para_plts, para_nm)
    sm_plts <- append(sm_plts, para_plts)
  }

  # recheck: filter out NULLs as those are types of terms we can't plot (yet)
  no_plot <- map_lgl(sm_plts, is.null)
  sm_plts <- sm_plts[!no_plot]

  if (all(no_plot)) {
    cli_alert_info("Unable to draw any of the model terms.")
    return(invisible())
  }

  # add a class
  class(sm_plts) <- append(
    x = "assembled_plots", values = class(sm_plts), after = 0L
  )

  # add .call as attribute
  attr(sm_plts, ".call") <- .call

  # return
  sm_plts
}

#' @importFrom cli cli_h2 cli_rule cli_par cli_end cli_text cli_ol
#' @export
`print.assembled_plots` <- function(x, ...) {
  cli_h2("Assembly of {length(x)} plots")
  cli_par()
  cli_end()
  cli_ol(names(x))
  cli_par()
  cli_end()
}
