#' Control label layout in gratia plots
#'
#' Configure labels on axes, legends and colour bars across [draw()] and
#' [assemble()] methods. Omitted settings retain the existing appearance.
#'
#' @param axes Named list with optional `wrap`, `n_dodge`, `angle` and
#'   `title_wrap` settings. `wrap` and `title_wrap` are positive integer widths
#'   in characters. Tick labels wrap at word boundaries, preserving explicit
#'   line breaks. Unbroken words are not split. `n_dodge` is the positive integer
#'   number of rows for horizontal-axis labels, or columns for vertical-axis
#'   labels. `angle` is a finite number of degrees between -90 and 90.
#'   Tick settings apply only to discrete axes; `title_wrap` applies to ordinary
#'   x and y axis titles, including continuous axes.
#' @param legend Named list with optional `wrap`, `title_wrap`, `nrow`, `ncol`
#'   and `byrow` settings. Wrapping widths and row/column counts are positive
#'   integers. `byrow` is logical. These settings apply to key-based legends,
#'   including colour, fill, shape and linetype legends.
#' @param colourbar Named list with optional `title_wrap`, `angle`, `width` and
#'   `height` settings. `title_wrap` is a positive integer width in characters;
#'   `angle` is between -90 and 90 degrees. Dimensions are positive, length-one
#'   [grid::unit()] objects and refer to the displayed bar's width and height.
#'   These settings apply to continuous colour and fill guides.
#'
#' @details
#' Every setting defaults to `NULL`, retaining the method's existing behaviour.
#' Pass the result as `label_control` to [draw()] or [assemble()]. The same
#' configuration can be reused across plot types; absent guides are ignored.
#' Mathematical expressions and missing labels are preserved. Wrapping changes
#' displayed text after scale labelling, without changing factor levels,
#' breaks, transformations or palettes. Numeric formatting remains controlled
#' by the scale's `labels` argument.
#'
#' Explicit axis settings override the existing `angle` and `n_dodge` plotting
#' arguments on discrete axes. Hidden guides stay hidden. Native guide types
#' and unrelated settings are retained; later ggplot2 scale and guide additions
#' follow their usual precedence. Settings apply to panels before patchwork
#' collects their guides, including plots returned with `wrap = FALSE`.
#' Projected coordinate labels in 3D plots retain their renderer's settings;
#' their ordinary legends and colour bars can use this control.
#'
#' @return A `draw_label_control` object.
#' @seealso [draw()], [assemble()], [gratia_theme_get()]
#' @export
#' @examples
#' # Wrap categorical tick labels and stagger them across two rows.
#' layout <- draw_label_control(axes = list(wrap = 12, n_dodge = 2))
#'
#' # Reuse one configuration for legend text and colour-bar layout.
#' layout <- draw_label_control(
#'   axes = list(wrap = 12),
#'   legend = list(wrap = 20, title_wrap = 20, ncol = 2),
#'   colourbar = list(title_wrap = 20, height = grid::unit(35, "mm"))
#' )
#'
#' # Draw a model with the requested layout.
#' load_mgcv()
#' dat <- data_sim("eg4", n = 100, seed = 42)
#' model <- gam(y ~ fac + s(x2, by = fac, k = 5), data = dat, method = "REML")
#' draw(model, label_control = layout)
draw_label_control <- function(axes = list(), legend = list(), colourbar = list()) {
  groups <- list(axes = axes, legend = legend, colourbar = colourbar)
  allowed <- list(axes = c("wrap", "n_dodge", "angle", "title_wrap"),
    legend = c("wrap", "title_wrap", "nrow", "ncol", "byrow"),
    colourbar = c("title_wrap", "angle", "width", "height"))
  for (group in names(groups)) {
    values <- groups[[group]]
    if (!is.list(values) || (length(values) &&
        (is.null(names(values)) || anyNA(names(values)) ||
          any(!nzchar(names(values))) || anyDuplicated(names(values))))) {
      cli::cli_abort("{.arg {group}} must be a list with unique, non-empty names.")
    }
    unknown <- setdiff(names(values), allowed[[group]])
    if (length(unknown)) {
      cli::cli_abort("Unknown {.arg {group}} setting{?s}: {.val {unknown}}.")
    }
    for (name in names(values)) {
      value <- values[[name]]
      if (is.null(value)) next
      argument <- paste0(group, "$", name)
      if (name %in% c("wrap", "title_wrap", "n_dodge", "nrow", "ncol")) {
        values[[name]] <- sampling_count(value, argument)
      } else if (name == "angle") {
        if (!is.numeric(value) || length(value) != 1L ||
            !is.finite(value) || abs(value) > 90) {
          cli::cli_abort("{.arg {argument}} must be a finite number between -90 and 90.")
        }
      } else if (name == "byrow") {
        if (!is.logical(value) || length(value) != 1L || is.na(value)) {
          cli::cli_abort("{.arg {argument}} must be TRUE or FALSE.")
        }
      } else if (!grid::is.unit(value) || length(value) != 1L ||
          !is.finite(as.numeric(value)) || as.numeric(value) <= 0) {
        cli::cli_abort("{.arg {argument}} must be a positive, length-one grid unit.")
      }
    }
    groups[[group]] <- values[!vapply(values, is.null, logical(1))]
  }
  structure(groups, class = "draw_label_control")
}

validate_draw_label_control <- function(control) {
  if (is.null(control)) return(NULL)
  if (!is.list(control) || is.null(names(control)) || anyNA(names(control)) ||
      anyDuplicated(names(control)) ||
      any(!names(control) %in% c("axes", "legend", "colourbar"))) {
    cli::cli_abort("{.arg label_control} must be created by {.fun draw_label_control} or be a named list of its settings.")
  }
  do.call(draw_label_control, unclass(control))
}

# Format labels after scales have applied their formatter or label mapping.
# Lists can mix ordinary text with plotmath expressions; retain the latter.
wrap_draw_labels <- function(labels, width) {
  if (is.null(width) || is.null(labels) || is.expression(labels)) return(labels)
  if (is.list(labels)) return(lapply(labels, wrap_draw_labels, width = width))
  if (!is.character(labels)) return(labels)
  vapply(labels, function(label) {
    if (is.na(label)) return(NA_character_)
    lines <- stringr::str_split(label, stringr::fixed("\n"))[[1L]]
    paste(vapply(lines, function(line) {
      stringr::str_wrap(line, width = width)
    }, character(1)), collapse = "\n")
  }, character(1), USE.NAMES = FALSE) |>
    stats::setNames(names(labels))
}

# Extend the selected native guide, retaining its drawing and merging methods.
# extract_params() runs after scale labels are resolved and before guide hashes
# are calculated, so colour and fill guides can still merge normally.
draw_label_guide <- function(guide, control, position = FALSE) {
  if (is.null(guide) || inherits(guide, "GuideNone") || identical(guide, "none") ||
      identical(guide, FALSE)) return(guide)
  if (inherits(guide, "waiver")) {
    if (!position) return(guide)
    guide <- ggplot2::guide_axis()
  }
  if (is.character(guide)) {
    constructor <- paste0("guide_", guide)
    if (length(guide) != 1L || !constructor %in% getNamespaceExports("ggplot2")) {
      return(guide)
    }
    guide <- getExportedValue("ggplot2", constructor)()
  }
  group <- if (inherits(guide, "GuideAxis")) "axes" else {
    if (inherits(guide, "GuideColourbar")) "colourbar" else {
      if (inherits(guide, "GuideLegend")) "legend" else NULL
    }
  }
  if (is.null(group)) return(guide)
  settings <- control[[group]]
  if (!length(settings)) return(guide)
  parent <- guide
  ggplot2::ggproto(NULL, parent,
    extract_params = function(self, scale, params, ...) {
      params <- ggplot2::ggproto_parent(parent, self)$extract_params(scale, params, ...)
      if (group == "axes") {
        if (scale$is_discrete()) {
          params$key$.label <- wrap_draw_labels(params$key$.label, settings$wrap)
          if (!is.null(settings$n_dodge)) params$n.dodge <- settings$n_dodge
          if (!is.null(settings$angle)) params$angle <- settings$angle
        }
      } else {
        params$title <- wrap_draw_labels(params$title, settings$title_wrap)
        if (group == "legend") {
          params$key$.label <- wrap_draw_labels(params$key$.label, settings$wrap)
          if (!is.null(settings$nrow)) params$nrow <- settings$nrow
          if (!is.null(settings$ncol)) params$ncol <- settings$ncol
          if (!is.null(settings$byrow)) {
            theme <- if (is.null(params$theme)) ggplot2::theme() else params$theme
            params$theme <- theme + ggplot2::theme(legend.byrow = settings$byrow)
          }
        } else {
          if (!is.null(settings$angle)) params$angle <- settings$angle
          dimensions <- settings[intersect(c("width", "height"), names(settings))]
          if (length(dimensions)) {
            names(dimensions) <- paste0("legend.key.", names(dimensions))
            theme <- if (is.null(params$theme)) ggplot2::theme() else params$theme
            params$theme <- theme + do.call(ggplot2::theme, dimensions)
          }
        }
      }
      params
    })
}

draw_label_scale <- function(scale, control) {
  scale <- scale$clone()
  position <- any(c("x", "y") %in% scale$aesthetics)
  scale$guide <- draw_label_guide(scale$guide, control, position = position)
  scale
}

# Attach controls to existing scales and to defaults created later by ggplot2.
# Later explicit scale or guide additions replace these in the usual way.
draw_label_plot <- function(plot, control) {
  if (inherits(plot, "patchwork")) {
    plot$patches$plots <- lapply(plot$patches$plots, draw_label_plot, control = control)
  }
  if (!ggplot2::is_ggplot(plot)) {
    if (is.list(plot)) return(lapply(plot, draw_label_plot, control = control))
    return(plot)
  }
  scales <- plot$scales$clone()
  scales$scales <- lapply(scales$scales, draw_label_scale, control = control)
  plot$scales <- ggplot2::ggproto(NULL, scales,
    add_defaults = function(self, data, env) {
      n <- length(self$scales)
      ggplot2::ggproto_parent(scales, self)$add_defaults(data, env)
      if (length(self$scales) > n) {
        for (i in seq.int(n + 1L, length(self$scales))) {
          self$scales[[i]] <- draw_label_scale(self$scales[[i]], control)
        }
      }
    })
  guides <- plot$guides
  plot$guides <- ggplot2::ggproto(NULL, guides,
    guides = lapply(guides$guides, draw_label_guide, control = control))
  width <- control$axes$title_wrap
  coord <- plot$coordinates
  if (!is.null(width) && !inherits(coord, "Coord3D")) {
    plot$coordinates <- ggplot2::ggproto(NULL, coord,
      labels = function(self, labels, panel_params) {
        labels <- ggplot2::ggproto_parent(coord, self)$labels(labels, panel_params)
        labels$x <- wrap_draw_labels(labels$x, width)
        labels$y <- wrap_draw_labels(labels$y, width)
        labels
      })
  }
  plot
}

apply_draw_label_control <- function(value, control) {
  if (is.null(control) || !any(lengths(control))) return(value)
  draw_label_plot(value, control)
}
