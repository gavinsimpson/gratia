#' Native themes for gratia plots
#'
#' Gratia plots use ordinary ggplot2 themes. Component settings are read when
#' the plot is built, so they can also be changed after calling [draw()].
#'
#' @param theme A ggplot2 theme, or `NULL` to clear the package override.
#' @param ... Settings passed to [ggplot2::theme()].
#'
#' @details
#' `gratia_theme_get()` returns the override stored in
#' `options("gratia.plot.theme")`. It does not include ggplot2's active theme.
#' The override is attached to each plot when it is constructed. Later changes
#' to this option affect new plots. Use [withr::with_options()] for a temporary
#' override. Unspecified settings inherit ggplot2's active theme as usual.
#'
#' Component elements accept [ggplot2::element_geom()]. The main elements are
#' `geom.gratia.smooth`, `geom.gratia.effect`, `geom.gratia.derivative`,
#' `geom.gratia.sample`, `geom.gratia.interval`, `geom.gratia.partial.residual`,
#' `geom.gratia.increase`, `geom.gratia.decrease`, `geom.gratia.change`,
#' `geom.gratia.diagnostic.point`, `geom.gratia.reference`, `geom.gratia.rug`,
#' `geom.gratia.contour`, `geom.gratia.boundary`, `geom.gratia.observed`,
#' `geom.gratia.replicated`, `geom.gratia.chain`, `geom.gratia.histogram`,
#' `geom.gratia.rootogram.bar`, `geom.gratia.rootogram.fitted`, and
#' `geom.gratia.concurvity.bar`. Other elements are `geom.gratia.curve`
#' (the parent of smooth, effect, derivative, sample and basis),
#' `geom.gratia.basis`, `geom.gratia.surface`, `geom.gratia.diagnostic.boxplot`,
#' `geom.gratia.residual.covariate.point`, `geom.gratia.diagnostic.reference`,
#' `geom.gratia.chain.reference`, `geom.gratia.correlation.reference`,
#' `geom.gratia.histogram.outline`, `geom.gratia.rootogram.reference`,
#' `geom.gratia.rootogram.warning`, `geom.gratia.correlation`, and
#' `geom.gratia.trend`.
#'
#' Opacity is set separately, using numeric elements between zero and one:
#' `gratia.alpha.interval`, `gratia.alpha.partial.residual`,
#' `gratia.alpha.diagnostic.point`, `gratia.alpha.rug`,
#' `gratia.alpha.sample`, `gratia.alpha.observed`,
#' `gratia.alpha.replicated`, `gratia.alpha.chain`,
#' `gratia.alpha.histogram`, and `gratia.alpha.surface.interval`.
#' More specific elements, such as `geom.gratia.smooth.interval`,
#' `geom.gratia.diagnostic.interval`, `geom.gratia.surface.interval`,
#' `geom.gratia.surface.rug`, and their `gratia.alpha.*` counterparts,
#' override the general component setting.
#'
#' Standard fields such as `colour`, `fill`, `linewidth`, `pointsize` and
#' `pointshape` set component defaults. Fields such as `ink` and `borderwidth`
#' follow the parent ggplot2 geom's interpretation.
#'
#' Default component settings preserve the existing plot appearance. Mapped
#' aesthetics retain their scales. Explicit aesthetic arguments to plotting
#' functions are deprecated but remain fixed overrides; use themes instead.
#' Palette settings use `palette.colour.discrete`, `palette.fill.discrete`,
#' `palette.colour.continuous`, and `palette.fill.continuous`. Complete scale
#' objects can still be supplied through the existing scale arguments.
#'
#' For a single ggplot, add a theme with `+`. For a patchwork returned by
#' [draw()] or [appraise()], use `&` to affect every panel. Plots delegated to
#' bayesplot retain that package's styling.
#'
#' @return `gratia_theme_get()` returns a theme or `NULL`. The setter and updater
#'   invisibly return the previous override.
#' @export
#' @examples
#' library("ggplot2")
#' old <- gratia_theme_set(theme(
#'   geom.gratia.smooth = element_geom(colour = "navy"),
#'   gratia.alpha.interval = 0.15
#' ))
#' gratia_theme_update(geom.gratia.contour = element_geom(colour = "grey40"))
#' gratia_theme_set(old)
#'
#' pal <- gratia_palette("tol_banded_16")
#' theme(palette.colour.discrete = pal, palette.fill.discrete = pal)
gratia_theme_get <- function() {
  value <- getOption("gratia.plot.theme")
  check_gratia_theme(value)
  value
}

#' @rdname gratia_theme_get
#' @export
gratia_theme_set <- function(theme = NULL) {
  check_gratia_theme(theme)
  old <- getOption("gratia.plot.theme")
  options(gratia.plot.theme = theme)
  invisible(old)
}

#' @rdname gratia_theme_get
#' @export
gratia_theme_update <- function(...) {
  old <- gratia_theme_get()
  new <- ggplot2::theme(...)
  if (!is.null(old)) new <- old + new
  gratia_theme_set(new)
  invisible(old)
}

check_gratia_theme <- function(theme) {
  if (is.null(theme)) return(invisible(NULL))
  if (!ggplot2::is_theme(theme)) {
    cli::cli_abort("{.arg gratia.plot.theme} must be a ggplot2 theme or NULL.")
  }
  for (key in grep("^gratia\\.alpha\\.", names(theme), value = TRUE)) {
    check_gratia_alpha(theme[[key]], key)
  }
  invisible(theme)
}

check_gratia_alpha <- function(value, key) {
  if (!is.null(value) && (!is.numeric(value) || length(value) != 1L ||
      !is.finite(value) || value < 0 || value > 1)) {
    cli::cli_abort("Theme setting {.val {key}} must be one number between zero and one.")
  }
}

# Theme registration defines types and inheritance without fixing defaults over
# the geoms. Each geom retains its existing defaults until the user changes it.
gratia_theme_roles <- function() {
  c("curve", "smooth", "effect", "derivative", "sample", "basis",
    "interval", "smooth.interval", "diagnostic.interval", "surface.interval",
    "partial.residual", "increase", "decrease", "change",
    "diagnostic.point", "diagnostic.boxplot", "residual.covariate.point", "reference",
    "diagnostic.reference", "chain.reference", "correlation.reference",
    "rug", "surface.rug", "contour", "boundary", "surface",
    "observed", "replicated", "chain", "histogram", "histogram.outline",
    "rootogram.bar", "rootogram.fitted", "rootogram.reference",
    "rootogram.warning", "concurvity.bar", "correlation", "trend")
}

gratia_role_parent <- function(role) {
  switch(role, smooth =, effect =, derivative =, sample =, basis = "curve",
    smooth.interval =, diagnostic.interval =, surface.interval = "interval",
    residual.covariate.point = "diagnostic.point", surface.rug = "rug",
    diagnostic.reference =, chain.reference =, correlation.reference =,
    rootogram.reference =, rootogram.warning = "reference",
    histogram.outline = "histogram", NULL)
}

.onLoad <- function(libname, pkgname) {
  roles <- gratia_theme_roles()
  tree <- list()
  for (role in roles) {
    parent <- gratia_role_parent(role)
    tree[[paste0("geom.gratia.", role)]] <- ggplot2::el_def("element_geom",
      if (!is.null(parent)) paste0("geom.gratia.", parent) else "geom")
    tree[[paste0("gratia.alpha.", role)]] <- ggplot2::el_def(c("numeric", "integer"),
      if (!is.null(parent)) paste0("gratia.alpha.", parent))
  }
  ggplot2::register_theme_elements(element_tree = tree, complete = FALSE)
}

# This constructor attaches only the requested override; ordinary global theme
# settings remain deferred to ggplot2, including for plots printed later.
gratia_plot <- function(...) {
  plot <- ggplot2::ggplot(...)
  theme <- gratia_theme_get()
  if (!is.null(theme)) plot <- plot + theme
  plot
}

# Apply renderer defaults without overwriting the captured package theme.
gratia_plot_theme_defaults <- function(plot, ...) {
  plot$theme <- ggplot2::theme(...) + plot$theme
  plot
}

# Remember explicit deprecated arguments while methods forward them. Attribute
# tagging preserves numeric/colour validation and does not change their values.
gratia_plot_arguments <- function(args, envir) {
  caller <- sys.call(sys.parent())[[1L]]
  if (is.call(caller) && identical(caller[[1L]], as.name("::"))) caller <- caller[[3L]]
  function_name <- if (is.symbol(caller)) sub("\\..*$", "", as.character(caller)) else "draw"
  defaults <- args[vapply(args, function(arg) {
    eval(call("missing", as.name(arg)), envir)
  }, logical(1))]
  for (arg in args) {
    value <- get(arg, envir, inherits = FALSE)
    if (isTRUE(attr(value, "gratia.plot.argument")) ||
        isTRUE(attr(value, "gratia.plot.default"))) next
    if (arg %in% defaults) {
      if (!is.null(value)) assign(arg, gratia_plot_default(value), envir)
      next
    }
    lifecycle::deprecate_warn("0.11.3", paste0(function_name, "(", arg, ")"),
      details = "Use ggplot2 theme settings; see ?gratia_theme_get.",
      env = envir, user_env = globalenv())
    if (is.null(value)) next
    attr(value, "gratia.plot.argument") <- TRUE
    assign(arg, value, envir)
  }
  invisible(NULL)
}

gratia_explicit <- function(x) isTRUE(attr(x, "gratia.plot.argument"))
gratia_unmark <- function(x) {
  attr(x, "gratia.plot.argument") <- NULL
  attr(x, "gratia.plot.default") <- NULL
  x
}

# Resolve semantic defaults at build time, preserving native precedence for
# mappings and explicit layer parameters. Drawing stays with the parent geom.
gratia_geom_defaults <- function(self, data, params = list(),
  modifiers = ggplot2::aes(), default_aes = NULL, theme = NULL, ...) {
  ae <- if (is.null(default_aes)) self$default_aes else default_aes
  role <- self$gratia_role
  element <- gratia_component_element(role, theme)
  if (!is.null(element)) {
    # Use the parent geom to interpret ink, paper, accent and border settings.
    # Its from_theme() expressions retain the native meaning of each field.
    theme[[paste0("geom.gratia.", role)]] <- element
    native_aes <- self$gratia_parent$default_aes
    for (aesthetic in intersect(names(ae), names(native_aes))) {
      fields_used <- all.vars(rlang::get_expr(native_aes[[aesthetic]]))
      requested <- vapply(fields_used, function(field) {
        !is.null(element[[field]])
      }, logical(1))
      if (any(requested)) ae[aesthetic] <- native_aes[aesthetic]
    }
  }
  fields <- c(colour = "colour", fill = "fill", linewidth = "linewidth",
    linetype = "linetype", size = "pointsize", shape = "pointshape",
    family = "family")
  for (aesthetic in intersect(names(fields), names(ae))) {
    if (!is.null(element)) {
      value <- element[[fields[[aesthetic]]]]
      if (!is.null(value)) ae[[aesthetic]] <- value
    }
  }
  alpha <- ggplot2::calc_element(paste0("gratia.alpha.", role), theme)
  check_gratia_alpha(alpha, paste0("gratia.alpha.", role))
  if (!is.null(alpha) && "alpha" %in% names(ae)) ae$alpha <- alpha
  # Joined legend keys may contain aesthetics from another layer. Defaults
  # that were fixed in the old renderer must also retain their legend values.
  data[intersect(names(data), self$gratia_fixed_defaults)] <- NULL
  ggplot2::ggproto_parent(self$gratia_parent, self)$use_defaults(data,
    params = params, modifiers = modifiers, default_aes = ae, theme = theme, ...)
}

# A semantic subclass can have several native geometry variants. Per-layer
# defaults retain historical differences, including outlines and line widths.
gratia_geom <- function(role, layer) {
  if (!inherits(layer, "Layer")) {
    if (is.list(layer)) return(lapply(layer, function(x) {
      if (inherits(x, "Layer")) gratia_geom(role, x) else x
    }))
    return(layer)
  }
  native <- layer$geom
  ae <- native$default_aes
  fixed_defaults <- character()
  for (name in names(layer$aes_params)) {
    value <- layer$aes_params[[name]]
    if (gratia_explicit(value)) {
      layer$aes_params[[name]] <- gratia_unmark(value)
    } else {
      ae[name] <- list(gratia_unmark(value))
      fixed_defaults <- c(fixed_defaults, name)
      layer$aes_params[name] <- NULL
      # A formerly fixed default must not inherit a mapping from the plot.
      layer$mapping[name] <- list(NULL)
    }
  }
  class_name <- paste0("GeomGratia", paste0(
    toupper(substring(strsplit(role, ".", fixed = TRUE)[[1]], 1L, 1L)),
    substring(strsplit(role, ".", fixed = TRUE)[[1]], 2L), collapse = ""))
  layer$geom <- ggplot2::ggproto(class_name, native, default_aes = ae,
    gratia_role = role, gratia_parent = native,
    gratia_fixed_defaults = fixed_defaults,
    use_defaults = gratia_geom_defaults)
  layer
}

# Keep scale configuration intact; only defer its default palette to the theme.
gratia_default_scale <- function(scale) {
  if (is.null(scale$palette)) return(scale)
  palette <- scale$palette
  scale$fallback_palette <- function(...) palette(...)
  scale$palette <- NULL
  scale
}

# Internal constructors retain the native geom's arguments and drawing code.
geom_gratia_partial_residual_point <- function(...) gratia_geom("partial.residual", ggplot2::geom_point(...))
geom_gratia_change_line <- function(...) gratia_geom("change", ggplot2::geom_line(...))
geom_gratia_smooth_line <- function(...) gratia_geom("smooth", ggplot2::geom_line(...))
geom_gratia_rug <- function(...) gratia_geom("rug", ggplot2::geom_rug(...))
geom_gratia_surface_rect <- function(...) gratia_geom("surface", ggplot2::geom_rect(...))
geom_gratia_surface_raster <- function(...) gratia_geom("surface", ggplot2::geom_raster(...))
geom_gratia_surface_tile <- function(...) gratia_geom("surface", ggplot2::geom_tile(...))
geom_gratia_contour <- function(...) gratia_geom("contour", ggplot2::geom_contour(...))
geom_gratia_smooth_point <- function(...) gratia_geom("smooth", ggplot2::geom_point(...))
geom_gratia_boundary_path <- function(...) gratia_geom("boundary", ggplot2::geom_path(...))
geom_gratia_reference_abline <- function(...) gratia_geom("reference", ggplot2::geom_abline(...))
geom_gratia_smooth_pointrange <- function(...) gratia_geom("smooth", ggplot2::geom_pointrange(...))
geom_gratia_surface_sf <- function(...) gratia_geom("surface", ggplot2::geom_sf(...))
geom_gratia_contour_sf <- function(...) gratia_geom("contour", ggplot2::geom_sf(...))
geom_gratia_increase_line <- function(...) gratia_geom("increase", ggplot2::geom_line(...))
geom_gratia_decrease_line <- function(...) gratia_geom("decrease", ggplot2::geom_line(...))
geom_gratia_basis_line <- function(...) gratia_geom("basis", ggplot2::geom_line(...))
geom_gratia_reference_hline <- function(...) gratia_geom("reference", ggplot2::geom_hline(...))
geom_gratia_effect_pointrange <- function(...) gratia_geom("effect", ggplot2::geom_pointrange(...))
geom_gratia_diagnostic_reference_abline <- function(...) gratia_geom("diagnostic.reference", ggplot2::geom_abline(...))
geom_gratia_diagnostic_interval_ribbon <- function(...) gratia_geom("diagnostic.interval", ggplot2::geom_ribbon(...))
geom_gratia_diagnostic_point <- function(...) gratia_geom("diagnostic.point", ggplot2::geom_point(...))
geom_gratia_diagnostic_reference_hline <- function(...) gratia_geom("diagnostic.reference", ggplot2::geom_hline(...))
geom_gratia_histogram <- function(...) gratia_geom("histogram", ggplot2::geom_histogram(...))
geom_gratia_histogram_step <- function(...) gratia_geom("histogram", ggplot2::geom_step(...))
geom_gratia_histogram_rect <- function(...) gratia_geom("histogram", ggplot2::geom_rect(...))
geom_gratia_histogram_outline_path <- function(...) gratia_geom("histogram.outline", ggplot2::geom_path(...))
geom_gratia_residual_covariate_point <- function(...) gratia_geom("residual.covariate.point", ggplot2::geom_point(...))
geom_gratia_diagnostic_boxplot <- function(...) gratia_geom("diagnostic.boxplot", ggplot2::geom_boxplot(...))
geom_gratia_trend_line <- function(...) gratia_geom("trend", ggplot2::geom_line(...))
geom_gratia_replicated_pointrange <- function(...) gratia_geom("replicated", ggplot2::geom_pointrange(...))
geom_gratia_observed_point <- function(...) gratia_geom("observed", ggplot2::geom_point(...))
geom_gratia_replicated_line <- function(...) gratia_geom("replicated", ggplot2::geom_line(...))
geom_gratia_observed_line <- function(...) gratia_geom("observed", ggplot2::geom_line(...))
geom_gratia_chain_line <- function(...) gratia_geom("chain", ggplot2::geom_line(...))
geom_gratia_chain_reference_hline <- function(...) gratia_geom("chain.reference", ggplot2::geom_hline(...))
geom_gratia_chain_step <- function(...) gratia_geom("chain", ggplot2::geom_step(...))
geom_gratia_interval_ribbon <- function(...) gratia_geom("interval", ggplot2::geom_ribbon(...))
geom_gratia_effect_line <- function(...) gratia_geom("effect", ggplot2::geom_line(...))
geom_gratia_reference_qq_line <- function(...) gratia_geom("reference", ggplot2::geom_qq_line(...))
geom_gratia_smooth_linerange <- function(...) gratia_geom("smooth.interval", ggplot2::geom_linerange(...))
geom_gratia_concurvity_bar_col <- function(...) gratia_geom("concurvity.bar", ggplot2::geom_col(...))
geom_gratia_rootogram_bar_rect <- function(...) gratia_geom("rootogram.bar", ggplot2::geom_rect(...))
geom_gratia_rootogram_fitted_line <- function(...) gratia_geom("rootogram.fitted", ggplot2::geom_line(...))
geom_gratia_rootogram_fitted_point <- function(...) gratia_geom("rootogram.fitted", ggplot2::geom_point(...))
geom_gratia_rootogram_reference_hline <- function(...) gratia_geom("rootogram.reference", ggplot2::geom_hline(...))
geom_gratia_rootogram_warning_hline <- function(...) gratia_geom("rootogram.warning", ggplot2::geom_hline(...))
geom_gratia_correlation_reference_hline <- function(...) gratia_geom("correlation.reference", ggplot2::geom_hline(...))
geom_gratia_correlation_segment <- function(...) gratia_geom("correlation", ggplot2::geom_segment(...))
geom_gratia_correlation_point <- function(...) gratia_geom("correlation", ggplot2::geom_point(...))

geom_gratia_surface_rug_point <- function(...) gratia_geom("surface.rug", ggplot2::geom_point(...))
geom_gratia_surface_rug_sf <- function(...) gratia_geom("surface.rug", ggplot2::geom_sf(...))
gratia_change_geom <- function(variable, ...) {
  role <- switch(variable, .increase = "increase", .decrease = "decrease", "change")
  gratia_geom(role, ggplot2::geom_line(...))
}

# Resolve the styling of individual primitives without separating the scene.
gratia_surface_defaults <- function(self, data, params = list(),
  modifiers = ggplot2::aes(), default_aes = NULL, theme = NULL, ...) {
  data <- ggplot2::ggproto_parent(self$gratia_scene_parent, self)$use_defaults(
    data, params = params, modifiers = modifiers, default_aes = default_aes,
    theme = theme, ...)
  for (key in c("surface_colour_explicit", "surface_fill_explicit", "surface_alpha_explicit")) {
    data[[key]][is.na(data[[key]])] <- FALSE
  }
  data$surface_fill_fixed <- !is.na(data$surface_fixed_fill)
  data$surface_size <- rep(1.5, nrow(data))
  data$surface_shape <- rep(19, nrow(data))
  roles <- as.character(data$surface_role)
  roles[roles %in% c("lower", "upper")] <- "surface.interval"
  roles[roles == "estimate"] <- "surface"
  for (role in unique(roles)) {
    rows <- which(roles == role)
    element <- gratia_component_element(role, theme)
    if (!is.null(element)) {
      fields <- c(colour = "surface_colour", fill = "surface_fixed_fill",
        linewidth = "surface_linewidth", pointsize = "surface_size",
        pointshape = "surface_shape")
      for (field in names(fields)) {
        value <- element[[field]]
        if (is.null(value)) next
        explicit <- if (field == "colour") data$surface_colour_explicit else
          if (field == "fill") data$surface_fill_explicit else FALSE
        take <- rows[!rep_len(explicit, nrow(data))[rows]]
        data[[fields[[field]]]][take] <- value
        if (field == "fill") data$surface_fill_fixed[take] <- TRUE
      }
    }
    alpha <- ggplot2::calc_element(paste0("gratia.alpha.", role), theme)
    check_gratia_alpha(alpha, paste0("gratia.alpha.", role))
    if (!is.null(alpha)) {
      take <- rows[!data$surface_alpha_explicit[rows]]
      data$surface_alpha[take] <- alpha
    }
  }
  data
}

# Merge only settings requested for this component; its historical fallback
# takes precedence over unspecified generic geom fields.
gratia_component_element <- function(role, theme) {
  if (is.null(theme)) return(NULL)
  parent <- gratia_role_parent(role)
  inherited <- if (!is.null(parent)) gratia_component_element(parent, theme)
  element <- theme[[paste0("geom.gratia.", role)]]
  if (is.null(element)) return(inherited)
  if (is.null(inherited)) return(element)
  ggplot2::merge_element(element, inherited)
}

# Preserve a caller-specific default when it is forwarded under another name.
gratia_plot_default <- function(value) {
  attr(value, "gratia.plot.default") <- TRUE
  value
}
