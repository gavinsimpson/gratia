#' Discrete palettes for gratia plots
#'
#' Create a palette function for ggplot2 theme settings. The default preserves
#' gratia's existing nine-colour Okabe-Ito palette and hue fallback.
#'
#' @param name Palette name: `"default"`, `"tol_bright"`, `"tol_vibrant"`,
#'   `"tol_muted"`, `"tol_dark"`, `"tol_banded_12"`, `"tol_banded_16"`, or
#'   `"tol_banded_20"`.
#' @param reverse Logical; reverse the palette order?
#'
#' @details
#' Tol's bright and vibrant palettes contain seven colours, muted contains
#' nine, and dark contains eleven. The dark palette favours visibility against
#' white over separation between colours. Banded palettes provide 12, 16 or 20
#' colours; neighbouring shades become less distinct as the palette grows.
#' These are optional choices and do not change gratia's defaults.
#'
#' Finite palettes use the published order, returning the first requested
#' colours. They error if their capacity is exceeded; they never recycle or
#' interpolate colours. The default palette uses hue colours above nine levels.
#' Set both colour and fill palettes to keep grouped curves and ribbons paired.
#'
#' Definitions follow Paul Tol's colour schemes as published in August 2026.
#' The banded schemes are derived from his sequential schemes for use when
#' larger sets of colours are required. See
#' <https://sronpersonalpages.nl/~pault/> for the definitions and examples.
#'
#' @return A function taking the number of colours and returning a character
#'   vector of hexadecimal colours.
#' @export
#' @examples
#' pal <- gratia_palette("tol_muted")
#' \dontshow{
#' pal <- utils::removeSource(pal)
#' }
#' pal(5)
#' ggplot2::theme(
#'   palette.colour.discrete = pal,
#'   palette.fill.discrete = pal
#' )
gratia_palette <- function(name = "default", reverse = FALSE) {
  palettes <- gratia_tol_palettes()
  name <- match.arg(name, c("default", names(palettes)))
  if (!is.logical(reverse) || length(reverse) != 1L || is.na(reverse)) {
    cli::cli_abort("{.arg reverse} must be TRUE or FALSE.")
  }
  colours <- if (name == "default") NULL else palettes[[name]]
  if (reverse && !is.null(colours)) colours <- rev(colours)
  force(name)
  function(n) {
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 0 || n != trunc(n)) {
      cli::cli_abort("The number of colours must be one non-negative integer.")
    }
    if (is.null(colours)) {
      values <- if (n > 9L) scales::pal_hue()(n) else
        gratia_okabe_ito()[seq_len(n)]
      if (reverse) values <- rev(values)
      return(values)
    }
    if (n > length(colours)) {
      cli::cli_abort("Palette {.val {name}} provides at most {length(colours)} colours; {n} requested.")
    }
    colours[seq_len(n)]
  }
}

# Keep the established plotting order: orange first and black last.
gratia_okabe_ito <- function() {
  unname(grDevices::palette.colors(n = 9, palette = "Okabe-Ito")[c(2:9, 1)])
}

gratia_okabe_ito_palette <- function(n) {
  values <- gratia_okabe_ito()
  if (n > length(values)) {
    stop("Insufficient values in manual scale. ", n,
      " needed but only ", length(values), " provided.", call. = FALSE)
  }
  values
}

scale_colour_okabe_ito <- function(...) {
  ggplot2::discrete_scale("colour", palette = gratia_okabe_ito_palette, ...)
}

scale_fill_okabe_ito <- function(...) {
  ggplot2::discrete_scale("fill", palette = gratia_okabe_ito_palette, ...)
}

# Colour data, in the published default order, from Paul Tol's Notes.
gratia_tol_palettes <- function() {
  list(
    tol_bright = c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB"),
    tol_vibrant = c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311", "#009988", "#BBBBBB"),
    tol_muted = c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499"),
    tol_dark = c("#4477BB", "#CC4466", "#228833", "#2255AA", "#993366", "#006644", "#117788", "#882288", "#BB4488", "#775500", "#444444"),
    tol_banded_12 = c("#D2D1E2", "#C1A7C4", "#AD7BA4", "#93C7EC", "#6DA1D6", "#3E79BD", "#B8D29E", "#7BB47D", "#2C925B", "#F4A937", "#EB7121", "#D9070D"),
    tol_banded_16 = c("#D2D1E2", "#C5B3CC", "#B893B5", "#A8719D", "#97CDEF", "#7DB1DF", "#5F94CE", "#3976BB", "#BCD4A0", "#90BE88", "#60A770", "#198D56", "#F5B43B", "#F08C2B", "#E85E1B", "#D9070D"),
    tol_banded_20 = c("#D2D1E2", "#C8B9D1", "#BEA0BF", "#B287AD", "#A56B99", "#9ACFF1", "#85BAE4", "#6FA3D7", "#578DC9", "#3774BA", "#BFD6A1", "#9CC48E", "#78B27B", "#4F9F68", "#058A53", "#F6B93D", "#F29B31", "#ED7A24", "#E65318", "#D9070D")
  )
}
