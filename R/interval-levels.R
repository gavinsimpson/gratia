# Check only interval controls; leave prediction and layout arguments in dots.
check_interval_level <- function(level, arg, dots = character()) {
  bad <- intersect(dots, setdiff(c("level", "ci_level", "coverage"), arg))
  if (length(bad)) {
    bad <- bad[[1L]]
    cli::cli_abort(
      "{.arg {bad}} is not supported here. Use {.arg {arg}} to set the interval level."
    )
  }
  if (!is.numeric(level) || is.complex(level) || length(level) != 1L ||
      !is.finite(level) || level <= 0 || level >= 1) {
    cli::cli_abort(
      "{.arg {arg}} must be one finite numeric value strictly between 0 and 1."
    )
  }
  invisible(NULL)
}
