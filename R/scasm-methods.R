# scasm fits retain class gam. Route their plots without changing the model.
#' @export
#' @rdname draw.gam
draw.scasm <- function(object, ..., overall_uncertainty = FALSE,
                       interval_method = c("auto", "gaussian", "bootstrap")) {
  draw_gam_impl(object, ..., overall_uncertainty = overall_uncertainty,
    interval_method = interval_method)
}

scasm_overall_uncertainty <- function(object, value, supplied,
                                      smooths = object$smooth) {
  if (!is_scasm_model(object)) return(value)
  if (!supplied) return(FALSE)
  unavailable <- vapply(smooths, function(sm) {
    n <- attr(sm, "nCons")
    is.null(n) || !isTRUE(n > 0L)
  }, logical(1))
  if (isTRUE(value) && any(unavailable)) {
    warning("Overall mean uncertainty is unavailable for selected scasm smooths; using component-only uncertainty for those smooths.",
      call. = FALSE)
  }
  value
}
