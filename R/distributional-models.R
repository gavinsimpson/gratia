# Functions for distributional GAMs that don't fit naturally elsewhere

#' General names of LSS parameters for each GAM family
#'
#' @keywords internal
lss_parameters <- function(object) {
  fn <- family_type(object)
  par_names <- switch(fn,
    "gaulss"  = c("location", "scale"),
    "gammals" = c("location", "scale"),
    "gumbls"  = c("location", "scale"),
    "gevlss"  = c("location", "scale", "shape"),
    "shash"   = c("location", "scale", "skewness", "kurtosis"),
    "ziplss"  = c("location", "pi"),
    "twlss"   = c("location", "power", "scale"),
    "location" # <- default, for most GAM families that's all there is
  )
  par_names
}

#' @importFrom purrr map
lss_links <- function(object, inverse = FALSE, which_eta = NULL) {
  params <- lss_parameters(object)
  param_nms <- c(
    "location", "scale", "shape", "skewness", "kurtosis",
    "power", "pi"
  )
  out <- rep(list(identity_fun), length(param_nms)) |>
    setNames(param_nms)
  funs <- purrr::map(params, .f = function(p, model, inverse, which_eta) {
    extract_link(family(model),
      parameter = p, inverse = inverse,
      which_eta = which_eta
    )
  }, model = object, inverse = inverse, which_eta = which_eta) |>
    setNames(params)
  out[params] <- funs
  out
}

# an identity function that simply returns input
identity_fun <- function(eta) {
  eta
}
