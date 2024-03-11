#' Extract and tidy penalty matrices
#'
#' @param object a fitted GAM or a smooth.
#' @param select character, logical, or numeric; which smooths to extract
#'   penalties for. If `NULL`, the default, then penalties for all model
#'   smooths are drawn. Numeric `select` indexes the smooths in the order they
#'   are specified in the formula and stored in `object`. Character `select`
#'   matches the labels for smooths as shown for example in the output from
#'   `summary(object)`. Logical `select` operates as per numeric `select` in
#'   the order that smooths are stored.
#' @param smooth `r lifecycle::badge("deprecated")` Use `select` instead.
#' @param rescale logical; by default, *mgcv* will scale the penalty matrix for
#'   better performance in [mgcv::gamm()]. If `rescale` is `TRUE`, this scaling
#'   will be undone to put the penalty matrix back on the original scale.
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#' @param margins logical; extract the penalty matrices for the tensor
#'   product or the marginal smooths of the tensor product?
#' @param data data frame; a data frame of values for terms mentioned in the
#'   smooth specification.
#' @param ... additional arguments passed to methods.
#'
#' @return A 'tibble' (data frame) of class `penalty_df` inheriting from
#'   `tbl_df`, with the following components:
#' * `.smooth` - character; the label *mgcv* uses to refer to the smooth,
#' * `.type` - character; the type of smooth,
#' * `.penalty` - character; the label for the specific penalty. Some smooths
#'   have multiple penalty matrices, so the `penalty` component identifies the
#'   particular penalty matrix and uses the labelling that *mgcv* uses
#'   internally,
#' * `.row` - character; a label of the form `fn` where `n` is an integer for
#'   the `n`th basis function, referencing the columns of the penalty matrix,
#' * `.col` - character; a label of the form `fn` where `n` is an integer for
#'   the `n`th basis function, referencing the columns of the penalty matrix,
#' * `.value` - double; the value of the penalty matrix for the combination of
#'   `row` and `col`,
#'
#' @note The `print()` method uses [base::zapsmall()] to turn very small numbers
#'   into 0s for display purposes only; the underlying values of the penalty
#'   matrix or matrices are not changed.
#'
#'   For smooths that are subject to an eigendecomposition (e.g. the default
#'   thin plate regression splines, `bs = "tp"`), the signs of the eigenvectors
#'   are not defined and as such you can expect differences across systems in
#'   the penalties for such smooths that are system-, OS-, and CPU architecture-
#'   specific.
#'
#' @author Gavin L. Simpson
#' @export
#' @importFrom lifecycle deprecated is_present
#'
#' @examples
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 3)
#' }
#' load_mgcv()
#' dat <- data_sim("eg4", n = 400, seed = 42)
#' m <- gam(
#'   y ~ s(x0, bs = "cr") + s(x1, bs = "cr") +
#'     s(x2, by = fac, bs = "cr"),
#'   data = dat, method = "REML"
#' )
#'
#' # penalties for all smooths
#' penalty(m)
#'
#' # for a specific smooth
#' penalty(m, select = "s(x2):fac1")
#'
#' \dontshow{
#' options(op)
#' }
`penalty` <- function(object, ...) {
  UseMethod("penalty")
}

#' @export
#' @rdname penalty
#' @inheritParams basis.default
`penalty.default` <- function(
    object, rescale = FALSE, data, knots = NULL,
    constraints = FALSE, ...) {
  # class of object and check for ".smooth.spec"
  cls <- class(object)
  if (str_detect(cls, "smooth.spec", negate = TRUE)) {
    stop("'object' doesn't appear to be a smooth created by {mgcv}.")
  }
  ## call smoothCon to create the basis as specified in `object`
  sm <- smoothCon(object, data = data, knots = knots, absorb.cons = constraints)

  out <- if (length(sm) == 1L) {
    penalty(sm[[1]])
  } else {
    lapply(sm, penalty) |> bind_rows() # ?
  }
  out
}

#' @export
#' @rdname penalty
`penalty.gam` <- function(object, select = NULL, smooth = deprecated(),
    rescale = FALSE, partial_match = FALSE, ...) {
  if (lifecycle::is_present(smooth)) {
    lifecycle::deprecate_warn("0.8.9.9", "penalty(term)",
      "penalty(select)")
    select <- smooth
  }
  ## are particular smooths selected
  smooth_ids <- if (!is.null(select)) {
    check_user_select_smooths(smooths = smooths(object), select = select,
    partial_match = partial_match) |>
      which() # which_smooths(object, smooth) # which smooths match 'smooth'
  } else {
    seq_len(n_smooths(object))
  }

  ## extract the mgcv.smooth objects
  smooths <- get_smooths_by_id(object, smooth_ids)

  ## loop over the smooths applying penalty to each
  pen <- lapply(smooths, penalty, rescale = rescale)
  pen <- bind_rows(pen)
  ## ensure class is right
  ## don't think I need this; bind_rows does it, but documented?
  if (!inherits(pen, "penalty_df")) {
    class(pen) <- c("penalty_df", class(pen))
  }
  pen
}

#' @export
#' @rdname penalty
`penalty.mgcv.smooth` <- function(object, rescale = FALSE, ...) {
  ## smooth label
  sm_lab <- smooth_label(object)
  ## type of smooth
  sm_type <- smooth_type(object)
  ## extract the set of penalty matrices
  S <- object[["S"]] # S is a list even if length(S) == 1
  len_S <- length(S)
  sp_label <- if (is.null(object[["sp"]])) {
    paste(sm_lab, seq_len(len_S), sep = ".")
  } else {
    names(object[["sp"]]) # penalty matrix label
  }
  pen <- vector("list", length = len_S)
  ## loop over penalty matrices & tidy each of them
  pen_seq <- seq_along(pen)
  for (i in pen_seq) {
    pen[[i]] <- tidy_penalty(S[[i]],
      smooth = sm_lab,
      type = sm_type,
      label = sp_label[i]
    )
  }

  # should the default penalty rescaling for gamm performance be reversed?
  if (rescale) {
    for (i in pen_seq) {
      S_scale <- object[["S.scale"]][i]
      if (is.null(S_scale)) {
        S_scale <- 1
      }
      pen[[i]][[".value"]] <- pen[[i]][[".value"]] * S_scale
    }
  }

  # combine the tidy penalty matrices into a single tibble
  pen <- bind_rows(pen)
  class(pen) <- c("penalty_df", class(pen))
  pen
}

#' @export
#' @rdname penalty
`penalty.tensor.smooth` <- function(object, margins = FALSE, ...) {
  .NotYetImplemented()
}

#' @export
#' @rdname penalty
`penalty.t2.smooth` <- function(object, margins = FALSE, ...) {
  .NotYetImplemented()
}

#' @export
#' @importFrom mgcv smoothCon
#' @importFrom dplyr bind_rows
#'
#' @rdname penalty
`penalty.re.smooth.spec` <- function(object, data, ...) {
  sm <- smoothCon(object, data, ...)
  pen <- lapply(sm, penalty, ...)
  pen <- bind_rows(pen)
  pen
}

#' @importFrom tibble add_column as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with
#' @importFrom rlang set_names
`tidy_penalty` <- function(s, smooth, type, label) {
  nc <- ncol(s)
  new_names <- formatC(seq_len(nc), width = nchar(nc), flag = "0")
  new_names <- paste0("F", new_names)
  s <- s |>
    as_tibble(.name_repair = "minimal") |>
    set_names(new_names) |>
    add_column(.row = new_names, .before = 1L) |>
    pivot_longer(
      cols = starts_with("f"), names_to = ".col",
      values_to = ".value"
    )

  ns <- nrow(s)
  s <- add_column(s,
    .smooth = rep(smooth, ns),
    .type = rep(type, ns),
    .penalty = rep(label, ns), .before = 1L
  )
  s
}

#' @export
#' @importFrom rlang .data
`print.penalty_df` <- function(x, ...) {
  x <- mutate(x, .value = zapsmall(.data$.value))
  NextMethod()
}
