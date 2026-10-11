#' Generate regular data over the covariates of a smooth
#'
#' @param model a fitted model
#' @param id the number ID of the smooth within `model` to process.
#' @param n numeric; the number of new observations to generate.
#' @param n_2d numeric; the number of points along each of the first two axes
#'   of a smooth surface, including surface panels of higher-dimensional smooths.
#'   The default, `NULL`, uses `n` instead. Factor levels
#'   are retained, and curves with only one continuous covariate use `n`.
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth. For factor conditioning dimensions, retain at most
#'   this many levels, taking the first in fitted order.
#' @param n_4d numeric; the number of new observations to generate for the
#'   dimensions higher than 2 (!) of a *k*D smooth (*k* >= 4). For example, if
#'   the smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d`
#'   new observations. For factor conditioning dimensions, retain at most this
#'   many levels per dimension, taking the first in fitted order.
#' @details
#' Spline-on-the-sphere grids use evenly spaced observed-range latitude and
#' longitude values. Plotting constructs geographic cell edges separately,
#' without moving these evaluation coordinates.
#'
#' @param offset numeric; value of the model offset to use.
#' @param include_all logical; include all covariates involved in the smooth?
#'   if `FALSE`, only the covariates involved in the smooth will be included in
#'   the returned data frame. If `TRUE`, a representative value will be included
#'   for all other covariates in the model that aren't actually used in the
#'   smooth. This can be useful if you want to pass the returned data frame on
#'   to [mgcv::PredictMat()].
#' @param var_order character; the order in which the terms in the smooth
#'   should be processed. Only useful for tensor products with at least one
#'   2d marginal smooth.
#'
#' @param back_transform logical; generate the grid on original covariate
#'   scales? The default, `FALSE`, uses evenly spaced model coordinates. With
#'   `TRUE`, recover raw fitting rows and generate original-scale coordinates,
#'   then evaluate the model expressions forwards. Each axis must depend on one
#'   distinct original model covariate. Both raw and evaluated columns are
#'   returned, with auxiliary `.gratia_raw_*` columns for transformed axes.
#'   Missing raw inputs or unavailable functions produce an error.
#'   Numeric `by` multipliers are one on automatic grids, including when a
#'   multiplier shares an original axis name; raw axis coordinates are retained
#'   separately. If the multiplier is itself an untransformed grid axis, supply
#'   prediction `data` to `smooth_estimates()` instead. Typical values added by
#'   `include_all = TRUE` to model-scale
#'   grids do not provide raw coordinates for transformed axes.
#' @inheritParams smooth_estimates
#'
#' @export
#'
#' @importFrom dplyr bind_cols setdiff
#' @importFrom tibble as_tibble
#' @importFrom rlang exec !!!
#' @importFrom tidyr expand_grid
#'
#' @examples
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 4)
#' }
#' load_mgcv()
#' df <- data_sim("eg1", seed = 42)
#' m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)
#'
#' # generate data over range of x1 for smooth s(x1)
#' smooth_data(m, id = 2)
#'
#' # generate data over range of x1 for smooth s(x1), with typical value for
#' # other covariates in the model
#' smooth_data(m, id = 2, include_all = TRUE)
#'
#' \dontshow{
#' options(op)
#' }
`smooth_data` <- function(model, id, n = 100, n_2d = NULL, n_3d = NULL,
                          n_4d = NULL, offset = NULL, include_all = FALSE,
                          var_order = NULL, envir = NULL, back_transform = FALSE) {
  check_back_transform(back_transform)
  model <- with_model_envir(model, envir)
  mf <- model.frame(model) # model.frame used to fit model

  ## remove response
  respvar <- attr(model$terms, "response")
  if (!identical(respvar, 0)) {
    mf <- mf[, -respvar, drop = FALSE]
  }

  # Isolated smooth grids do not need model offsets.
  ff <- vapply(mf, is.factor, logical(1L)) # which, if any, are factors vars
  ## list of model terms (variable names); extract these from `var.summary`
  ## because model.frame() on a gamm() contains extraneous variables, related
  ## to the mixed model form for lme()
  m.terms <- names(model[["var.summary"]])

  ## need a list of terms used in current smooth
  sm <- get_smooths_by_id(model, id)[[1L]]
  mf <- prepare_smooth_data(model, sm)
  plot_spec <- tensor_plot_spec(model, id)
  if (!is.null(plot_spec)) var_order <- plot_spec$variables
  orig_order <- unique(smooth_variable(sm))
  smooth_vars <- if (is.null(var_order)) {
    orig_order
  } else {
    var_order
  }
  # Random-effect tensor conditioning dimensions follow the continuous axes.
  if (!is.null(sm$margin) &&
      any(vapply(sm$margin, inherits, logical(1), "random.effect"))) {
    smooth_vars <- tensor_plot_variables(smooth_vars, mf)
  }
  ## is smooth a by? If it is, extract the by variable
  by_var <- if (is_by_smooth(sm)) {
    by_variable(sm)
  } else {
    NULL
  }
  grid_vars <- smooth_vars
  if (back_transform) {
    axes <- lapply(smooth_vars, smooth_coordinate_info, model = model)
    if (any(vapply(axes, function(x) is.null(x$original), logical(1)))) {
      cli::cli_abort("Each raw-scale grid axis must depend on one original model covariate.")
    }
    grid_vars <- vapply(axes, `[[`, character(1), "original")
    if (anyDuplicated(grid_vars)) cli::cli_abort("Original covariates for grid axes must be distinct.")
    raw <- recover_raw_data(model, vars = unique(grid_vars))
    # Even when raw columns are stored in the model, changed functions or
    # constants must not silently define a different covariate transformation.
    reference <- evaluate_terms(raw, smooth_vars, model)
    for (label in smooth_vars) {
      if (!isTRUE(all.equal(reference[[label]], mf[[label]],
          check.attributes = FALSE))) {
        cli::cli_abort(c("Cannot reproduce fitted coordinate {.val {label}} from raw covariates.",
          "i" = "Restore the fitting functions and constants in `envir`, or supply explicit raw prediction `data`."))
      }
    }
    for (v in grid_vars) mf[[v]] <- raw[[v]]
  }
  used_vars <- c(smooth_vars, by_var)

  # Figure out the n to use for each dimension
  sm_dim <- smooth_dim(sm)
  # NULL resolution controls fall back to n.
  if (is.null(n_2d)) {
    n_2d <- n
  }
  if (is.null(n_3d)) {
    n_3d <- n
  }
  # likewise fix up n_4d; set it to `n` if `n_4d` is NULL
  if (is.null(n_4d)) {
    n_4d <- n
  }
  seq_per_dim <- function(data, vars, dim, n, n_2d, n_3d, n_4d,
      round_extra = TRUE) {
    if (dim > 2L && any(vapply(data[vars[seq.int(3L, dim)]],
        is.factor, logical(1)))) {
      resolution <- if (dim == 3L) n_3d else n_4d
      if (!is.numeric(resolution) || length(resolution) != 1L ||
          !is.finite(resolution) || resolution < 1 ||
          resolution != floor(resolution)) {
        cli::cli_abort("Factor conditioning resolution must be a positive integer.")
      }
    }
    n_per_dim <- rep(n, dim)
    # Factor levels do not turn a curve into a surface.
    if (sum(!vapply(data[vars], is.factor, logical(1))) >= 2L) {
      n_per_dim[1:2] <- n_2d
    }
    if (dim == 3L) {
      n_per_dim[3] <- n_3d
    } else if (dim > 3L) {
      n_per_dim[seq_len(dim - 2) + 2] <- n_4d
    }
    seq_min_max_wrapper <- function(i, data, vars, n) {
      # if dim >= 2 we want n_3d or n_4d pretty values, but pretty()
      # won't return exactly the right n
      out <- seq_min_max(data[[vars[i]]], n = n[i])
      # Dimension controls also bound categorical conditioning grids. Explicit
      # level selections are authoritative; never truncate them afterwards.
      if (is.factor(out) && i > 2L &&
          !smooth_vars[i] %in% names(plot_spec$levels)) {
        out <- utils::head(out, n[i])
      }
      # now that we have the ordering of vars corrected we can round here
      if (round_extra && i > 2L && !(is.factor(data[[vars[i]]]))) {
        out <- round(out, 3)
      }
      out
      out
    }
    out <- lapply(seq_along(vars),
      FUN = seq_min_max_wrapper,
      data = data, vars = vars, n = n_per_dim
    )
    out <- setNames(out, vars)
    out
  }

  ## generate covariate values for the smooth
  # newlist <- lapply(mf[smooth_vars], seq_min_max, n = n)
  if (inherits(sm, "sos.smooth")) {
    newlist <- sos_data(mf, vars = smooth_vars, n = n_2d)
  } else {
    newlist <- seq_per_dim(
      data = mf, vars = grid_vars, dim = sm_dim,
      n = n, n_2d = n_2d, n_3d = n_3d, n_4d = n_4d,
      round_extra = !back_transform
    )
  }

  if (!is.null(plot_spec)) {
    for (v in names(plot_spec$levels)) {
      target <- if (back_transform) grid_vars[match(v, smooth_vars)] else v
      newlist[[target]] <- newlist[[target]][as.character(newlist[[target]]) %in% plot_spec$levels[[v]]]
    }
  }

  if (!is.null(by_var)) {
    if (is_factor_by_smooth(sm)) {
      ## ordered or simple factor? Grab class as a function to apply below
      FUN <- match.fun(data.class(mf[[by_var]]))
      ## extract levels of factor by var,
      levs <- levels(mf[[by_var]])
      ## coerce level for this smooth to correct factor type with FUN
      ##   return as a list with the correct names
      newfac <- setNames(list(FUN(by_level(sm), levels = levs)), by_var)
      ## append this list to the list of new smooth covariate values
      newlist <- append(newlist, newfac)
    } else {
      ## continuous by var; set to median among observed values?
      ## newby <- setNames(list(median(mf[[by_var]]), na.rm = TRUE), by_var)
      ## no, should be set to 1
      newby <- setNames(list(1L), by_var)
      if (back_transform && by_var %in% smooth_vars) {
        cli::cli_abort(c("A numeric by multiplier cannot also be an untransformed automatic grid axis.",
          "i" = "Supply prediction `data` to retain the actual by values."))
      }
      if (!back_transform || !by_var %in% names(newlist)) {
        newlist <- append(newlist, newby)
      }
    }
  }
  newdata <- exec(expand_grid, !!!newlist) # compute expand.grid-alike

  if (back_transform) {
    # The continuous by multiplier remains one on the evaluated scale.
    newdata <- evaluate_terms(newdata, smooth_vars, model)
    newdata <- retain_smooth_coordinates(newdata, smooth_vars, model, required = TRUE)
    if (!is.null(by_var) && !is_factor_by_smooth(sm)) newdata[[by_var]] <- 1L
  }

  if (isTRUE(include_all)) {
    ## need to provide single values for all other covariates in data
    unused_vars <- dplyr::setdiff(m.terms, c(used_vars, names(newdata)))
    ## only processed unused_vars if length() > 0L
    if (length(unused_vars) > 0L) {
      unused_summ <- model[["var.summary"]][unused_vars]
      ## FIXME: put this in utils.R with a better name!
      ## this basically just reps the data (scalar) for the closest
      ## observation to the median over all observations
      `rep_fun` <- function(x, n) {
        ## if `x` isn't a factor, select the second element of `x` which
        ## is the value of the observation in the data closest to median
        ## of set of observations in data used to fit the model.
        if (!is.factor(x)) {
          x <- x[2L]
        }
        ## repeat `x` as many times as is needed
        rep(x, times = n)
      }
      n_new <- NROW(newdata)
      unused_data <- as_tibble(lapply(unused_summ,
        FUN = rep_fun,
        n = n_new
      ))
      ## add unnused_data to newdata so we're ready to predict
      newdata <- bind_cols(newdata, unused_data)
    }
  }

  attr(newdata, "gratia.evaluated") <- TRUE
  newdata # return
}

# This is just stupid - what was I even thinking, doesn't work if there are
# multiple boundaries for example, and I don't even think it is necessary...
`soap_film_data` <- function(smooth,
    n = 100,
    n_2d = NULL,
    n_3d = NULL,
    n_4d = NULL,
    offset = NULL,
    include_all = FALSE,
    var_order = NULL) {
  bnd <- boundary(smooth)

  sub_bnd_data <- function(bnd, n = 100) {
    l <- lapply(bnd, \(x) evenly(x, n = n))
    d <- expand.grid(l) |>
      as_tibble()
    d
  }

  bnd_data <- lapply(bnd, FUN = sub_bnd_data, n = n) |>
    bind_rows()

  # are points inside boundary?
  # Will deal with this in eval_smooth.soap.film because mgcv::inSide has a bad
  # design
  bnd_data
}

`sos_data` <- function(
  data,
  n = 100,
  vars
) {
  df <- data.frame(
    X1 = evenly(data[[vars[1]]], n),
    X2 = evenly(data[[vars[2]]], n)
  ) |>
    setNames(vars)

  # Evaluation coordinates are not tile centres. Geographic cell boundaries
  # are constructed separately when drawing, without moving prediction rows.
  df
}
