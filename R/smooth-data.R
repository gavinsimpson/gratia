#' Generate regular data over the covariates of a smooth
#'
#' @param model a fitted model
#' @param id the number ID of the smooth within `model` to process.
#' @param n numeric; the number of new observations to generate.
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth.
#' @param n_4d numeric; the number of new observations to generate for the
#'   dimensions higher than 2 (!) of a *k*D smooth (*k* >= 4). For example, if
#'   the smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d`
#'   new observations.
#' @param offset numeric; value of the model offset to use.
#' @param include_all logical; include all covariates involved in the smooth?
#'   if `FALSE`, only the covariates involved in the smooth will be included in
#'   the returned data frame. If `TRUE`, a representative value will be included
#'   for all other covariates in the model that aren't actually used in the
#'   model. This can be useful if you want to pass the returned data frame on to
#'   [mgcv::PredictMat()].
#' 
#' @export
#' 
#' @importFrom dplyr bind_cols setdiff
#' @importFrom tibble as_tibble
#' @importFrom rlang exec !!!
#' @importFrom tidyr expand_grid
`smooth_data` <- function(model, id, n = 100, n_3d = NULL, n_4d = NULL,
                          offset = NULL, include_all = FALSE) {
    mf <- model.frame(model)           # model.frame used to fit model

    ## remove response
    respvar <- attr(model$terms, "response")
    if (!identical(respvar, 0)) {
        mf <- mf[, -respvar, drop = FALSE]
    }

    # remove offset() var; model.frame returns both `offset(foo(var))` and
    # `var`, so we can just remove the former, but we also want to set the
    # offset variable `var` to something constant. FIXME
    if (is.null(offset)) {
        offset <- 1L
    }
    mf <- fix_offset(model, mf, offset_val = offset)
    ff <- vapply(mf, is.factor, logical(1L)) # which, if any, are factors vars
    ## list of model terms (variable names); extract these from `var.summary`
    ## because model.frame() on a gamm() contains extraneous variables, related
    ## to the mixed model form for lme()
    m.terms <- names(model[["var.summary"]])

    ## need a list of terms used in current smooth
    sm <- get_smooths_by_id(model, id)[[1L]]
    smooth_vars <- unique(smooth_variable(sm))
    ## is smooth a by? If it is, extract the by variable
    by_var <- if (is_by_smooth(sm)) {
        by_variable(sm)
    } else {
        NULL
    }
    used_vars <- c(smooth_vars, by_var)

    # Figure out the n to use for each dimension
    sm_dim <- smooth_dim(sm)
    # fix up the n, n_3d, n_4d. If `n_3d` is `NULL` set `n_3d <- n`
    if (is.null(n_3d)) {
        n_3d <- n
    }
    # likewise fix up n_4d; set it to `n` if `n_4d` is NULL
    if (is.null(n_4d)) {
        n_4d <- n
    }
    seq_per_dim <- function(data, vars, dim, n, n_3d, n_4d) {
        n_per_dim <- rep(n, dim)
        if (dim == 3L) {
            n_per_dim[3] <- n_3d
        } else if (dim > 3L) {
            n_per_dim[seq_len(dim - 2) + 2] <- n_4d
        }
        seq_min_max_wrapper <- function(i, data, vars, n) {
            out <- seq_min_max(data[[vars[i]]], n = n[i])
            if (i > 2L) {
                out <- round(out, 3)
            }
            out
        }
        out <- lapply(seq_along(vars),
                      FUN = seq_min_max_wrapper,
                      data = data, vars = vars, n = n_per_dim)
        out <- setNames(out, vars)
        out
    }

    ## generate covariate values for the smooth
    #newlist <- lapply(mf[smooth_vars], seq_min_max, n = n)
    newlist <- seq_per_dim(data = mf, vars = smooth_vars, dim = sm_dim,
                           n = n, n_3d = n_3d, n_4d = n_4d)
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
            newby <- setNames(list(median(mf[[by_var]]), na.rm = TRUE), by_var)
            newlist <- append(newlist, newby)
        }
    }
    newdata <- exec(expand_grid, !!!newlist) # compute expand.grid-alike

    if (isTRUE(include_all)) {
        ## need to provide single values for all other covariates in data
        unused_vars <- dplyr::setdiff(m.terms, used_vars)
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
            unused_data <- as_tibble(lapply(unused_summ, FUN = rep_fun,
                                            n = n_new))
            ## add unnused_data to newdata so we're ready to predict
            newdata <- bind_cols(newdata, unused_data)
        }
    }

    newdata # return
}
