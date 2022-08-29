#' Return the linear prediction matrix of a fitted GAM
#'
#' `lp_matrix()` is a wrapper to `predict(..., type = "lpmatrix")` for returning
#' the linear predictor matrix for the model training data (when `data = NULL`),
#' or user-specified data values supplied via `data`.
#'
#' @param model a fitted model
#' @param data a data frame of values at which to return the linear prediction
#'   matrix.
#' @param ... arguments passed to other methods and `predict` methods including
#'   [mgcv::predict.gam()] and [mgcv::predict.bam()]
#'
#' @details
#'
#' The linear prediction matrix \eqn{\mathbf{X}_p} is a matrix that maps values
#' of parameters \eqn{\hat{\mathbf{\beta}}_p} to values on the linear
#' predictor of the model \eqn{\hat{\eta}_p = \mathbf{X}_p
#' \hat{\mathbf{\beta}}_p}. \eqn{\mathbf{X}_p} is the model matrix where spline
#' covariates have been replaced by the values of the basis functions evaluated
#' at the respective covariates. Parametric covariates are also included.
#'
#' @return The linear prediction matrix is returned as a matrix. The object
#' returned is of class `"lp_matrix"`, which inherits from classes `"matrix"`
#' and `"array"`. The special class allows the printing of the matrix to be
#' controlled, which we do by printing the matrix as a tibble.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{op <- options(digits = 3, cli.unicode = FALSE)}
#' df <- data_sim("eg1", seed = 1)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)
#'
#' # linear prediction matrix for observed data
#' xp <- lp_matrix(m)
#' xp
#'
#' # the object `xp` *is* a matrix
#' class(xp)
#' # but we print like a tibble to avoid spamming the R console
#'
#' # linear predictor matrix for new data set
#' ds <- data_slice(m, x2 = evenly(x2))
#' xp <- lp_matrix(m, data = ds)
#' xp
#' \dontshow{options(op)}
`lp_matrix` <- function(model, ...) {
    UseMethod("lp_matrix")
}

#' @export
#'
#' @rdname lp_matrix
#' @importFrom stats predict
`lp_matrix.gam` <- function(model, data = NULL, ...) {
    xp <- if (is.null(data)) {
        predict(model, type = "lpmatrix", ..., se.fit = FALSE)
    } else {
        predict(model, newdata = data, type = "lpmatrix", ..., se.fit = FALSE)
    }
    class(xp) <- append(class(xp), "lp_matrix", after = 0)
    xp
}

#' @export
#' @importFrom pillar tbl_sum tbl_format_header
`print.lp_matrix` <- function(x, ..., n = 5, max_footer_lines = 1) {
    class(x) <- class(x)[-1]
    tbl <- as_tibble(x)
    class(tbl) <- append(class(tbl), "tbl_lp_matrix", after = 0)
    print(tbl, ..., n = n, max_footer_lines = max_footer_lines)
}

#' @export
#' @importFrom cli symbol
`tbl_sum.tbl_lp_matrix` <- function(x, ...) {
  c("Linear prediction matrix" = paste(nrow(x), cli::symbol$times, ncol(x)))
}

#' @export
#' @importFrom cli style_dim
`tbl_format_header.tbl_lp_matrix` <- function(x, setup, ...) {
    style_dim(names(setup$tbl_sum), " (", setup$tbl_sum, ")")
}
