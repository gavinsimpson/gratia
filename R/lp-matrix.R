#' Return the linear prediction matrix
#'
#' @param model a fitted model
#' @param ... arguments passed to other methods and `predict` methods including
#'   [mgcv::predict.gam] and [mgcv::predict.bam]
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

#' @param data a data frame of values at which to return the linear prediction
#'   matrix.
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
tbl_format_header.tbl_lp_matrix <- function(x, setup, ...) {
    style_dim(names(setup$tbl_sum), " (", setup$tbl_sum, ")")
}
