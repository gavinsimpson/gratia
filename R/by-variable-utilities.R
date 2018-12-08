##' Tests for by variable smooths
##'
##' Functions to check if a smooth is a by-variable one and to test of the type
##' of by-variable smooth is a factor-smooth or a continous-smooth interaction.
##'
##' @param smooth an object of class `"mgcv.smooth"`
##'
##' @return A logical vector.
##'
##' @author Gavin L. Simpson
##'
##' @export
##' @rdname is_by_smooth
`is_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    is_factor_by_smooth(smooth) | is_continuous_by_smooth(smooth)
}

##' @export
##' @rdname is_by_smooth
`is_factor_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    by.level <- smooth[["by.level"]]
    !is.null(by.level)
}

##' @export
##' @rdname is_by_smooth
`is_continuous_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    by.level <- by_level(smooth)
    by.var <- by_variable(smooth)
    !(is.null(by.level) & by.var == "NA")
}

##' @export
##' @rdname is_by_smooth
`by_variable` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    as.character(smooth[["by"]])
}

##' @export
##' @rdname is_by_smooth
`by_level` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    smooth[["by.level"]]
}


##' @importFrom tibble add_column
`add_missing_by_info_to_smooth` <- function(smooth) {
    smooth <- add_column(smooth,
                         by_variable = factor(rep(NA_character_, nrow(smooth))),
                         .after = 1L)
}

##' @importFrom tibble add_column
`add_by_var_info_to_smooth` <- function(smooth, by_name, by_data, n) {
    nc <- NCOL(smooth)
    nr <- NROW(smooth)
    if (length(by_name) > 1L) {
        if (by_name[[1L]] == "NA") {
            f <- factor(c(rep(NA_character_, n), rep(by_name[[2L]], NROW(smooth) - n)))
            d <- factor(rep(c(NA, levels(by_data)), each = n))
        } else {
            f <- factor(c(rep(by_name[[2L]], NROW(smooth) - n), rep(NA_character_, n)))
            d <- factor(rep(c(levels(by_data), NA), each = n))
        }
        smooth <- add_column(smooth, by_variable = f, .after = 1L)
        smooth <- add_column(smooth, by_var = d)
    names(smooth)[NCOL(smooth)] <- by_name[by_name != "NA"]
    } else {
        smooth <- add_column(smooth,
                             by_variable = factor(rep(by_name, NROW(smooth))),
                             .after = 1L)
        smooth <- add_column(smooth,
                             by_var = factor(rep(levels(by_data), each = n)))
        names(smooth)[NCOL(smooth)] <- by_name
    }
    smooth
}
