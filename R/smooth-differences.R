##' Differences of factor smooth interactions
##'
##' @param model A fitted model.
##' @param smooth character; which smooth to compute differences for.
##' @param n numeric; the number of points at which to evaluate the difference
##'   between pairs of smooths.
##' @param ci_level numeric between 0 and 1; the coverage of credible interval.
##' @param newdata data frame of locations at which to evaluate the difference
##'   between smooths.
##' @param unconditional logical; account for smoothness selection in the model?
##' @param frequentist logical; use the frequentist covariance matrix?
##' @param ... arguments passed to other methods.
##' 
##' @export
##' @examples
##'
##' load_mgcv()
##' \dontshow{
##' set.seed(42)
##' op <- options(digits = 3, cli.unicode = FALSE)
##' }
##' df <- data_sim("eg4")
##' m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
##'
##' difference_smooths(m, smooth = "s(x2)")
##' \dontshow{options(op)}
`difference_smooths` <- function(model, ...) {
    UseMethod("difference_smooths")
}

##' @export
##'
##' @importFrom purrr pmap
##' @importFrom dplyr bind_rows
##' @importFrom tibble add_column
##' @importFrom stats qnorm coef
##' @importFrom utils combn
##'
##' @rdname difference_smooths
`difference_smooths.gam` <- function(model,
                                     smooth,
                                     n = 100,
                                     ci_level = 0.95,
                                     newdata = NULL,
                                     unconditional = FALSE,
                                     frequentist = FALSE,
                                     ...) {
    if (missing(smooth)) {
        stop("Must specify a smooth to difference via 'smooth'.")
    }

    sm_ids <- which_smooths(model, smooth)
    smooths <- get_smooths_by_id(model, sm_ids)
    sm_data <- map(sm_ids, smooth_data,
                   model = model, n = n, include_all = TRUE)
    sm_data <- bind_rows(sm_data)
    by_var <- by_variable(smooths[[1L]])
    smooth_var <- smooth_variable(smooths[[1L]])
    pairs <- as_tibble(as.data.frame(t(combn(levels(sm_data[[by_var]]), 2)),
                                     stringsAsFactor = FALSE))
    names(pairs) <- paste0("f", 1:2)

    Xp <- predict(model, newdata = sm_data, type = "lpmatrix")
    V <- get_vcov(model, unconditional = unconditional,
                  frequentist = frequentist)
    coefs <- coef(model)

    out <- pmap(pairs, calc_difference, smooth = smooth, by_var = by_var,
                smooth_var = smooth_var, data = sm_data, Xp = Xp, V = V,
                coefs = coefs)
    out <- bind_rows(out)
    crit <- qnorm((1 - ci_level) / 2, lower.tail = FALSE)
    out <- add_column(out,
                      lower = out$diff - (crit * out$se),
                      upper = out$diff + (crit * out$se),
                      .after = 6L)
    out
}

##' @export
`difference_smooths.bam` <- function(model, ...) {
    NextMethod()
}

##' @export
`difference_smooths.gamm` <- function(model, ...) {
    difference_smooths(model[["gam"]], ...)
}

##' @export
`difference_smooths.list` <- function(model, ...) {
    if (! is_gamm4(model)) {
        stop()
    }
    difference_smooths(model$gam, ...)
}

##' @importFrom tibble new_tibble
##' @importFrom dplyr bind_cols
`calc_difference` <- function(f1, f2, smooth, by_var, smooth_var, data, Xp, V, coefs) {
    ## make sure f1 and f2 are characters
    f1 <-  as.character(f1)
    f2 <-  as.character(f2)
    cnames <- colnames(Xp)
    ## columns of Xp associated with pair of smooths
    c1 <- grepl(mgcv_by_smooth_labels(smooth, by_var, f1), cnames, fixed = TRUE)
    c2 <- grepl(mgcv_by_smooth_labels(smooth, by_var, f2), cnames, fixed = TRUE)
    ## rows of Xp associated with pair of smooths
    r1 <- data[[by_var]] == f1
    r2 <- data[[by_var]] == f2

    ## difference rows of Xp for pair of smooths
    X <- Xp[r1, ] - Xp[r2, ]

    ## zero the cols related to other splines
    X[, ! (c1 | c2)] <- 0

    ## zero out the parametric cols
    X[, !grepl('^s\\(', cnames)] <- 0

    ## compute difference
    sm_diff <- drop(X %*% coefs)
    se <- sqrt(rowSums((X %*% V) * X))
    nr <- NROW(X)
    out <- list(smooth = rep(smooth, nr), by = rep(by_var, nr),
                level_1 = rep(f1, nr),
                level_2 = rep(f2, nr),
                diff = sm_diff, se = se)
    out <- new_tibble(out, nrow = NROW(X), class = "difference_smooth")
    ## Only need rows associated with one of the levels
    out <- bind_cols(out, data[r1, smooth_var])
    
    out
}
