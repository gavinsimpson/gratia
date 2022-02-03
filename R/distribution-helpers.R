## helpers for working with mgcv's families

#' Negative binomial parameter theta
#'
#' @param model a fitted model.
#'
#' @return A numeric vector of length 1 containing the estimated value of
#'   theta.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", n = 500, dist = "poisson", scale = 0.1, seed = 6)
#'
#' m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
#'          s(x3, bs = "cr"), family = nb, data = df, method = "REML")
#' nb_theta(m)
`nb_theta` <- function(model) {
    UseMethod("nb_theta")
}

#' @export
#' @importFrom stringr str_detect
#' @describeIn nb_theta Method for class `"gam"`
`nb_theta.gam` <- function(model) {
    supported <- str_detect(family_name(model),
                            c("Negative Binomial", "negative binomial"))
    if (!any(supported)) {
        stop()
    }

    ## how mgcv stores theta depends on which family was used, and this also
    ## affects the actual function stored in the family. Need theta on natural
    ## scale so we transform this if family is nb()
    fam <- family(model)
    theta <- if (inherits(fam, "extended.family")) {
        fam$getTheta(trans = TRUE)
    } else {
        fam$getTheta()
    }

    ## return
    theta
}