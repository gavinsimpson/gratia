#' Generate fitted values from a estimated GAM
#'
#' @param object a fitted model. Currently only models fitted by [mgcv::gam()]
#'   and [mgcv::bam()] are supported.
#' @param data optional data frame of covariate values for which fitted values
#'   are to be returned.
#' @param scale character; what scale should the fitted values be returned on?
#'   `"linear predictor"` is a synonym for `"link"` if you prefer that
#'   terminology.
#' @param ci_level numeric; a value between 0 and 1 indicating the coverage of
#'   the credible interval.
#' @param ... arguments passed to [mgcv::predict.gam()]. Note that `type`,
#'   `newdata`, and `se.fit` are already used and passed on to
#'   [mgcv::predict.gam()].
#'
#' @note Regardless of the scale on which the fitted values are returned, the
#'   `se` component of the returned object is on the *link* (*linear predictor*)
#'   scale, not the response scale.
#'
#' @return A tibble (data frame) whose first *m* columns contain either the data
#'   used to fit the model (if `data` was `NULL`), or the variables supplied to
#'  `data`. Four further columns are added:
#'
#' * `fitted`: the fitted values on the specified scale,
#' * `se`: the standard error of the fitted values (always on the *link* scale),
#' * `lower`, `upper`: the limits of the credible interval on the fitted values,
#'     on the specified scale.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, digits = 6)
#' }
#' sim_df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = sim_df, method = "REML")
#' fv <- fitted_values(m)
#' fv
#' \dontshow{options(op)}
`fitted_values` <- function(object, ...) {
    UseMethod("fitted_values")
}

#' @export
#' @rdname fitted_values
#' @importFrom rlang set_names .data
#' @importFrom dplyr bind_cols mutate across
#' @importFrom tibble as_tibble is_tibble
`fitted_values.gam` <- function(object,
                                data = NULL,
                                scale = c("response",
                                          "link",
                                          "linear predictor"),
                                ci_level = 0.95, ...) {
    # Handle everything up to and including the extended families, but not more
    if (inherits(family(object), "general.family")) {
        stop("General likelihood GAMs not yet supported.")
    }
    scale <- match.arg(scale)

    if (is.null(data)) {
        data <- delete_response(object, model_frame = FALSE) %>%
                as_tibble()
    } else if (!is_tibble(data)) {
        data <- as_tibble(data)
    }
    fit <- predict(object, newdata = data, ..., type = "link",
                   se.fit = TRUE) %>%
           bind_cols() %>%
           rlang::set_names(c("fitted", "se"))
    fit <- bind_cols(data, fit)

    # create the confidence interval
    crit <- coverage_normal(ci_level)
    fit <- mutate(fit,
                  lower = .data[["fitted"]] - (crit * .data[["se"]]),
                  upper = .data[["fitted"]] + (crit * .data[["se"]]))

    # convert to the response scale if requested
    if (identical(scale, "response")) {
        ilink <- inv_link(object)
        fit <- mutate(fit, across(all_of(c("fitted", "lower", "upper")), ilink))
    }

    fit
}
