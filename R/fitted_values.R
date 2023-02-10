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
#' @note For most families, regardless of the scale on which the fitted values
#'   are returned, the `se` component of the returned object is on the *link*
#'   (*linear predictor*) scale, not the response scale. An exception is the
#'   `mgcv::ocat()` family, for which the `se` is on the response scale if
#'   `scale = "response"`.
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
#' Models fitted with certain families will include additional variables
#'
#' * `mgcv::ocat()` models: when `scale = "repsonse"`, the returned object will
#'   contain a `row` column and a `category` column, which indicate to which row
#'   of the `data` each row of the returned object belongs. Additionally, there
#'   will be `nrow(data) * n_categories` rows in the returned object; each row
#'   is the predicted probability for a single category of the response.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{op <- options(cli.unicode = FALSE, pillar.sigfig = 6)}
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

    # handle special distributions that return more than vector fit & std. err.
    # find the name of the function that produces fitted values for this family
    fit_vals_fun <- get_fit_fun(object)
    # compute fitted values
    fit <- fit_vals_fun(object, data = data, ci_level = ci_level,
        scale = scale, ...)
    fit
}

#' @importFrom rlang set_names .data
#' @importFrom dplyr bind_cols mutate across
#' @importFrom tibble as_tibble is_tibble
`fit_vals_default` <- function(object, data, ci_level = 0.95,
    scale = "response", ...) {
    fit <- predict(object, newdata = data, ..., type = "link",
        se.fit = TRUE) |>
        as.data.frame() |>
        rlang::set_names(c("fitted", "se")) |>
        as_tibble()
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

#' @importFrom dplyr bind_rows relocate
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
`fit_vals_ocat` <- function(object, data, ci_level = 0.95, scale = "response",
    ...) {
    # if link (linear predictor) scale, we can just use `fit_vals_fun()`
    if (scale %in% c("link", "linear predictor")) {
        fit <- fit_vals_default(object, data = data, ci_level = ci_level,
            scale = "link", ...)
    } else {
        # predict, needs to be response scale for ocat!
        fv <- predict(object, newdata = data, ..., type = "response",
            se.fit = TRUE)
        crit <- coverage_normal(ci_level)

        # extract information on how many thresholds, categories in the model
        theta <- theta(object) # the estimated thresholds, first is always -1
        n_cut <- length(theta) # how many thresholds...
        n_cat <- n_cut + 1     # ...which implies this many categories
        n_data <- NROW(data)   # how many data are we predicting for

        # \hat{pi} is the estimated probability of each class for each data
        # \hat{pi} is given by fv$fit
        # std. err. of \hat{pi} is given by fv$se.fit

        # compute standard error of logit(\hat{pi}) via delta method
        # this comes from Christensen RHB (2022), a vignette of ordinal
        # package:
        # https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf
        #
        # se(logit(pi)) = se(pi) / (pi * (1 - pi))
        se_lp <- fv$se.fit / (fv$fit * (1 - fv$fit))

        # grab slightly better versions of plogis() & qlogis() from the binomial
        # family
        bin_fam <- binomial()
        lfun <- link(bin_fam)
        ifun <- inv_link(bin_fam)

        # convert \hat{pi} to logit scale and form a Wald interval, then back
        # transform the interval only (we already have \hat{pi})
        fit_lp <- lfun(fv$fit) # logit(\hat{pi})
        fit_lwr <- ifun(fit_lp - (crit * se_lp))
        fit_upr <- ifun(fit_lp + (crit * se_lp))

        # create the return object
        fit <- tibble(row = rep(seq_len(n_data), times = n_cat),
            category = factor(rep(seq_len(n_cat), each = n_data)),
            fitted = as.numeric(fv$fit),
            se = as.numeric(fv$se.fit),
            lower = as.numeric(fit_lwr),
            upper = as.numeric(fit_upr))

        # expand data so it is replicated once per category & add to the fitted
        # values
        fit <- expand_grid(category = seq_len(n_cat), data) |>
            select(-c("category")) |>
            bind_cols(fit) |>
            relocate(row, .before = 1)
    }
    fit
}

#' @importFrom dplyr case_when
`get_fit_fun` <- function(object) {
    fn <- family_type(object)
    fam <- case_when(
        grepl("^ordered_categorical", fn, ignore.case = TRUE) == TRUE ~ "ocat",
        .default = "default"
    )
    get(paste0("fit_vals_", fam), mode = "function")
}

## my original code trying to follow Simon's ocat

    # lp <- as.numeric(fv$fit)
    # se <- as.numeric(fv$se.fit)
    # upr <- lp + (crit * se)
    # lwr <- lp - (crit * se)

    # theta <- theta(object)
    # n_cut <- length(theta)
    # n_cat <- n_cut + 1
    # n_data <- NROW(data)
    # p_fit <- p_lwr <- p_upr <- matrix(0, nrow = n_data,
    #     ncol = n_cut + 2)
    # # cumulative probability should sum to 1 over the latent
    # # fill final column with 1 to reflect that
    # p_fit[, n_cut + 2] <- p_lwr[, n_cut + 2] <- p_upr[, n_cut + 2] <- 1

    # # function to give probability from latent
    # `ocat_prob` <- function(lp, theta) {
    #     p <- theta - lp
    #     i <- p > 0
    #     p[i] <- 1 / (1 + exp(-p[i]))
    #     p[!i] <- exp(p[!i]) / (1 + exp(p[!i]))
    #     p
    # }

    # # fill in the matrix of cumulative probability
    # for (j in seq_along(theta)) {
    #     p_fit[, j + 1] <- ocat_prob(lp, theta[j])
    #     p_lwr[, j + 1] <- ocat_prob(lwr, theta[j])
    #     p_upr[, j + 1] <- ocat_prob(upr, theta[j])
    # }
    # #browser()
    # p_fit <- as.numeric(t(diff(t(p_fit))))
    # p_lwr <- as.numeric(t(diff(t(p_lwr))))
    # p_upr <- as.numeric(t(diff(t(p_upr))))

    # fit <- tibble(row = rep(seq_len(n_data), times = n_cat),
    #     category = factor(rep(seq_len(n_cat), each = n_data)),
    #     fitted = p_fit,
    #     lower = p_lwr,
    #     upper = p_upr)
    # # expand data so it is replicated once per category
    # fit <- expand_grid(category = seq_len(n_cat), data) |>
    #     select(-c("category")) |>
    #     bind_cols(fit) |>
    #     relocate(row, .before = 1)
    # fit