#' Provides an overview of a model and the terms in that model
#'
#' @param model a fitted model object to overview.
#' @param ... arguments passed to other methods.
#' 
#' @export
`overview` <- function(model, ...) {
    UseMethod("overview")
}

#' @param parametric logical; include the model parametric terms in the
#'   overview?
#' @param random_effects tests of fully penalized smooth terms (those with a
#'   zero-dimensaionl null space, e.g. random effects) are computationally
#'   expensive and for large data sets producing these p values can take a
#'   very long time. If `random_effects = FALSE`, the tests of the expensive
#'   terms will be skipped.
#' @param dispersion numeric; a known value for the dispersion parameter. The
#'   default `NULL` implies that the estimated value or the default value (1
#'   for the Poisson distribution for example) where this is specified is used
#'   instead.
#' @param frequentist logical; by default the Bayesian estimated covariance
#'   matrix of the parameter estimates is used to calculate p values for
#'   parametric terms. If `frequentist = FALSE`, the frequentist covariance
#'   matrix of the parameter estimates is used.
#' @param accuracy numeric; accuracy with which to report p values, with p
#'   values below this value displayed as `"< accuracy"`.
#'
#' @export
#' @rdname overview
#' @importFrom dplyr %>% select
#' @importFrom tibble rownames_to_column as_tibble add_column
#' @importFrom tidyselect matches
#' @importFrom rlang set_names .data
#' 
#' @examples
#' 
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim(n = 400, seed = 2)
#' m <- gam(y ~ x3 + s(x0) + s(x1, bs = "bs") + s(x2, bs = "ts"),
#'          data = df, method = "REML")
#' overview(m)
#' \dontshow{
#' options(op)
#' }
`overview.gam` <- function(model, parametric = TRUE, random_effects = TRUE,
                           dispersion = NULL, frequentist = FALSE,
                           accuracy = 0.001,
                           ...) {
    smry <- summary(model, dispersion = dispersion, re.test = random_effects,
                    freq = frequentist)
    nms <- c("term", "type", "edf", "statistic", "p.value")

    # smooth terms
    types <- vapply(model$smooth, smooth_type, character(1))
    out <- as.data.frame(smry$s.table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      select(!matches("Ref.df")) %>%
      add_column(type = types, .after = 1L)

    # parametric terms
    para <- NULL
    if (isTRUE(parametric) && ! is.null(smry$pTerms.table)) {
        para <- as.data.frame(smry$pTerms.table) %>%
          rownames_to_column() %>%
          as_tibble() %>%
          rename(edf = "df") %>%
          add_column(type = rep("parametric", nrow(smry$pTerms.table)),
                     .after = 1L)
        out <- bind_rows(para, out)
    }

    out <- set_names(out, nms)

    out <- mutate(out, p.value = format.pval(.data$p.value, eps = accuracy))

    class(out) <- append(class(out), values = "overview", after = 0)
    out
}

#' @export
`overview.gamm` <- function(model, ...) {
    out <- overview(model$gam)
    class(out) <- append(class(out), values = "overview_gamm", after = 0)
    out
}

#' @export
`overview.bam` <- function(model, ...) {
    out <- NextMethod()
    class(out) <- append(class(out), values = "overview_bam", after = 0)
    out
}

#' @export
#' @importFrom cli symbol pluralize
`tbl_sum.overview` <- function(x, ...) {
  c("Generalized Additive Model" = pluralize("with {nrow(x)} term{?s}"))
}

#' @export
#' @importFrom cli symbol pluralize
`tbl_sum.overview_gamm` <- function(x, ...) {
  c("Generalized Additive Mixed Model" = pluralize("with {nrow(x)} term{?s}"))
}

#' @export
#' @importFrom cli symbol pluralize
`tbl_sum.overview_bam` <- function(x, ...) {
  c("Big Additive Model" = pluralize("with {nrow(x)} term{?s}"))
}

#' @export
#' @importFrom cli style_dim
tbl_format_header.overview <- function(x, setup, ...) {
    style_dim("\n", names(setup$tbl_sum), " ", setup$tbl_sum, "\n")
}
