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
#'   zero-dimensional null space, e.g. random effects) are computationally
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
#' @param stars logical; should significance stars be added to the output?
#'
#' @export
#' @rdname overview
#' @importFrom dplyr %>% select
#' @importFrom tibble rownames_to_column as_tibble add_column
#' @importFrom tidyselect matches
#' @importFrom rlang set_names .data
#' @importFrom stats symnum
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
                           stars = FALSE,
                           ...) {
    smry <- summary(model, dispersion = dispersion, re.test = random_effects,
                    freq = frequentist)
    nms <- c("term", "type", "k", "edf", "statistic", "p.value")

    # smooth terms
    types <- vapply(model$smooth, smooth_type, character(1))
    dfs <- vapply(model$smooth, basis_dim, double(1))
    out <- as.data.frame(smry$s.table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      select(!matches("Ref.df")) %>%
      add_column(type = types, k = dfs, .after = 1L)

    # parametric terms
    para <- NULL
    if (isTRUE(parametric) && ! is.null(smry$pTerms.table)) {
        nr <- nrow(smry$pTerms.table)
        para <- as.data.frame(smry$pTerms.table) %>%
          rownames_to_column() %>%
          as_tibble() %>%
          rename(edf = "df") %>%
          add_column(type = rep("parametric", nr), k = rep(NA_real_, nr),
            .after = 1L)
        out <- bind_rows(para, out)
    }
    out <- set_names(out, nms)

    if (stars) {
      sstars <- symnum(out$p.value, corr = FALSE, na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", " "))
      out <- mutate(out,
        #p = .data$p.value,
        p.value = format.pval(.data$p.value, eps = accuracy),
        stars = sstars) # not sure why as.character(sstars) is wrong here "***"
      attr(out, "legend") <- attr(sstars, "legend")
    } else {
      out <- mutate(out, p.value = format.pval(.data$p.value, eps = accuracy))
    }

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

#' @importFrom cli style_dim
#' @exportS3Method tbl_format_header overview
tbl_format_header.overview <- function(x, setup, ...) {
    style_dim("\n", names(setup$tbl_sum), " ", setup$tbl_sum, "\n")
}

#' @importFrom pillar style_subtle tbl_format_footer
#' @exportS3Method tbl_format_footer overview
`tbl_format_footer.overview` <- function(x, setup, ...) {
    default_footer <- NextMethod()
    star_leg <- attr(x, "legend")
    out <- if (!is.null(star_leg)) {
      leg_footer <- style_subtle(paste0("\n# ", star_leg))
      c(default_footer, leg_footer)
    } else {
      default_footer
    }
    out
}