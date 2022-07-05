#' Provides an overview of a model and the terms in that model
#'
#' @param model a fitted model object to overview.
#' @param ... arguments passed to other methods.
#' 
#' @export
`overview` <- function(model, ...) {
    UseMethod("overview")
}

#' @export
#' @rdname overview
#' @importFrom dplyr %>% select
#' @importFrom tibble rownames_to_column as_tibble add_column
#' @importFrom tidyselect matches
#' @importFrom rlang set_names .data
`overview.gam` <- function(model, parametric = TRUE, random_effects = TRUE,
                           dispersion = NULL, freq = FALSE, accuracy = 0.001,
                           ...) {
    smry <- summary(model, dispersion = dispersion, re.test = random_effects,
                    freq = freq)
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
    if (isTRUE(parametric)) {
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
