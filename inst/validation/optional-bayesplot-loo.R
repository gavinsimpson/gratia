# Run from the package root with:
# Rscript inst/validation/optional-bayesplot-loo.R
#
# Every scenario runs in a clean R process using a temporary library containing
# links to installed packages, with the unavailable optional packages omitted.
# This checks actual namespace loading rather than mocking requireNamespace().
# The installed bayesplot version only Suggests loo, so either optional package
# can be tested alone. Package installations are never changed.

validate_optional_bayesplot_loo <- function(path = ".") {
  path <- normalizePath(path, mustWork = TRUE)
  installed <- utils::installed.packages(noCache = TRUE)
  missing <- setdiff(c("pkgload", "bayesplot", "loo"), installed[, "Package"])
  if (length(missing)) stop("Install the validation packages first: ",
    paste(missing, collapse = ", "), call. = FALSE)
  description <- read.dcf(file.path(path, "DESCRIPTION"))
  imported <- trimws(gsub("\\s*\\([^)]*\\)", "",
    unlist(strsplit(description[1L, "Imports"], ","))))
  suggested <- trimws(gsub("\\s*\\([^)]*\\)", "",
    unlist(strsplit(description[1L, "Suggests"], ","))))
  stopifnot(!any(c("bayesplot", "loo") %in% imported),
    all(c("bayesplot", "loo") %in% suggested))

  temporary <- tempfile("gratia-optional-packages-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  child_file <- file.path(temporary, "check.R")
  child <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    path <- args[1L]
    library <- args[2L]
    available <- if (args[3L] == "none") character() else
      strsplit(args[3L], ",", fixed = TRUE)[[1L]]
    before <- identical(args[4L], "before")
    order <- if (args[5L] == "none") character() else
      strsplit(args[5L], ",", fixed = TRUE)[[1L]]

    # R appends its main library even when .libPaths(new) is called. Set the
    # private search vector directly in this disposable child so packages
    # omitted from the temporary library really cannot be found.
    assign(".lib.loc", library, envir = environment(.libPaths))
    stopifnot(identical(.libPaths(), library),
      !any(c("bayesplot", "loo") %in% loadedNamespaces()))
    for (package in setdiff(c("bayesplot", "loo"), available)) {
      stopifnot(!length(find.package(package, quiet = TRUE)),
        !requireNamespace(package, quietly = TRUE))
    }
    if (before) invisible(lapply(order, loadNamespace))
    loaded_before <- intersect(c("bayesplot", "loo"), loadedNamespaces())
    pkgload::load_all(path, helpers = FALSE, export_all = FALSE, quiet = TRUE)
    stopifnot(setequal(intersect(c("bayesplot", "loo"), loadedNamespaces()),
      loaded_before))
    if (!before) invisible(lapply(order, loadNamespace))
    stopifnot(setequal(intersect(c("bayesplot", "loo"), loadedNamespaces()),
      available))
    stopifnot(!any(c("pp_check", "loo", "waic") %in%
      getNamespaceExports("gratia")))

    d <- data.frame(x = seq(-1, 1, length.out = 24))
    d$y <- sin(d$x) + 0.3 * cos(seq_len(nrow(d)))
    s <- mgcv::s
    fit <- mgcv::gam(y ~ s(x, k = 5), data = d, method = "REML")
    ll <- gratia::log_lik(fit, n = 20, seed = 278)
    stopifnot(identical(dim(ll), c(20L, 24L)), all(is.finite(ll)))
    # The pre-existing sampler also remains usable without either package.
    samples <- gratia::posterior_samples(fit, n = 2, seed = 278)
    stopifnot(nrow(samples) == 48L)
    message_contains <- function(expr, pattern) {
      message <- tryCatch({ force(expr); NA_character_ },
        error = conditionMessage)
      stopifnot(!is.na(message), grepl(pattern, message, fixed = TRUE))
    }
    if ("loo" %in% available) {
      result <- suppressWarnings(loo::loo(fit, n = 100, seed = 278, cores = 1))
      stopifnot(inherits(result, "loo"), nrow(result$pointwise) == 24L)
      result <- suppressWarnings(loo::waic(fit, n = 100, seed = 278))
      stopifnot(inherits(result, "waic"), nrow(result$pointwise) == 24L)
    } else {
      message_contains(getFromNamespace("loo.gam", "gratia")(fit),
        "Package 'loo' is required")
      message_contains(getFromNamespace("waic.gam", "gratia")(fit),
        "Package 'loo' is required")
    }
    if ("bayesplot" %in% available) {
      plot <- suppressMessages(bayesplot::pp_check(fit, n = 10, seed = 278))
      stopifnot(inherits(plot, "ggplot"))
      if ("loo" %in% available) {
        plot <- suppressWarnings(suppressMessages(bayesplot::pp_check(fit,
          type = "loo_pit_overlay", n = 200, seed = 278)))
        stopifnot(inherits(plot, "ggplot"))
      } else {
        message_contains(bayesplot::pp_check(fit, type = "loo_pit_overlay"),
          "Package 'loo' is required")
      }
    } else {
      message_contains(getFromNamespace("pp_check.gam", "gratia")(fit),
        "Package 'bayesplot' is required")
    }
    cat("PASS\n")
  }
  writeLines(c("check <-", deparse(child), "check()"), child_file)

  scenarios <- list(
    list(available = character(), before = FALSE, order = character()),
    list(available = "bayesplot", before = TRUE, order = "bayesplot"),
    list(available = "bayesplot", before = FALSE, order = "bayesplot"),
    list(available = "loo", before = TRUE, order = "loo"),
    list(available = "loo", before = FALSE, order = "loo"),
    list(available = c("bayesplot", "loo"), before = TRUE,
      order = c("bayesplot", "loo")),
    list(available = c("bayesplot", "loo"), before = TRUE,
      order = c("loo", "bayesplot")),
    list(available = c("bayesplot", "loo"), before = FALSE,
      order = c("bayesplot", "loo")),
    list(available = c("bayesplot", "loo"), before = FALSE,
      order = c("loo", "bayesplot")))
  libraries <- list()
  collapse <- function(x) if (length(x)) paste(x, collapse = ",") else "none"
  results <- lapply(scenarios, function(scenario) {
    key <- collapse(scenario$available)
    library <- libraries[[key]]
    if (is.null(library)) {
      library <- file.path(temporary, paste0("library-", key))
      dir.create(library)
      packages <- installed[!duplicated(installed[, "Package"]), , drop = FALSE]
      omitted <- c("gratia", setdiff(c("bayesplot", "loo"), scenario$available))
      packages <- packages[!packages[, "Package"] %in% omitted, , drop = FALSE]
      linked <- file.symlink(file.path(packages[, "LibPath"], packages[, "Package"]),
        file.path(library, packages[, "Package"]))
      stopifnot(all(linked))
      libraries[[key]] <<- library
    }
    label <- paste(key, if (scenario$before) "before" else "after",
      collapse(scenario$order))
    message("Checking ", label)
    output <- system2(file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", shQuote(child_file), shQuote(path), shQuote(library),
        shQuote(key), if (scenario$before) "before" else "after",
        shQuote(collapse(scenario$order))), stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if ((!is.null(status) && status != 0L) || !"PASS" %in% output) {
      stop("Scenario failed: ", label, "\n", paste(output, collapse = "\n"),
        call. = FALSE)
    }
    data.frame(available = key, gratia_loaded = if (scenario$before) "last" else "first",
      optional_order = collapse(scenario$order), result = "PASS")
  })
  do.call(rbind, results)
}

print(validate_optional_bayesplot_loo())
