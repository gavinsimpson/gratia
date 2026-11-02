# Preserve the last spatial operation and its inputs if compiled code stops R.
# Run in a separate R process so tracing cannot affect the package check.
# Replay an uploaded input in a fresh R session with the same sf installation:
# input <- readRDS("spherical-test-progress.log.rds")
# do.call(get(input$function_name, asNamespace("sf")), input$arguments)
run_spherical_tests <- function() {
  stopifnot(requireNamespace("sf", quietly = TRUE))
  print(sessionInfo())
  print(sf::sf_extSoftVersion())
  for (package in c("sf", "s2", "wk", "ggplot2", "mgcv")) {
    cat(package, as.character(packageVersion(package)), "\n")
  }

  progress <- Sys.getenv("GRATIA_TEST_PROGRESS")
  if (!nzchar(progress)) stop("Set GRATIA_TEST_PROGRESS to a log file path.")
  native_input <- paste0(progress, ".rds")
  ns <- asNamespace("sf")
  functions <- intersect(c("CPL_transform", "CPL_transform_bounds",
    "CPL_gdal_segmentize", "CPL_geos_op", "CPL_geos_op2", "CPL_geos_binop",
    "CPL_geos_normalize", "CPL_geos_is_valid", "CPL_geos_is_empty",
    "CPL_get_bbox"), ls(ns))

  checkpoint <- function(name, state, frame) {
    if (state == "START") {
      arguments <- mget(names(formals(get(name, ns))), frame,
        inherits = FALSE)
      saveRDS(list(package = as.character(packageVersion("sf")),
        function_name = name, arguments = arguments), native_input,
        compress = FALSE)
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      state, name, "\n", file = progress, append = TRUE)
  }
  assign(".gratia_sf_checkpoint", checkpoint, envir = .GlobalEnv)
  traced <- character()
  on.exit({
    for (name in traced) suppressMessages(untrace(name, where = ns))
    rm(".gratia_sf_checkpoint", envir = .GlobalEnv)
  }, add = TRUE)
  for (name in functions) {
    suppressMessages(trace(name, where = ns, print = FALSE,
      tracer = bquote(get(".gratia_sf_checkpoint", envir = .GlobalEnv)(
        .(name), "START", environment())),
      exit = bquote(get(".gratia_sf_checkpoint", envir = .GlobalEnv)(
        .(name), "DONE", environment()))))
    traced <- c(traced, name)
  }

  source("tests/testthat/helper-builder-diagnostics.R")
  reporter <- testthat::MultiReporter$new(list(
    testthat::SummaryReporter$new(), builder_progress_reporter(progress)))
  testthat::test_local(filter = "^compare-smooths$", reporter = reporter,
    stop_on_failure = TRUE, stop_on_warning = TRUE)
}

run_spherical_tests()
