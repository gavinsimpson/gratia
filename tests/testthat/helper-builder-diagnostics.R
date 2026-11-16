# Checkpoints survive an abrupt exit before testthat prints its summary.
# A separate file keeps progress out of captured messages and snapshots.
builder_diagnostics_enabled <- function() {
  identical(Sys.getenv("GRATIA_TEST_DIAGNOSTICS"), "true")
}

# cli formatting changes the spacing of later messages, even with a file.
# Write directly so progress reporting does not change message snapshots.
builder_progress_reporter <- function(file) {
  R6::R6Class(inherit = testthat::LocationReporter, public = list(
    cat_line = function(...) {
      cat(..., "\n", sep = "", file = self$out, append = TRUE)
    },
    add_result = function(context, test, result) {
      location <- result$srcref
      if (inherits(location, "srcref")) {
        self$cat_line("  ", attr(location, "srcfile")$filename,
          ":", location[1L], ":", location[2L], " [", class(result)[1L], "]")
      }
    }
  ))$new(file = file)
}

builder_diagnostic_step <- function(label, code) {
  if (!builder_diagnostics_enabled()) return(force(code))
  progress <- Sys.getenv("GRATIA_TEST_PROGRESS")
  checkpoint <- function(state) {
    line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "[gratia builder]", state, label)
    if (nzchar(progress)) {
      cat(line, "\n", sep = "", file = progress, append = TRUE)
    } else {
      cat(line, "\n", sep = "", file = stderr())
      flush(stderr())
    }
  }
  checkpoint("START")
  value <- force(code)
  checkpoint("DONE")
  value
}

# Keep the last call's arguments on disk for replay in a fresh R session:
# input <- readRDS("check-spatial-trace.log.rds")
# do.call(get(input$function_name, asNamespace("sf")), input$arguments)
local_spatial_trace <- function(file) {
  if (!nzchar(file)) return(invisible(NULL))

  ns <- asNamespace("sf")
  version <- as.character(utils::packageVersion("sf"))
  native_input <- paste0(file, ".rds")
  functions <- intersect(c("CPL_transform", "CPL_transform_bounds",
    "CPL_crs_equivalent", "CPL_crs_parameters", "CPL_crs_from_input",
    "CPL_wrap_dateline", "CPL_gdal_segmentize", "CPL_geos_op",
    "CPL_geos_op2", "CPL_geos_binop", "CPL_geos_normalize",
    "CPL_geos_is_valid", "CPL_geos_is_empty", "CPL_get_bbox"), ls(ns))

  record <- function(...) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), ..., "\n",
      file = file, append = TRUE)
  }
  checkpoint <- function(name, frame) {
    record("ENTER", name)
    arguments <- mget(names(formals(get(name, ns))), frame, inherits = FALSE)
    saveRDS(list(package = version, function_name = name, arguments = arguments),
      native_input, compress = FALSE)
    record("START", name)
  }

  record("Tracing sf", version, "in R process", Sys.getpid())
  traced <- character()
  withr::defer({
    for (name in traced) suppressMessages(untrace(name, where = ns))
    record("Finished tracing sf")
  }, envir = parent.frame())
  for (name in functions) {
    suppressMessages(trace(name, where = ns, print = FALSE,
      tracer = bquote(.(checkpoint)(.(name), environment())),
      exit = bquote(.(record)("EXIT", .(name)))))
    traced <- c(traced, name)
  }
  invisible(NULL)
}
