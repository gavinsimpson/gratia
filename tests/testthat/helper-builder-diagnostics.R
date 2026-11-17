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

  native_input <- paste0(file, ".rds")
  record <- function(...) {
    line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), ...)
    cat(line, "\n", sep = "", file = file, append = TRUE)
  }
  hook <- packageEvent("sf", "onLoad")
  previous <- getHook(hook)
  traced <- character()
  ns <- NULL
  withr::defer({
    setHook(hook, previous, action = "replace")
    for (name in traced) suppressMessages(untrace(name, where = ns))
    record("Finished tracing sf")
  }, envir = parent.frame())

  install <- function(...) {
    ns <<- asNamespace("sf")
    version <- as.character(getNamespaceVersion(ns))
    functions <- intersect(c("CPL_transform", "CPL_transform_bounds",
      "CPL_crs_equivalent", "CPL_crs_parameters", "CPL_crs_from_input",
      "CPL_proj_direct", "CPL_axis_order_authority_compliant",
      "CPL_wrap_dateline", "CPL_gdal_segmentize", "CPL_geos_op",
      "CPL_geos_op2", "CPL_geos_binop", "CPL_geos_normalize",
      "CPL_geos_is_valid", "CPL_geos_is_empty", "CPL_get_bbox"), ls(ns))
    checkpoint <- function(name, frame) {
      record("ENTER", name)
      arguments <- mget(names(formals(get(name, ns))), frame, inherits = FALSE)
      saveRDS(list(package = version, function_name = name, arguments = arguments),
        native_input, compress = FALSE)
      record("START", name)
    }

    record("Tracing sf", version, "in R process", Sys.getpid())
    for (name in functions) {
      suppressMessages(trace(name, where = ns, print = FALSE,
        tracer = bquote(.(checkpoint)(.(name), environment())),
        exit = bquote(.(record)("EXIT", .(name)))))
      traced <<- c(traced, name)
    }
  }

  # Let the tests load sf at the same point as in a check without tracing.
  if (isNamespaceLoaded("sf")) {
    install()
  } else {
    setHook(hook, install)
  }
  invisible(NULL)
}

# Record the expressions that ggplot2 and sf are about to evaluate. Limit this
# to the spherical builds that stopped during the Windows ARM package check.
local_plot_build_trace <- function() {
  file <- Sys.getenv("GRATIA_SPATIAL_TRACE")
  if (!nzchar(file)) return(invisible(NULL))

  record <- function(...) {
    line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3", tz = "UTC"), ...)
    cat(line, "\n", sep = "", file = file, append = TRUE)
  }
  gg <- asNamespace("ggplot2")
  sf <- asNamespace("sf")
  cleanup <- list()
  withr::defer({
    for (entry in rev(cleanup)) {
      suppressMessages(untrace(entry$name, where = entry$where))
    }
  }, envir = parent.frame())

  trace_steps <- function(name, where, label) {
    fn <- get0(name, where, inherits = FALSE)
    if (!is.function(fn) || !is.call(body(fn)) ||
        !identical(body(fn)[[1L]], as.name("{"))) {
      record("Cannot trace", label)
      return(invisible(NULL))
    }
    steps <- as.list(body(fn))[-1L]
    labels <- vapply(steps, deparse, character(1L), width.cutoff = 100L, nlines = 1L)
    mark_step <- function(frame) {
      key <- ".gratia_trace_step"
      step <- get0(key, frame, inherits = FALSE, ifnotfound = 0L) + 1L
      assign(key, step, frame)
      record("STEP", label, step, labels[[step]])
    }
    suppressMessages(trace(name, where = where, print = FALSE,
      at = as.list(seq_along(steps) + 1L),
      tracer = bquote(.(mark_step)(environment())),
      exit = bquote(.(record)("EXIT", .(label)))))
    cleanup[[length(cleanup) + 1L]] <<- list(name = name, where = where)
  }

  trace_steps("ggplot_build", gg, "ggplot2::ggplot_build")
  trace_steps("ggplot_build.ggplot2::ggplot", get(".__S3MethodsTable__.", gg),
    "ggplot2 ggplot build method")
  for (name in c("setup", "setup_panel_params")) {
    trace_steps(name, ggplot2::Layout, paste0("ggplot2::Layout$", name))
  }
  for (name in c("setup_params", "setup_data", "setup_panel_params")) {
    trace_steps(name, ggplot2::CoordSf, paste0("ggplot2::CoordSf$", name))
  }
  for (name in c("st_sfc", "st_graticule")) {
    trace_steps(name, sf, paste0("sf::", name))
  }
  invisible(NULL)
}
