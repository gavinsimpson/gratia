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
