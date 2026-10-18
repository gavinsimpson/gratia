# stderr checkpoints survive an abrupt exit before testthat prints its summary.
# Enable them automatically on Windows ARM, or explicitly on another builder.
builder_diagnostics_enabled <- function() {
  setting <- Sys.getenv("GRATIA_TEST_DIAGNOSTICS")
  if (nzchar(setting)) return(identical(setting, "true"))
  .Platform$OS.type == "windows" &&
    grepl("aarch64|arm64", R.version$platform, ignore.case = TRUE)
}

builder_diagnostic_step <- function(label, code) {
  if (!builder_diagnostics_enabled()) return(force(code))
  checkpoint <- function(state) {
    cat("[gratia builder] ", state, " ", label, "\n", sep = "", file = stderr())
    flush(stderr())
  }
  checkpoint("START")
  value <- force(code)
  checkpoint("DONE")
  value
}
