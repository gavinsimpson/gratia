# stderr checkpoints survive an abrupt exit before testthat prints its summary.
# The focused Windows ARM sdmTMB job enables them explicitly.
builder_diagnostics_enabled <- function() {
  identical(Sys.getenv("GRATIA_TEST_DIAGNOSTICS"), "true")
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
