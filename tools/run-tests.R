# Install into a temporary library so every worker uses the current checkout.
# Installed test copies also keep snapshot cleanup away from the source files.
run_tests <- function() {
  package_path <- normalizePath(".", winslash = "/", mustWork = TRUE)
  package <- read.dcf(file.path(package_path, "DESCRIPTION"),
    fields = "Package")[[1L]]
  test_library <- tempfile("gratia-test-library-")
  dir.create(test_library)
  on.exit(unlink(test_library, recursive = TRUE), add = TRUE)
  .libPaths(c(test_library, .libPaths()))
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))

  r_command <- file.path(R.home("bin"),
    if (.Platform$OS.type == "windows") "R.exe" else "R")
  status <- system2(r_command, c("CMD", "INSTALL", "--install-tests",
    shQuote(paste0("--library=", test_library)), shQuote(package_path)))
  if (status != 0L) stop("Package installation failed.", call. = FALSE)

  cpu_count <- Sys.getenv("TESTTHAT_CPUS", "4")
  if (!grepl("^[1-9][0-9]*$", cpu_count) ||
      as.double(cpu_count) > .Machine$integer.max) {
    stop("TEST_CPUS must be a positive integer.", call. = FALSE)
  }
  options(Ncpus = as.integer(cpu_count))
  options(device = function(...) grDevices::pdf(file = NULL))
  testthat::test_package(package, reporter = "summary",
    stop_on_failure = TRUE, stop_on_warning = TRUE)
}

run_tests()
