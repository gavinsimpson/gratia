# Namespaced worker helpers must resolve against the package under test, also
# for load_all() and coverage runs that have another version installed.
gratia_test_workers <- function(n) {
  mirai::daemons(n, dispatcher = FALSE, autoexit = NA)
  setup <- mirai::everywhere({
    .libPaths(libraries)
    if (development) {
      pkgload::load_all(path, quiet = TRUE, helpers = FALSE)
    } else {
      loadNamespace("gratia", lib.loc = dirname(path))
    }
    invisible(NULL)
  }, libraries = .libPaths(), development = pkgload::is_dev_package("gratia"),
    path = getNamespaceInfo(asNamespace("gratia"), "path"))
  invisible(mirai::collect_mirai(setup, options = ".stop"))
}
