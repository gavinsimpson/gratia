# Replay the input to ggplot2's layer-statistics calculation in fresh R.
path <- commandArgs(trailingOnly = TRUE)
stopifnot(length(path) == 1L)
layer <- readRDS(path)
geometry <- layer$geometry
stopifnot(nrow(layer) > 0L, inherits(geometry, "sfc"), length(geometry) == nrow(layer))

for (package in c("sf", "vctrs")) {
  cat(package, as.character(packageVersion(package)), find.package(package), "\n")
}
print(sf::sf_extSoftVersion())
cat("START vec_ptype for", length(geometry), class(geometry)[[1L]], "geometries\n")
flush(stdout())
prototype <- vctrs::vec_ptype(geometry)
stopifnot(length(prototype) == 0L, identical(class(prototype), class(geometry)),
  identical(sf::st_crs(prototype), sf::st_crs(geometry)),
  identical(sf::st_precision(prototype), sf::st_precision(geometry)))
cat("RETURN vec_ptype for the geometry column\n")

cat("START vec_ptype for the whole layer\n")
flush(stdout())
prototype <- vctrs::vec_ptype(layer)
stopifnot(nrow(prototype) == 0L, identical(names(prototype), names(layer)),
  identical(class(prototype$geometry), class(geometry)))
cat("RETURN vec_ptype for the whole layer\n")
