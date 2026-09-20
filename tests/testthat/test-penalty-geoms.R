penalty_geom_fixture <- function() {
  cells <- expand.grid(.row = 1:3, .col = 1:3)
  cells$.value <- c(-4, 0, 2, 0, 8, 1, 2, 1, 3)
  cells$.smooth <- "s(x)"
  cells$.type <- "test penalty"
  cells$.penalty <- "first"
  other <- cells
  other$.penalty <- "second"
  other$.value <- -cells$.value
  out <- tibble::as_tibble(rbind(cells, other))
  class(out) <- c("penalty_df", class(out))
  out
}

test_that("penalty geom selection preserves cells and scales", {
  pen <- penalty_geom_fixture()
  fields <- c("x", "y", "xmin", "xmax", "ymin", "ymax", "fill")
  for (as_matrix in c(FALSE, TRUE)) {
    for (normalize in c(FALSE, TRUE)) {
      raster <- draw(pen,
        geom = "raster", as_matrix = as_matrix,
        normalize = normalize
      )
      tile <- draw(pen,
        geom = "tile", as_matrix = as_matrix,
        normalize = normalize
      )
      expect_length(raster, 2L)
      expect_length(tile, 2L)
      for (i in seq_len(2L)) {
        expect_s3_class(raster[[i]]$layers[[1]]$geom, "GeomRaster")
        expect_s3_class(tile[[i]]$layers[[1]]$geom, "GeomTile")
        expect_equal(tile[[i]]$data, raster[[i]]$data)
        a <- ggplot2::ggplot_build(raster[[i]])$data[[1]]
        b <- ggplot2::ggplot_build(tile[[i]])$data[[1]]
        expect_equal(b[fields], a[fields])
        expect_equal(as.numeric(b$x), rep(1:3, each = 3))
        rows <- if (as_matrix) 3:1 else 1:3
        expect_equal(as.numeric(b$y), rep(rows, 3))
        expect_equal(b$xmax - b$xmin, rep(1, 9))
        expect_equal(b$ymax - b$ymin, rep(1, 9))
        expect_true(all(is.na(b$colour)))
        values <- tile[[i]]$data$.value
        expected <- if (i == 1L) pen$.value[1:9] else pen$.value[10:18]
        if (normalize) {
          expect_equal(range(values), c(-1, 1))
        } else {
          expect_equal(values, expected)
        }
      }
    }
  }
})

test_that("penalty geom defaults to raster and rejects invalid choices", {
  pen <- penalty_geom_fixture()
  default <- draw(pen)
  raster <- draw(pen, geom = "raster")
  for (i in seq_len(2L)) {
    expect_s3_class(default[[i]]$layers[[1]]$geom, "GeomRaster")
    expect_equal(
      ggplot2::ggplot_build(default[[i]])$data,
      ggplot2::ggplot_build(raster[[i]])$data
    )
  }
  expect_error(draw(pen, geom = "invalid"), "'arg' should be one of")
  expect_error(
    gratia:::plot_penalty(pen, geom = "invalid"),
    "'arg' should be one of"
  )
})
