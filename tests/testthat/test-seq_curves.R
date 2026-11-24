mock_curve <- function(x, ...) {
  bb <- sf::st_bbox(x)
  sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]), 2, byrow = TRUE)),
    crs = sf::st_crs(x)
  ))
}

test_that("seq_curves() writes expected layer", {
  with_seq_cache({
    local_mocked_bindings(get_curves = mock_curve)

    path <- seq_curves(seq_cache, verbose = FALSE, overwrite = TRUE)
    curves <- read_sf(path)

    expect_true(file.exists(path))
    expect_all_true(sf::st_geometry_type(curves) %in% c("LINESTRING", "MULTILINESTRING"))
    expect_true(sf::st_crs(curves) == sf::st_crs(2154))
    expect_true(seq_field("identifier")$name %in% names(curves))
  })
})

test_that("seq_curves() doesn't write when no data", {
  with_seq_cache({
    called <- 0

    local_mocked_bindings(
      get_curves = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    path <- seq_curves(seq_cache, verbose = FALSE)

    expect_null(path)
    expect_equal(called, 0)
  })
})
