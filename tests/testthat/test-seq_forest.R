test_that("seq_forest() writes expected layer", {
  with_seq_cache({
    local_mocked_bindings(
      get_forest = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_forest(seq_cache, verbose = FALSE, overwrite = TRUE)
    forest <- read_sf(path)

    expect_true(file.exists(path))
    expect_true(all(sf::st_geometry_type(forest) %in% c("POLYGON", "MULTIPOLYGON")))
    expect_true(sf::st_crs(forest) == sf::st_crs(2154))
    expect_true(seq_field("identifier")$name %in% names(forest))
  })
})

test_that("seq_forest() doesn't write when no data", {
  with_seq_cache({
    called <- 0

    local_mocked_bindings(
      get_forest = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    path <- seq_forest(seq_cache, verbose = FALSE)

    expect_null(path)
    expect_equal(called, 0)
  })
})
