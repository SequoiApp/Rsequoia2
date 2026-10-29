test_that("seq_forest() returns expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_forest = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_forest(
      seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    expect_length(paths, 1)
    expect_true(all(file.exists(unlist(paths))))
  })
})


test_that("seq_forest() returns correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_forest = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_forest(
      seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    forest <- sf::read_sf(path)

    expect_true(all(
      sf::st_geometry_type(forest) %in% c("POLYGON", "MULTIPOLYGON")
    ))
    expect_true(sf::st_crs(forest) == sf::st_crs(2154))
  })
})


test_that("seq_forest() layers contain identifier", {
  with_seq_cache({

    local_mocked_bindings(
      get_forest = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_forest(
      seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    forest <- sf::read_sf(path)

    identifier <- seq_field("identifier")$name

    expect_true(identifier %in% names(forest))
  })
})


test_that("seq_forest() does not write when no data", {
  with_seq_cache({

    called <- 0

    local_mocked_bindings(
      get_forest = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    path <- seq_forest(
      seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    expect_null(path)
    expect_equal(called, 0)
  })
})
