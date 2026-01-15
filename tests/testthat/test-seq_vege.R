test_that("seq_vege() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_vege_poly = function(...) Rsequoia2:::seq_poly,
      get_vege_line = function(...) Rsequoia2:::seq_line,
      get_vege_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_vege(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_vege() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_vege_poly = function(...) Rsequoia2:::seq_poly,
      get_vege_line = function(...) Rsequoia2:::seq_line,
      get_vege_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_vege(seq_cache, verbose = FALSE, overwrite = TRUE)
    vege <- lapply(paths, read_sf)

    poly <- vege[grepl("poly", names(vege))][[1]]
    expect_all_true(all(sf::st_geometry_type(poly) %in% c("POLYGON", "MULTIPOLYGON")))
    expect_true(sf::st_crs(poly) == sf::st_crs(2154))

    line <- vege[grepl("line", names(vege))][[1]]
    expect_all_true(all(sf::st_geometry_type(line) %in% c("LINESTRING", "MULTILINESTRING")))
    expect_true(sf::st_crs(line) == sf::st_crs(2154))

    point <- vege[grepl("point", names(vege))][[1]]
    expect_all_true(all(sf::st_geometry_type(point) %in% c("POINT", "MULTIPOINT")))
    expect_true(sf::st_crs(line) == sf::st_crs(2154))

  })
})

test_that("seq_vege() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_vege_poly = function(...) Rsequoia2:::seq_poly,
      get_vege_line = function(...) Rsequoia2:::seq_line,
      get_vege_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_vege(seq_cache, verbose = FALSE, overwrite = TRUE)
    vege <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(vege, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_vege() write layer even if there no data", {
  with_seq_cache({

    local_mocked_bindings(
      get_vege_poly = function(...) Rsequoia2:::seq_empty,
      get_vege_line = function(...) Rsequoia2:::seq_empty,
      get_vege_point = function(...) Rsequoia2:::seq_empty,
    )

    paths <- seq_vege(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))

    vege <- lapply(paths, read_sf)
    expect_all_true(vapply(vege, \(x) nrow(x) == 0, logical(1)))

  })
})
