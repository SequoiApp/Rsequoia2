test_that("seq_infra() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) Rsequoia2:::seq_line,
      get_infra_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_infra() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) Rsequoia2:::seq_line,
      get_infra_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)
    infra <- lapply(paths, read_sf)

    poly <- infra[grepl("poly", names(infra))][[1]]
    expect_all_true(all(sf::st_geometry_type(poly) %in% c("POLYGON", "MULTIPOLYGON")))
    expect_true(sf::st_crs(poly) == sf::st_crs(2154))

    line <- infra[grepl("line", names(infra))][[1]]
    expect_all_true(all(sf::st_geometry_type(line) %in% c("LINESTRING", "MULTILINESTRING")))
    expect_true(sf::st_crs(line) == sf::st_crs(2154))

    point <- infra[grepl("point", names(infra))][[1]]
    expect_all_true(all(sf::st_geometry_type(point) %in% c("POINT", "MULTIPOINT")))
    expect_true(sf::st_crs(line) == sf::st_crs(2154))

  })
})

test_that("seq_infra() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) Rsequoia2:::seq_line,
      get_infra_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)
    infra <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(infra, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_infra() write layer even if there no data", {
  with_seq_cache({

    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_empty,
      get_infra_line = function(...) Rsequoia2:::seq_empty,
      get_infra_point = function(...) Rsequoia2:::seq_empty,
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))

    infra <- lapply(paths, read_sf)
    expect_all_true(vapply(infra, \(x) nrow(x) == 0, logical(1)))

  })
})
