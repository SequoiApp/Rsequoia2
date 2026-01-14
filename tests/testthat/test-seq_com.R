test_that("seq_com() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_com_poly = function(...) Rsequoia2:::seq_poly,
      get_com_line = function(...) Rsequoia2:::seq_line,
      get_com_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_com(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 5)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_com() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_com_poly = function(...) Rsequoia2:::seq_poly,
      get_com_line = function(...) Rsequoia2:::seq_line,
      get_com_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_com(seq_cache, verbose = FALSE, overwrite = TRUE)
    com <- lapply(paths, read_sf)

    poly <- com[grepl("poly", names(com))]
    expect_all_true(
      vapply(
        poly,
        \(x) all(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")),
        logical(1)
      )
    )

    line <- com[grepl("line", names(com))]
    expect_all_true(
      vapply(
        line,
        \(x) all(sf::st_geometry_type(x) %in% c("LINESTRING", "MULTILINESTRING")),
        logical(1)
      )
    )

    point <- com[grepl("point", names(com))]
    expect_all_true(
      vapply(
        point,
        \(x) all(sf::st_geometry_type(x) %in% c("POINT", "MULTIPOINT")),
        logical(1)
      )
    )

  })
})

test_that("seq_com() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_com_poly = function(...) Rsequoia2:::seq_poly,
      get_com_line = function(...) Rsequoia2:::seq_line,
      get_com_point = function(...) Rsequoia2:::seq_point,
    )

    paths <- seq_com(seq_cache, verbose = FALSE, overwrite = TRUE)
    com <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(com, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_com() does not write layers when no commune intersects", {
  with_seq_cache({

    local_mocked_bindings(
      get_com_poly = function(...) NULL,
      get_com_line = function(...) NULL,
      get_com_point = function(...) NULL
    )

    paths <- seq_com(seq_cache, verbose = FALSE)

    expect_length(paths, 0)
  })
})
