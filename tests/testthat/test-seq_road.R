test_that("seq_road() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_road = function(...) Rsequoia2:::seq_line
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_road() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_road = function(...) Rsequoia2:::seq_line
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    roads <- read_sf(paths)

    expect_all_true(sf::st_geometry_type(roads) %in% c("LINESTRING"))
    expect_true(sf::st_crs(roads) == sf::st_crs(2154))

  })
})

test_that("seq_road() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_road = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    roads <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(roads, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_road() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_road = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)

    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
