test_that("seq_voids() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_voids = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_voids(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_voids() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_voids = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_voids(seq_cache, verbose = FALSE, overwrite = TRUE)
    gaps <- read_sf(path)

    expect_all_true(sf::st_geometry_type(gaps) %in% c("POLYGON", "MULTIPOLYGON"))
    expect_true(sf::st_crs(gaps) == sf::st_crs(2154))
  })
})

test_that("seq_voids() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_voids = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_voids(seq_cache, verbose = FALSE, overwrite = TRUE)
    gaps <- read_sf(path)

    id_field <- seq_field("identifier")$name
    expect_true(id_field %in% names(gaps))
  })
})

test_that("seq_voids() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      seq_voids = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_voids(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
