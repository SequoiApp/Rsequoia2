test_that("seq_vides() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_vides = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_vides(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_vides() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_vides = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_vides(seq_cache, verbose = FALSE, overwrite = TRUE)
    gaps <- read_sf(path)

    expect_all_true(sf::st_geometry_type(gaps) %in% c("POLYGON", "MULTIPOLYGON"))
    expect_true(sf::st_crs(gaps) == sf::st_crs(2154))
  })
})

test_that("seq_vides() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_vides = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_vides(seq_cache, verbose = FALSE, overwrite = TRUE)
    gaps <- read_sf(path)

    id_field <- seq_field("identifier")$name
    expect_true(id_field %in% names(gaps))
  })
})

test_that("seq_vides() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      seq_vides = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_vides(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
