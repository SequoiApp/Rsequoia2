test_that("seq_curves() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_curves = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_curves(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_curves() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_curves = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_curves(seq_cache, verbose = FALSE, overwrite = TRUE)
    curves <- read_sf(paths)

    expect_all_true(sf::st_geometry_type(curves) %in% c("POINT", "MULTIPOINT"))
    expect_true(sf::st_crs(curves) == sf::st_crs(2154))

  })
})

test_that("seq_curves() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_curves = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_curves(seq_cache, verbose = FALSE, overwrite = TRUE)
    curves <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(curves, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_curves() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_curves = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_curves(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
