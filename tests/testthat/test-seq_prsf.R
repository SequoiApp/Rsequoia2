test_that("seq_prsf() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_prsf = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_prsf(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_prsf() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_prsf = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_prsf(seq_cache, verbose = FALSE, overwrite = TRUE)
    prsf <- read_sf(paths)

    expect_all_true(sf::st_geometry_type(prsf) %in% c("POINT", "MULTIPOINT"))
    expect_true(sf::st_crs(prsf) == sf::st_crs(2154))

  })
})

test_that("seq_prsf() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_prsf = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_prsf(seq_cache, verbose = FALSE, overwrite = TRUE)
    prsf <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(prsf, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_prsf() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_prsf = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_prsf(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
