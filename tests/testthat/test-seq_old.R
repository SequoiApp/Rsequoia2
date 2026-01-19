test_that("seq_old() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_old = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_old(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_old() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_old = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_old(seq_cache, verbose = FALSE, overwrite = TRUE)
    old <- read_sf(path)

    expect_all_true(sf::st_geometry_type(old) %in% c("POLYGON", "MULTIPOLYGON"))
    expect_true(sf::st_crs(old) == sf::st_crs(2154))
  })
})

test_that("seq_old() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_old = function(...) Rsequoia2:::seq_poly
    )

    path <- seq_old(seq_cache, verbose = FALSE, overwrite = TRUE)
    old <- read_sf(path)

    identifier <- seq_field("identifier")$name
    expect_true(identifier %in% names(old))
  })
})

test_that("seq_old() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_old = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    path <- seq_old(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(path, 0)
    expect_equal(called, 0)
  })
})
