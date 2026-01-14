test_that("seq_toponyme() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_toponyme = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_toponyme(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 1)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_toponyme() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_toponyme = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_toponyme(seq_cache, verbose = FALSE, overwrite = TRUE)
    toponyme <- read_sf(paths)

    expect_all_true(sf::st_geometry_type(toponyme) %in% c("POINT", "MULTIPOINT"))
    expect_true(sf::st_crs(toponyme) == sf::st_crs(2154))

  })
})

test_that("seq_toponyme() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_toponyme = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_toponyme(seq_cache, verbose = FALSE, overwrite = TRUE)
    toponyme <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(toponyme, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_toponyme() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_toponyme = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_toponyme(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
