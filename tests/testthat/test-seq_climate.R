test_that("seq_climate() fetches climate data by default", {
  with_seq_cache({

    tracker <- list(args = list())

    local_mocked_bindings(
      .climate_fetcher = function(
    x, buffer, cache, dirname, id, overwrite, verbose
      ) {
        tracker$args <<- list(
          x = x,
          buffer = buffer,
          cache = cache,
          dirname = dirname,
          id = id,
          overwrite = overwrite,
          verbose = verbose
        )

        file.path(dirname, "CLIM_TYPE_poly.gpkg")
      }
    )

    path <- seq_climate(
      dirname = seq_cache,
      cache = "climate_cache",
      verbose = FALSE,
      overwrite = TRUE
    )

    expect_identical(path, file.path(seq_cache, "CLIM_TYPE_poly.gpkg"))
    expect_true(sf::st_equals(tracker$args$x, p, sparse = FALSE)[1, 1])
    expect_identical(tracker$args$buffer, 100)
    expect_identical(tracker$args$cache, "climate_cache")
    expect_identical(tracker$args$dirname, seq_cache)
    expect_identical(tracker$args$id, "ECKMUHL")
    expect_identical(tracker$args$overwrite, TRUE)
    expect_identical(tracker$args$verbose, FALSE)
  })
})


test_that("seq_climate() forwards the buffer argument", {
  with_seq_cache({

    tracker <- list(buffer = NULL)

    local_mocked_bindings(
      .climate_fetcher = function(x, buffer, ...) {
        tracker$buffer <<- buffer
        "climate.gpkg"
      }
    )

    seq_climate(
      dirname = seq_cache,
      buffer = 250,
      verbose = FALSE
    )

    expect_identical(tracker$buffer, 250)
  })
})


test_that("seq_climate() returns NULL when no climate is found", {
  with_seq_cache({

    local_mocked_bindings(
      .climate_fetcher = function(...) NULL
    )

    path <- seq_climate(
      dirname = seq_cache,
      verbose = FALSE
    )

    expect_null(path)
  })
})


test_that("seq_climate() passes the project identifier", {
  with_seq_cache({

    tracker <- list(id = NULL)

    local_mocked_bindings(
      .climate_fetcher = function(x, id, ...) {
        tracker$id <<- id
        "climate.gpkg"
      }
    )

    seq_climate(
      dirname = seq_cache,
      verbose = FALSE
    )

    expect_identical(tracker$id, "ECKMUHL")
  })
})
