test_that("seq_access() returns expected paths when accessibility exists", {

  with_seq_cache({
    local_mocked_bindings(
      get_accessibility = function(...) p #need to return parca becaus there intersection
    )

    paths <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))

  })

})

test_that("seq_access() layers contain identifier", {

  with_seq_cache({

    local_mocked_bindings(
      get_accessibility = function(x, type, ...) p
    )

    paths <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)

    layers <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name

    expect_all_true(
      vapply(layers, function(x) identifier %in% names(x), logical(1))
    )

  })

})

test_that("seq_access() writes entry point even when no accessibility", {

  with_seq_cache({
    local_mocked_bindings(
      get_accessibility = function(...) NULL
    )

    paths <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)

    expect_length(paths, 1)
    expect_true(file.exists(paths[[1]]))
    entry <- read_sf(paths[[1]])
    expect_equal(nrow(entry), 0)

  })

})

test_that("seq_access() layers are in EPSG:2154", {

  with_seq_cache({

    local_mocked_bindings(
      get_accessibility = function(...) p
    )

    paths <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)

    layers <- lapply(paths, read_sf)

    expect_all_true(
      vapply(layers, \(x) sf::st_crs(x)$epsg == 2154, logical(1))
    )

  })
})

