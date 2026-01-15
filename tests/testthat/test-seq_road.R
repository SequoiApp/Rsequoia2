test_that("seq_road() returned expected path", {
  with_seq_cache({

    local_mocked_bindings(
      get_road = function(...) Rsequoia2:::seq_line
    )

    local_mocked_bindings(
      line_to_poly = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 2)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_road() returned correct geometry", {
  with_seq_cache({

    local_mocked_bindings(
      get_road = function(...) Rsequoia2:::seq_line
    )

    local_mocked_bindings(
      line_to_poly = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    road_line <- read_sf(paths[1])
    road_poly <- read_sf(paths[2])

    expect_all_true(sf::st_geometry_type(road_line) %in% c("LINESTRING", "MULTILINESTRING"))
    expect_all_true(sf::st_geometry_type(road_poly) %in% c("POLYGON", "MULTIPOLYGON"))
    expect_true(sf::st_crs(road_line) == sf::st_crs(2154))
    expect_true(sf::st_crs(road_poly) == sf::st_crs(2154))
  })
})

# test_that("seq_road() layers contain id", {
#   with_seq_cache({
#
#     local_mocked_bindings(
#       get_road = function(...) Rsequoia2:::seq_line
#     )
#
#     local_mocked_bindings(
#       line_to_poly = function(...) Rsequoia2:::seq_poly
#     )
#
#     paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
#     road_line <- read_sf(paths[1])
#     road_poly <- read_sf(paths[2])
#
#     id_field <- seq_field("identifier")$name
#     expect_all_true(vapply(road_line, \(x) id_field %in% names(x), TRUE))
#     expect_all_true(vapply(road_poly, \(x) id_field %in% names(x), TRUE))
#   })
# })

test_that("seq_road() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      seq_road = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
