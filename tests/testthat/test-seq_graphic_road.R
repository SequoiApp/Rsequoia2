test_that("seq_graphic_road() returned expected path", {
  with_seq_cache({

    type_field <- seq_field("type")$name

    poly <- Rsequoia2:::seq_poly
    poly[[type_field]] <- c("RN", "RD", "RF")

    line <- Rsequoia2:::seq_line
    line[[type_field]] <- c("RN", "RD","PN")

    mock_seq_read <- function(x, dirname = ".") {
      if (x == "v.cad.vides.poly") {
        poly
      } else if (x == "v.road.topo.line") {
        # return a simple line sf
        line
      }
    }

    local_mocked_bindings(
      seq_read = mock_seq_read,
      .package = "Rsequoia2"
    )

    local_mocked_bindings(
      vides_to_line = function(...) line
    )

    local_mocked_bindings(
      vides_to_poly = function(...) poly
    )

    paths <- seq_graphic_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 2)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_graphic_road() returned correct geometry", {
  with_seq_cache({

    type_field <- seq_field("type")$name

    poly <- Rsequoia2:::seq_poly
    poly[[type_field]] <- c("RN", "RD", "RF")

    line <- Rsequoia2:::seq_line
    line[[type_field]] <- c("RN", "RD","PN")

    mock_seq_read <- function(x, dirname = ".") {
      if (x == "v.cad.vides.poly") {
        poly
      } else if (x == "v.road.topo.line") {
        # return a simple line sf
        line
      }
    }

    local_mocked_bindings(
      seq_read = mock_seq_read,
      .package = "Rsequoia2"
    )

    local_mocked_bindings(
      vides_to_line = function(...) line
    )

    local_mocked_bindings(
      vides_to_poly = function(...) poly
    )

    paths <- seq_graphic_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    road_line <- read_sf(paths[1])
    road_poly <- read_sf(paths[2])

    expect_all_true(sf::st_geometry_type(road_line) %in% c("LINESTRING", "MULTILINESTRING"))
    expect_all_true(sf::st_geometry_type(road_poly) %in% c("POLYGON", "MULTIPOLYGON"))
    expect_true(sf::st_crs(road_line) == sf::st_crs(2154))
    expect_true(sf::st_crs(road_poly) == sf::st_crs(2154))
  })
})

# test_that("seq_graphic_road() layers contain id", {
#   with_seq_cache({
#
#     type_field <- seq_field("type")$name
#
#     poly <- Rsequoia2:::seq_poly
#     poly[[type_field]] <- c("RN", "RD", "RF")
#
#     line <- Rsequoia2:::seq_line
#     line[[type_field]] <- c("RN", "RD","PN")
#
#     mock_seq_read <- function(x, dirname = ".") {
#       if (x == "v.cad.vides.poly") {
#         poly
#       } else if (x == "v.road.topo.line") {
#         # return a simple line sf
#         line
#       }
#     }
#
#     local_mocked_bindings(
#       seq_read = mock_seq_read,
#       .package = "Rsequoia2"
#     )
#
#     local_mocked_bindings(
#       vides_to_line = function(...) line
#     )
#
#     local_mocked_bindings(
#       vides_to_poly = function(...) poly
#     )
#
#     paths <- seq_graphic_road(seq_cache, verbose = FALSE, overwrite = TRUE)
#     road_sf <- lapply(paths, read_sf)
#
#     id_field <- seq_field("identifier")$name
#     expect_all_true(vapply(road_sf, \(x) id_field %in% names(x), TRUE))
#   })
# })

test_that("seq_graphic_road() doesn't write if there no data", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      seq_graphic_road = function(...) NULL,
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_graphic_road(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(paths, 0)
    expect_equal(called, 0)
  })
})
