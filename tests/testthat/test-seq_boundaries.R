test_that("seq_boundaries() return needed product", {
  with_seq_cache({

    boundaries <- seq_boundaries(dirname = seq_cache, verbose = FALSE, overwrite = T)
    expect_all_true(vapply(boundaries, file.exists, TRUE))

    boundaries_sf <- lapply(boundaries, read_sf)

    # all layer conbain identifier
    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(boundaries_sf, \(x) identifier %in% names(x), TRUE))

    poly <- boundaries_sf[grepl("poly", names(boundaries_sf))]
    expect_all_true(
      vapply(poly, \(x) sf::st_geometry_type(x) %in% c("MULTIPOLYGON", "POLYGON"), TRUE)
    )

    line <- boundaries_sf[grepl("line", names(boundaries_sf))]
    expect_all_true(
      vapply(line, \(x) sf::st_geometry_type(x) %in% c("MULTILINESTRING", "LINESTRING"), TRUE)
    )

    point <- boundaries_sf[grepl("point", names(boundaries_sf))]
    expect_all_true(
      vapply(point, \(x) sf::st_geometry_type(x) %in% c("MULTIPOINT", "POINT"), TRUE)
    )
  })
})

test_that("seq_boundaries() preserves CRS", {

  with_seq_cache({

    boundaries <- seq_boundaries(dirname = seq_cache, verbose = FALSE)

    crs <- vapply(
      lapply(boundaries, sf::read_sf),
      \(x) sf::st_crs(x)$epsg,
      numeric(1)
    )

    expect_all_true(crs == 2154)
  })
})

test_that("forest boundaries have one feature per identifier", {

  with_seq_cache({

    boundaries <- seq_boundaries(dirname = seq_cache, verbose = FALSE)

    forest_poly <- sf::read_sf(boundaries[["v.seq.forest.poly"]])

    identifier <- seq_field("identifier")$name
    expect_equal(length(unique(forest_poly[[identifier]])), nrow(forest_poly))
  })
})

test_that("owner boundaries are split by owner", {

  with_seq_cache({

    owner <- seq_field("owner")$name
    p[[owner]] <- "OWNER_1"
    p[[owner]][1:2] <- "OWNER_2"

    testthat::local_mocked_bindings(
      seq_read = function(...) p
    )

    boundaries <- seq_boundaries(dirname = seq_cache, verbose = FALSE)

    owner_poly <- sf::read_sf(boundaries[["v.seq.owner.poly"]])

    expect_shape(owner_poly, nrow = 2)
  })
})
