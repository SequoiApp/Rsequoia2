test_that("get_vides() aborts with invalid source", {

  expect_error(
    gaps <- get_vides(Rsequoia2:::seq_poly),
    "Source must be one of"
  )
})

test_that("get_vides() returns same geom when no cadastral data retrieved", {

  x <- Rsequoia2:::seq_poly
  source_field <- seq_field("source")$name
  x[[source_field]] <- "bdp"

  fetch_envelope <- envelope(x, 1000)

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  gaps <- get_vides(x) |> quiet()

  # tests
  expect_equal(sf::st_geometry(gaps), sf::st_geometry(fetch_envelope))
})

test_that("get_vides() works", {

  mock_envelope <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0,
          10, 0,
          10,10,
          0,10,
          0, 0),
        ncol = 2,
        byrow = TRUE
      )))
    ),
    crs = 2154
  )

  mock_parcels <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(3, 3,
          7, 3,
          7, 7,
          3, 7,
          3, 3),
        ncol = 2,
        byrow = TRUE
      )))
    ),
    crs = 2154
  )

  x <- Rsequoia2:::seq_poly
  source_field <- seq_field("source")$name
  x[[source_field]] <- "bdp"

  mask <- sf::st_union(
    sf::st_geometry(mock_parcels),
    sf::st_geometry(x)
  ) |> sf::st_union()

  difference <- sf::st_difference(mock_envelope, mask) |>
    quiet() |>
    sf::st_make_valid()

  testthat::local_mocked_bindings(
    envelope = function(...) mock_envelope
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) mock_parcels,
    .package = "happign"
  )

  res <- get_vides(x)

  expect_s3_class(res, "sf")

  # Compare area
  expect_equal(
    as.numeric(sf::st_area(res)),
    as.numeric(sf::st_area(difference))
  )

  # Topological equality
  expect_true(
    length(sf::st_equals(
      sf::st_union(res),
      sf::st_union(difference)
    )[[1]]) > 0
  )
})
