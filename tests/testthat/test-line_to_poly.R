test_that("line_to_poly() aborts on invalid road type", {
  type_field <- seq_field("type")$name

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0,0,1,1), ncol=2, byrow=TRUE)),
      crs = 2154
    )
  )

  x[[type_field]] <- "BAD"

  expect_error(
    line_to_poly(x),
    regexp = "Invalid road type value"
  )
})

test_that("line_to_poly() works on minimal valid input with two output lines", {

  type_field <- seq_field("type")$name

  # minimal sf with two longer lines to survive the buffer/intersection
  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0,
                                 0, 5,
                                 1, 10), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(5, 0,
                                 5, 5,
                                 6, 10), ncol = 2, byrow = TRUE)),
      crs = 2154
    )
  ) |>
    seq_normalize("road_line")

  # add type column
  x[[type_field]] <- c("RN", "RC")

  # run the function
  out <- line_to_poly(x, dist = 5)

  # check results
  expect_s3_class(out, "sf")
  expect_true(type_field %in% names(out))
  expect_true(all(out[[type_field]] %in% c("RN", "RD", "RC", "RF")))

  # ensure we have at least two output lines
  expect_true(nrow(out) >= 2)
})
