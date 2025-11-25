test_that("ua_generate_ug() calculates corrected surfaces correctly", {

  # Create a simple example UA sf object
  ua <- sf::st_sf(
    IDU = c("A", "A", "C"),
    SURF_CA = c(10, 10, 20),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0), c(0,100), c(100,100), c(100,0), c(0,0)))),
      sf::st_polygon(list(rbind(c(0,0), c(0,200), c(200,200), c(200,0), c(0,0)))),
      sf::st_polygon(list(rbind(c(0,0), c(0,100), c(100,100), c(100,0), c(0,0))))
    ) |> sf::st_sfc(crs = 2154)
  ) |> seq_normalize("ua")

  res <- ua_generate_area(ua, verbose = FALSE)
  surf_cor <- seq_field("surf_cor")$name
  surf_cad <- seq_field("surf_cad")$name

  # Check that the new field exists
  expect_true(surf_cor %in% names(res))

  # Check that total per IDU matches SURF_CA
  expect_equal(ave(res[[surf_cor]], res[[surf_cad]], FUN = sum), res[[surf_cad]])

})

test_that("ua_generate_ug() throw success message when verbose = TRUE", {

  # Create a simple example UA sf object
  ua <- sf::st_sf(
    IDU = c("A", "A", "C"),
    SURF_CA = c(10, 10, 20),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0), c(0,100), c(100,100), c(100,0), c(0,0)))),
      sf::st_polygon(list(rbind(c(0,0), c(0,200), c(200,200), c(200,0), c(0,0)))),
      sf::st_polygon(list(rbind(c(0,0), c(0,100), c(100,100), c(100,0), c(0,0))))
    ) |> sf::st_sfc(crs = 2154)
  ) |> seq_normalize("ua")

  expect_message(
    ua_generate_area(ua, verbose = TRUE),
    "Corrected cadastral areas calculated."
  )

})
