test_that("ua_generate_area() calculates corrected surfaces correctly", {

  # Create a simple example UA sf object
  ua <- Rsequoia2:::seq_poly
  ua$IDU = c("A", "A", "C")
  ua$SURF_CAD = c(10, 10, 20)
  ua <- seq_normalize(ua, "ua")

  res <- ua_generate_area(ua, verbose = FALSE)
  cor_area <- seq_field("cor_area")$name
  cad_area <- seq_field("cad_area")$name

  # Check that the new field exists
  expect_true(cor_area %in% names(res))

  # Check that total per IDU matches SURF_CA
  expect_equal(ave(res[[cor_area]], res[[cad_area]], FUN = sum), res[[cad_area]])

})

test_that("ua_generate_area() throw success message when verbose = TRUE", {

  # Create a simple example UA sf object
  ua <- Rsequoia2:::seq_poly
  ua$IDU = c("A", "A", "C")
  ua$SURF_CAD = c(10, 10, 20)
  ua <- seq_normalize(ua, "ua")

  expect_message(
    ua_generate_area(ua, verbose = TRUE),
    "Corrected cadastral areas calculated."
  )

})


test_that("ua_generate_ug() apply correct value correction", {

  # Create a simple example UA sf object
  ua <- Rsequoia2:::seq_poly
  ua$IDU = c("A", "A", "C")
  ua$SURF_CAD = c(10, 10, 20)
  ua <- seq_normalize(ua, "ua")

  expect_message(
    ua_generate_area(ua, verbose = TRUE),
    "Corrected cadastral areas calculated."
  )

})
