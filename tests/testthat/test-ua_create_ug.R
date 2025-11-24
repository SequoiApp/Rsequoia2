test_that("ua_create_ug creates UG field correctly with seq_field names", {
  skip_if_not_installed("sf")
  library(sf)

  # Simulate seq_field() behavior for the test
  seq_field <- function(key) {
    list(name = switch(key,
                       "parcelle" = "N_PARFOR",
                       "sous_parcelle" = "N_SSPARFOR",
                       "ug" = "PARFOR"))
  }

  # Sample sf object with correct column names
  ua <- st_sf(
    N_PARFOR =   c(1, 2, NA, "A"),
    N_SSPARFOR = c(1, NA, 3, "B"),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)),
                      st_point(c(2,2)), st_point(c(3,3)))
  )

  # Call the function
  ua_out <- ua_create_ug(ua, ug_keys = c("parcelle", "sous_parcelle"),
                         separator = ".", verbose = FALSE)

  ug_field <- seq_field("ug")$name

  # UG field exists
  expect_true(ug_field %in% names(ua_out))

  # Check that UG values are as expected
  expect_equal(ua_out[[ug_field]],
               c("01.01", "02.00", "00.03", "A.B"))
})
