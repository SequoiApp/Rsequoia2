# test_that("ua_get_surfaces calculates corrected surfaces correctly", {
#   skip_if_not_installed("sf")
#   library(sf)
#
#   # Create a simple example UA sf object
#   ua <- st_sf(
#     IDU = c("A", "A", "C"),
#     SURF_CA = c(10.0000, 10.0000, 20.0000),
#     geometry = st_sfc(
#       st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
#       st_polygon(list(rbind(c(1,0), c(1,1), c(2,1), c(2,0), c(1,0)))),
#       st_polygon(list(rbind(c(0,1), c(0,2), c(1,2), c(1,1), c(0,1))))
#     )
#   ) |> seq_normalize("ua")
#
#   # Run function
#   ua_out <- ua_get_areas(ua, verbose = FALSE)
#
#   # Field names from seq_field()
#   surf_cor <- seq_field("surf_cor")$name
#
#   # Check that the new field exists
#   expect_true(surf_cor %in% names(ua_out))
#
#   # Check that total per IDU matches SURF_CA
#   expect_equal(
#     tapply(ua_out[[surf_cor]], ua_out$IDU, sum),
#     tapply(ua_out$SURF_CA, ua_out$IDU, sum)
#   )
#
#   # Check that corrected surfaces are numeric and non-negative
#   expect_true(all(ua_out[[surf_cor]] >= 0))
#   expect_type(ua_out[[surf_cor]], "double")
# })
