test_that("seq_read() reads vector layers correctly", {
  tmp <- tempdir()

  m <- create_matrice(tmp, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  v_path <- file.path(tmp,"MY_TEST_PARCA_poly.geojson")

  sf::write_sf(v, v_path, append = FALSE)

  on.exit(unlink(c(v_path, m)))

  v_read <- seq_read("v.seq.parca.poly", tmp)

  expect_s3_class(v_read, "sf")
  expect_equal(nrow(v_read), nrow(v))
  expect_true("geometry" %in% names(v_read))

  expect_message(
    seq_read("v.seq.parca.poly", tmp, verbose = TRUE),
    "Loaded vector layer"
  )
})

test_that("seq_read() reads raster layers correctly", {
  tmp <- tempdir()

  m <- create_matrice(tmp, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  r_path <- file.path(tmp, "MY_TEST_IRC.tif")

  on.exit(unlink(c(r_path, m)))

  terra::writeRaster(r, r_path, overwrite = TRUE)

  r_read <- seq_read("r.irc", tmp)

  expect_s4_class(r_read, "SpatRaster")
  expect_equal(dim(r), dim(r_read))

  expect_message(
    seq_read("r.irc", tmp, verbose = TRUE),
    "Loaded raster layer"
  )

})

